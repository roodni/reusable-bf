open Support.Pervasive
open Info

exception ExecutionError of string
exception ExecutionExit of trace * string

module IdMap = Map.Make(Id)
module IntHash = struct
  type t = int
  let equal = Int.equal
  let hash i = i land max_int
end
module IntTable = Hashtbl.Make(IntHash)

module Tape = struct
  type t = value IdMap.t
  and value =
    | Cell of { mutable v: int }
    | Array of array_param
  and array_param = {
    length: int option;
    indexes: int ref IdMap.t;
    values: t IntTable.t;
    ftype: Field.t;
  }


  let initial_indexes field =
    Field.fold
      (fun id mty tape -> match mty with
        | Field.Index -> IdMap.add id (ref 0) tape
        | Cell _ | Array _ -> tape
      )
      field IdMap.empty

  let from_field field : t =
    Field.fold
      (fun id mty tape ->
        let value =
          match mty with
          | Field.Cell _ ->
              Cell {v=0} |> Option.some
          | Array { length; members } ->
              Array {
                length = Some length;
                indexes = initial_indexes members;
                values = IntTable.create length;
                ftype = members;
              } |> Option.some
          | Index -> None
        in
        match value with
        | None -> tape
        | Some value -> IdMap.add id value tape
      )
      field IdMap.empty

  let from_field_main Field.{ finite; unlimited } =
    from_field finite
    |> IdMap.add
      Field.uarray_id
      (Array {
        length = None;
        indexes = initial_indexes unlimited;
        values = IntTable.create 100;
        ftype = unlimited
      })


  let validate_actual_index ap i =
    let length = Option.value ap.length ~default:max_int in
    if i < 0 || length <= i then
      raise @@ ExecutionError "Index out of range"

  let get_array_elm ap i =
    try IntTable.find ap.values i with
    | Not_found ->
        let a = from_field ap.ftype in
        IntTable.add ap.values i a;
        a

  let rec select tape = function
    | Sel.Member id -> IdMap.find id tape
    | Array { name; index_opt; offset; member } ->
        let ap = select_array tape (Sel.Member name) in
        let i = match index_opt with
          | Some idx_id ->
              let idx = IdMap.find idx_id ap.indexes in
              !idx + offset
          | None -> offset
        in
        validate_actual_index ap i;
        select (get_array_elm ap i) member
  and select_array tape sel =
    match select tape sel with
    | Cell _ -> assert false
    | Array ap -> ap

  let modify_cell_value ~cell_type v =
    match cell_type with
    | Bf.Exe.OCamlInt -> v
    | WrapAround256 -> v land 255
    | Overflow256 ->
        if 0 <= v && v < 256 then v
        else raise @@ ExecutionError "Overflow"

  let get_cell tape sel =
    match select tape sel with
    | Cell {v} -> v
    | Array _ -> assert false
  let set_cell tape sel v ~cell_type =
    match select tape sel with
    | Cell c ->
        c.v <- modify_cell_value ~cell_type v
    | Array _ -> assert false
  let add_cell tape n sel ~cell_type =
    match select tape sel with
    | Cell c ->
        c.v <- modify_cell_value ~cell_type (c.v + n)
    | Array _ -> assert false

  let get_index tape (arr_sel, idx_id: Sel.index) =
    let ap = select_array tape arr_sel in
    !(IdMap.find idx_id ap.indexes)

  let shift_index tape n (arr_sel, idx_id: Sel.index) followers =
    let ap = select_array tape arr_sel in
    let idx = IdMap.find idx_id ap.indexes in
    let i = !idx in
    let i' = i + n in
    validate_actual_index ap i';
    let elm = get_array_elm ap i in
    let elm' = get_array_elm ap i' in
    List.iter
      (fun follower_id ->
        let s = Sel.Member follower_id in
        match select elm s, select elm' s with
        | Cell c, Cell c' ->
            let v = c.v in
            c.v <- 0;
            c'.v <- v;
        | _ -> assert false
      )
      followers;
    idx := i';
end

let run ~printer ~input ~cell_type field ir_code =
  let tape = Tape.from_field_main field in
  let rec loop ir_code =
    List.iter
      (fun Code.{ cmd; trace; _ } ->
        try
          match cmd with
          | Add (n, s) ->
              Tape.add_cell ~cell_type tape n s
          | Put s ->
              let v = Tape.get_cell tape s land 255 in
              printer (char_of_int v)
          | Get s ->
              let v = match input () with
                | Some c -> int_of_char c
                | None -> raise @@ ExecutionError "End of input"
              in
              Tape.set_cell ~cell_type tape s v
          | Shift { n; index; followers } ->
              Tape.shift_index tape n index followers
          | Loop (s, child) ->
              while Tape.get_cell tape s <> 0 do
                loop child
              done
          | If (s, the, els) ->
              if Tape.get_cell tape s <> 0
                then loop the
                else loop els
          | IndexLoop (idx, child) ->
              while Tape.get_index tape idx <> 0 do
                loop child
              done
          | IndexIf (idx, the) ->
              if Tape.get_index tape idx <> 0 then
                loop the;
          | Reset s ->
              Tape.set_cell ~cell_type tape s 0
          | Use _ -> ()
        with ExecutionError msg -> raise @@ ExecutionExit (trace, msg)
      )
      ir_code
  in
  match loop ir_code with
  | () -> Ok ()
  | exception ExecutionExit (trace, msg) -> Error (trace, msg)

let print_error ?(ppf=Format.err_formatter) (trace, msg) =
  let open Format in
  fprintf ppf "@[<v>";
  output_trace ppf trace;
  fprintf ppf "Execution error: %s" msg;
  pp_print_newline ppf ();
;;

let run_stdio ~cell_type field code =
  run
    ~printer:(fun c -> print_char c; flush stdout)
    ~input:(fun () -> In_channel.input_char stdin)
    ~cell_type
    field code

let run_string ~input ~cell_type field code =
  let buf = Buffer.create 100 in
  let res =
    run
      ~printer:(Buffer.add_char buf)
      ~input:(String.to_seq input |> Seq.to_dispenser)
      ~cell_type
      field code
  in
  (res, Buffer.contents buf)