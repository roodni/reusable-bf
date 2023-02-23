open Support.Pervasive
open Support.Info

exception ExecutionError of string
exception ExecutionExit of info * string

module Tape = struct
  type t = (Id.t, value) Hashtbl.t
  and value =
    | Cell of { mutable v: int }
    | Array of array_param
  and array_param = {
    length: int option;
    indexes: (Id.t, int) Hashtbl.t;
    values: (int, t) Hashtbl.t;
    ftype: Field.t;
  }


  let initial_indexes field =
    let tbl = Hashtbl.create 5 in
    Field.fold
      (fun id mty () -> match mty with
        | Field.Index -> Hashtbl.add tbl id 0
        | Cell _ | Array _ -> ()
      )
      field ();
    tbl

  let from_field field : t =
    let tbl = Hashtbl.create 30 in
    Field.fold
      (fun id mty () ->
        let value =
          match mty with
          | Field.Cell _ ->
              Cell {v=0} |> Option.some
          | Array { length; members } ->
              Array {
                length = Some length;
                indexes = initial_indexes members;
                values = Hashtbl.create length;
                ftype = members;
              } |> Option.some
          | Index -> None
        in
        match value with
        | None -> ()
        | Some value -> Hashtbl.add tbl id value
      )
      field ();
    tbl

  let from_field_main Field.{ finite; unlimited } =
    let tbl = from_field finite in
    Hashtbl.add tbl Field.uarray_id
      (Array {
        length = None;
        indexes = initial_indexes unlimited;
        values = Hashtbl.create 100;
        ftype = unlimited
      });
    tbl

  let validate_actual_index ap i =
    let length = Option.value ap.length ~default:max_int in
    if i < 0 || length <= i then
      raise @@ ExecutionError "Index out of range"

  let get_array_elm ap i =
    try Hashtbl.find ap.values i with
    | Not_found ->
        let a = from_field ap.ftype in
        Hashtbl.add ap.values i a;
        a

  let rec select tape = function
    | Sel.Member id -> Hashtbl.find tape id
    | Array { name; index_opt; offset; member } ->
        let ap = select_array tape (Sel.Member name) in
        let i = match index_opt with
          | Some idx_id -> Hashtbl.find ap.indexes idx_id + offset
          | None -> offset
        in
        validate_actual_index ap i;
        select (get_array_elm ap i) member
  and select_array tape sel =
    match select tape sel with
    | Cell _ -> assert false
    | Array ap -> ap

  let get_cell tape sel =
    match select tape sel with
    | Cell {v} -> v
    | Array _ -> assert false
  let set_cell tape sel v ~cell_type =
    match select tape sel with
    | Cell c ->
        let v = match cell_type with
          | Bf.Exe.OCamlInt -> v
          | WrapAround256 -> v land 255
          | Overflow256 ->
              if 0 <= v && v < 256 then v
              else raise @@ ExecutionError "Overflow"
        in
        c.v <- v
    | Array _ -> assert false

  let get_index tape (arr_sel, idx_id: Sel.index) =
    let ap = select_array tape arr_sel in
    Hashtbl.find ap.indexes idx_id

  let shift_index tape n (arr_sel, idx_id: Sel.index) followers =
    let ap = select_array tape arr_sel in
    let i = Hashtbl.find ap.indexes idx_id in
    let i' = i + n in
    validate_actual_index ap i';
    let elm = get_array_elm ap i in
    let elm' = get_array_elm ap i' in
    LList.iter
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
    Hashtbl.replace ap.indexes idx_id i'
end

let run ~printer ~input ~cell_type field ir_code =
  let tape = Tape.from_field_main field in
  let rec loop ir_code =
    LList.iter
      (fun Code.{ cmd; info; _ } ->
        try
          match cmd with
          | Add (n, s) ->
              let v = Tape.get_cell tape s in
              Tape.set_cell ~cell_type tape s (v + n)
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
        with ExecutionError msg -> raise @@ ExecutionExit (info, msg)
      )
      ir_code
  in
  match loop ir_code with
  | () -> Ok ()
  | exception ExecutionExit (info, msg) -> Error (info, msg)

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