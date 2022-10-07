open Printf

type t = int
type label =
  | Special of string
  | Named of string
  | Merged of t list

let num = ref 0
let nametable = Hashtbl.create 30

let gen label: t =
  incr num;
  Hashtbl.add nametable !num label;
  !num

let gen_special name = gen (Special name)
let gen_named name = gen (Named name)
let gen_merged l =
  assert (l <> []);
  gen (Merged l)


module SMap = Map.Make(String)

let number_prefix = "%"
let rec list_to_string l =
  l
  |> List.fold_left
    (fun (sm: t list SMap.t) (id: t) ->
      let name = match Hashtbl.find nametable id with
        | Special s -> s
        | Named n -> n
        | Merged l -> list_to_string l (* 入れ子のmergedの出力は雑 *)
      in
      let l = SMap.find_opt name sm |> Option.value ~default:[] in
      SMap.add name (id :: l) sm
    )
    SMap.empty
  |> SMap.bindings
  |> List.map
    (fun (name, ids: string * t list): string ->
      match ids with
      | [id] -> sprintf "%s%s%d" name number_prefix id
      | _ ->
        let ids_s =
          List.sort Int.compare ids
          |> List.map string_of_int
          |> String.concat ","
        in
        sprintf "%s%s(%s)" name number_prefix ids_s
    )
  |> String.concat ", "
  |> sprintf "{%s}"
;;

let simple_name id =
  match Hashtbl.find nametable id with
  | Special s -> s
  | Named n -> n
  | Merged l -> list_to_string l (* mergedのsimple出力には未対応 *)
let numbered_name id =
  match Hashtbl.find nametable id with
  | Special s -> s
  | Named n -> sprintf "%s%s%d" n number_prefix id
  | Merged l -> list_to_string l

let to_int (t: t): int = t

let compare = Int.compare