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
let gen_merged l = gen (Merged l)


let rec simple_name id =
  match Hashtbl.find nametable id with
  | Special s -> s
  | Named n -> n
  | Merged l ->
      let s = List.map simple_name l |> String.concat ", " in
      sprintf "{%s}" s
let rec numbered_name id =
  match Hashtbl.find nametable id with
  | Special s -> s
  | Named n -> sprintf "%s#%d" n id
  | Merged l ->
    let s = List.map numbered_name l |> String.concat ", " in
    sprintf "{%s}" s

let to_int (t: t): int = t

let compare = Int.compare