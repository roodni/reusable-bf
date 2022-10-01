open Printf

type t = int
type name =
  | Special of string
  | Named of string

let num = ref 0
let nametable = Hashtbl.create 30

let gen (): t =
  incr num;
  !num

let gen_special name =
  let id = gen () in
  Hashtbl.add nametable id (Special name);
  id
let gen_named name =
  let id = gen () in
  Hashtbl.add nametable id (Named name);
  id

let simple_name id =
  match Hashtbl.find nametable id with
  | Special s -> sprintf "##%s" s
  | Named n -> n
let numbered_name id =
  match Hashtbl.find nametable id with
  | Special s -> sprintf "##%s" s
  | Named n -> sprintf "%s#%d" n id

let to_definition_order (t: t): int = t

let compare = Int.compare