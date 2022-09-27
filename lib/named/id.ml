open Printf

type t = int

let num = ref 0
let nametable = Hashtbl.create 30

let gen (): t =
  incr num;
  !num
let gen_named (name: string): t =
  let id = gen () in
  Hashtbl.add nametable id name;
  id

let to_string (t: t) =
  match Hashtbl.find_opt nametable t with
  | None -> sprintf "#%d" t
  | Some name -> name

let to_definition_order (t: t): int = t