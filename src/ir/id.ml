open Printf
open Support.Pervasive

type t = int
type label =
  | Special of string
  | Named of string * string option
      (* 識別子名・所属 *)
  | Merged of t list

let to_int (t: t): int = t
let compare = Int.compare

let num = ref 0
let nametable = Hashtbl.create 100
let gen label : t =
  incr num;
  Hashtbl.add nametable !num label;
  !num

let gen_special name = gen (Special name)
let gen_named name pname = gen (Named (name, pname))
let gen_merged l =
    assert (l <> []);
    gen (Merged l)


let number_prefix = "%"

let number_only_name id = sprintf "%s%d" number_prefix id
let simple_name id =
  match Hashtbl.find nametable id with
  | Special s -> s
  | Named (n, _) -> n
  | Merged _ -> number_only_name id
let numbered_name id =
  match Hashtbl.find nametable id with
  | Special s -> s
  | Named (n, _) -> sprintf "%s%s%d" n number_prefix id
  | Merged _ -> number_only_name id


let name_with_pname n p =
  match p with
  | None -> n
  | Some p -> sprintf "%s$%s" p n

module SSet = Set.Make(String)
let rec names l =
  l
  |> List.fold_left
    (fun set id -> match Hashtbl.find nametable id with
      | Special s -> SSet.add s set
      | Named (n, p) -> SSet.add (name_with_pname n p) set
      | Merged l -> SSet.add_seq (names l) set
    )
    SSet.empty
  |> SSet.to_seq
let detailed_name id =
  match Hashtbl.find nametable id with
  | Special s -> s
  | Named (n, p) -> (name_with_pname n p) ^ (number_only_name id)
  | Merged l ->
      let names = names l |> List.of_seq |> String.concat ", " in
      sprintf "{%s}%s" names (number_only_name id)
