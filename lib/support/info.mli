(** ソースコード中の位置 *)
type info
type 'a withinfo = { i : info; v : 'a }

val create_info : Lexing.position -> Lexing.position -> info
val set_pname_of_info : info -> string option -> unit
val get_pname_of_info : info -> string option

val withinfo : info -> 'a -> 'a withinfo
val clearinfo : 'a withinfo -> 'a

val merge_info : info -> info -> info

(** [withinfo2 info_start info_end value] *)
val withinfo2 : info -> info -> 'a -> 'a withinfo

val output_info : Format.formatter -> info -> unit
val lines_of_info : info -> int * int


type trace

val empty_trace : trace
val push_tailcall : trace -> trace
val push_info : info -> trace -> trace

val output_trace : Format.formatter -> trace -> unit

val top_of_trace : trace -> info
val lengths_of_trace : trace -> int list