(** ソースコード中の位置 *)
type info
type 'a withinfo = { i : info; v : 'a }

val create_info : Lexing.position -> Lexing.position -> info
val unknown_info : info

val withinfo : info -> 'a -> 'a withinfo

val merge_info : info -> info -> info

(** [withinfo2 info_start info_end value] *)
val withinfo2 : info -> info -> 'a -> 'a withinfo

val output_info : Format.formatter -> info -> unit