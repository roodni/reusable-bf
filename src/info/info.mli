(*
  ソースコードの位置情報を格納するもの
  info というアレな名前は TaPL の参考実装由来
  https://github.com/mspertus/TAPL/blob/1ed01066b8ecf5575d38379e4006b2610e1e461a/arith/lib/support.mli#L30
*)

(** ソースコード中の位置 *)
type info
type 'a withinfo = { i : info; v : 'a }

val create_info : Lexing.position -> Lexing.position -> info
val create_info_only_filename : string -> info

val withinfo : info -> 'a -> 'a withinfo
val clearinfo : 'a withinfo -> 'a

val merge_info : info -> info -> info

(** [withinfo2 info_start info_end value] *)
val withinfo2 : info -> info -> 'a -> 'a withinfo

val output_info : Format.formatter -> info -> unit
val lines_of_info : info -> int * int

val set_pname_of_info : info -> string option -> unit
val get_pname_of_info : info -> string option

module I : sig
  (* 全部の関数をopen前提で書くのがつらくなってきた *)
  val is_parened : info -> bool
  val enparen : info -> info
end

module Withinfo : sig
  val enparen : 'a withinfo -> 'a withinfo
end


type trace

val empty_trace : trace
val push_tailcall : trace -> trace
val push_info : info -> trace -> trace

val output_trace : Format.formatter -> trace -> unit

val top_of_trace : trace -> info
val lengths_of_trace : trace -> int list