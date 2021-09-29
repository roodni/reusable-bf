module Error : sig
  (** ソースコード中の位置 *)
  type info
  type 'a withinfo = { i : info; v : 'a }

  val create_info : Lexing.position -> Lexing.position -> info
  val unknown_info : info

  val withinfo : info -> 'a -> 'a withinfo

  val merge_info : info -> info -> info

  (** [withinfo2 info_start info_end value] *)
  val withinfo2 : info -> info -> 'a -> 'a withinfo

  (** [output_info out_channel info] *)
  val output_info : out_channel -> info -> unit

  (** 位置情報とエラーメッセージを出力し、終了コード1でexitする *)
  val error_at : info -> string -> 'a
end