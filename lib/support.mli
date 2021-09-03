module Error : sig
  (** ソースコード中の位置 *)
  type info

  (** [create_info file line col] *)
  val create_info : string -> int -> int -> info
  val unknown_info : info

  (** [output_info out_channel info] *)
  val output_info : out_channel -> info -> unit

  (** 位置情報とエラーメッセージを出力し、終了コード1でexitする *)
  val error_at : info -> string -> 'a
end