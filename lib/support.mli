module Error : sig
  (** ソースコード中の位置 *)
  type info

  (** [create_info file line col] *)
  val create_info : string -> int -> int -> info
end