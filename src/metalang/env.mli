module Make(V: Map.OrderedType): sig
  type key = V.t
  type 'a t
  val empty : 'a t
  val extend : key -> 'a -> 'a t -> 'a t
  val extend_ref : key -> 'a ref -> 'a t -> 'a t
  val lookup : key -> 'a t -> 'a option
  val to_seq : 'a t -> (key * 'a) Seq.t

  (** [merge src dest] *)
  val merge : 'a t -> 'a t -> 'a t
end