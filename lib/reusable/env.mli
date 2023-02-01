module Make(V: Map.OrderedType): sig
  type key = V.t
  type 'a t
  val empty : 'a t
  val extend : key -> 'a -> 'a t -> 'a t
  val lookup : key -> 'a t -> 'a option

  (** [merge src dest] *)
  val merge : 'a t -> 'a t -> 'a t
end