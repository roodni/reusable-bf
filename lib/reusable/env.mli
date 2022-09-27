module Make(V: Map.OrderedType): sig
  type key = V.t
  type 'a t
  val empty : 'a t
  val extend : ?export:bool -> key -> 'a -> 'a t -> 'a t
  val lookup : key -> 'a t -> 'a option
  val export : 'a t -> 'a t

  (** [import src dest] *)
  val import : 'a t -> 'a t -> 'a t
end