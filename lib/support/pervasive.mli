val (--) : int -> int -> int list

module String : sig
  include module type of String
  val repeat : string -> int -> string
end

module Hashtbl : sig
  include module type of Hashtbl
  val find_default : ('a, 'b) t -> 'a -> 'b -> 'b
  val add_assign_int : ('a, int) t -> 'a -> int -> unit
end

module List : sig
  include module type of List
end

val (@) : 'a list -> 'a list -> 'a list