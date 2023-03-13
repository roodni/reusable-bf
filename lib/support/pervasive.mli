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

(** とても長くなりうるリスト (内部表現はlistと同じ)
    非末尾再帰の操作を提供しない
  *)
type 'a llist
val llist : 'a list -> 'a llist
val (~~) : 'a list -> 'a llist
val lcons : 'a -> 'a llist -> 'a llist
val lnil : 'a llist
val (@+) : 'a llist -> 'a llist -> 'a llist

module LList : sig
  type 'a t = 'a llist

  val to_list_danger : 'a t -> 'a list

  val length : 'a t -> int
  val rev : 'a t -> 'a t
  val concat_map : ('a -> 'b t) -> 'a t -> 'b t
  val concat : 'a t t -> 'a t

  val iter : ('a -> unit) -> 'a t -> unit
  val iteri : (int -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val fold_left_map : ('a -> 'b -> 'a * 'c) ->'a -> 'b t -> 'a * 'c t
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  val find_map : ('a -> 'b option) -> 'a t -> 'b option
  val filter : ('a -> bool) -> 'a t -> 'a t

  val assoc : 'a -> ('a * 'b) t -> 'b
  val assoc_opt : 'a -> ('a * 'b) t -> 'b option

  val sort : ('a -> 'a -> int) -> 'a t -> 'a t

  val to_seq : 'a t ->'a Seq.t
  val of_seq : 'a Seq.t -> 'a t
end