type t

val gen_special: string -> t
val gen_named: string -> t
val gen_merged: t list -> t

val list_to_string: t list -> string

val simple_name: t -> string
val numbered_name: t -> string

val to_int: t -> int

val compare: t -> t -> int