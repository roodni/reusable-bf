type t

val gen_special: string -> t
val gen_named: string -> string option -> t
val gen_merged: t list -> t

val number_only_name: t -> string
val simple_name: t -> string
val numbered_name: t -> string
val detailed_name: t -> string

val to_int: t -> int

val compare: t -> t -> int