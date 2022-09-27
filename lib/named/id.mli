type t

val gen: unit -> t
val gen_named: string -> t

val to_string: t -> string

val to_definition_order: t -> int