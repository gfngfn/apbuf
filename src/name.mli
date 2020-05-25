
type t

val from_snake_case : string -> t option

val from_upper_camel_case : string -> t option

val original : t -> string

val to_snake_case : t -> string

val to_lower_camel_case : t -> string

val to_upper_camel_case : t -> string

val pp : Format.formatter -> t -> unit

val compare : t -> t -> int

val bool   : t
val int    : t
val string : t
val list   : t
val option : t
