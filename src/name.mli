
type t

val make : string -> t option

val original : t -> string

val snake_case : t -> string

val upper_camel_case : t -> string

val pp : Format.formatter -> t -> unit

val compare : t -> t -> int

val bool   : t
val int    : t
val string : t
val list   : t
val option : t
