
type t

val compute: Newspeak.t -> t

val get_callers: t -> string -> string list

val get_position: t -> string -> string
