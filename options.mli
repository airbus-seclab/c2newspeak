
type t =
  | Cfg_only
  | Verbose

val set : t -> unit -> unit
val get : t -> bool
