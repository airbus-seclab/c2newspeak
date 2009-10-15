
type t =
  | Cfg_only
  | Verbose
  | Graphviz

val set : t -> unit -> unit
val get : t -> bool
