
val set_cfg_only : unit -> unit
val get_cfg_only : unit -> bool
val set_verbose  : unit -> unit
val get_verbose  : unit -> bool
val set_graphviz : unit -> unit
val get_graphviz : unit -> bool
val set_widening : unit -> unit
val get_widening : unit -> bool

type opt_action =
  | Help
  | Call of (unit -> unit)
  | Carg of (string -> unit)

type opt = char       (* Short *)
         * string     (* Long  *)
         * string     (* Help string *)
         * opt_action

val parse_cmdline : opt list -> (string -> unit) -> string array -> unit

