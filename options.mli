
type t =
  | Cfg_only
  | Verbose
  | Graphviz

type opt_action =
  | Help
  | Set  of t
  | Call of (unit -> unit)
  | Carg of (string -> unit)

type opt = char       (* Short *)
         * string     (* Long  *)
         * string     (* Help string *)
         * opt_action

val parse_cmdline : opt list -> (string -> unit) -> string array -> unit

val get : t -> bool
