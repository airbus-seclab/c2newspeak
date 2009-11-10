(*
  This file is part of npksolver, a solver for Newspeak,
  a minimal language framework well suited for static analysis.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *)

(** @author Etienne Millon <etienne.millon@eads.net> *)

type fp_algorithm =
  | Roundrobin
  | Worklist

val set_cfg_only : unit   -> unit val get_cfg_only : unit -> bool
val set_verbose  : unit   -> unit val get_verbose  : unit -> bool
val set_graphviz : unit   -> unit val get_graphviz : unit -> bool
val set_widening : unit   -> unit val get_widening : unit -> bool
val set_fp_algo  : string -> unit val get_fp_algo  : unit -> fp_algorithm
val set_solver   : unit   -> unit val get_solver   : unit -> bool

type opt_action =
  | Help
  | Call of (unit -> unit)
  | Carg of (string -> unit)

type opt = char       (* Short *)
         * string     (* Long  *)
         * string     (* Help string *)
         * opt_action

val parse_cmdline : opt list -> (string -> unit) -> string array -> unit

