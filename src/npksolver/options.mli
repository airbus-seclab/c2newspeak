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

type 'a control

type 'a c_control

val set : 'a control -> 'a -> unit
val get : 'a control -> 'a

val cfg_only : bool control
val verbose  : bool control
val graphviz : bool control
val widening : bool control
val solver   : bool control

val set_cc : 'a c_control -> string -> unit
val get_cc : 'a c_control -> 'a

type opt_action =
  | Help
  | Set  of bool control
  | Call of (unit -> unit)
  | Carg of (string -> unit)

type opt = char       (* Short *)
         * string     (* Long  *)
         * string     (* Help string *)
         * opt_action

val parse_cmdline : opt list -> (string -> unit) -> string array -> unit

