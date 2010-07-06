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

  Copyright 2009, 2010 Etienne Millon <etienne.millon@eads.net>

 *)

(** @author Etienne Millon <etienne.millon@eads.net> *)

(**
 * Global options.
 *)

(**
 * A control is a 'tweak' meant to be get and set.
 *)
type 'a control

(** Set a tweak. *)
val set : 'a control -> 'a -> unit

(** Get a tweak. *)
val get : 'a control -> 'a

(** Process cfg only. *)
val cfg_only : bool control

(** Output more data. *)
val verbose  : bool control

(** Generate a DOT file. *)
val graphviz : bool control

(** Use widening. *)
val widening : bool control

(** Display solver output (abstract values). *)
val solver   : bool control

(** Getopt-style parser. *)
(* TODO use Arg instead. *)

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

