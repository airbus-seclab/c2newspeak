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

type node = int

type nodeid = string * node (* function name, node *)

type stmt =
  | Nop
  | Set   of Prog.lval * Prog.exp * Newspeak.location
  | Guard of Prog.exp
  | Push  of int (* size *)
  | Pop
  | Init of (string, int) Pmap.t (* globals : name, size *)
  | Assert_true of Prog.exp
  | Call of nodeid

type func_t = int * (nodeid, (nodeid * stmt * Newspeak.location) list) Pmap.t

type t = (string, func_t) Pmap.t
