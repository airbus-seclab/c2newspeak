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

(** This is a cross product of several Range.t *)

type t

val bottom : t

val singleton : string -> Range.t -> t

val join : t -> t -> t

val meet : t -> t -> t

val widen : t -> t -> t

val guard : string -> (Range.t -> Range.t) -> t -> t

val set_var : string -> Range.t -> t -> t

val get_var : string -> t -> Range.t

val to_string : t -> string

val yaml_dump : t -> string
