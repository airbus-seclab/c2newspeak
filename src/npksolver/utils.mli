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

val filter_map : ('a -> 'b option) -> 'a list -> 'b list

val may : ('a -> 'b) -> 'a option -> 'b option

val with_default : 'a -> 'a option -> 'a

module Lift : sig
  type 'a lift = 'a option

  val bind : ('a -> 'b lift) -> 'a lift -> 'b lift

  val return : 'a -> 'a lift

  val maybe : 'b -> ('a -> 'b) -> 'a lift -> 'b

  val bind2 : ('a -> 'b -> 'c lift) -> 'a lift -> 'b lift -> 'c lift

end
