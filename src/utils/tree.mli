(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans, Olivier Levillain

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

type 'a t
val create : unit -> 'a t
val push   : 'a -> 'a t -> unit
val pop    : 'a t -> 'a
val top    : 'a t -> 'a
val lookup : ('a -> 'b option) -> 'a t -> 'b option
val iter   : ('a -> unit) -> 'a t -> unit
val fold   : ('res -> 'a -> 'res) -> 'res -> 'a t -> 'res
val height : 'a t -> int
val nth    : 'a t -> int -> 'a
