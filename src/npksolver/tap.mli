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

(* Producer for the test anything protocol. *)
(* Not reentrant. *)

val test_plan : int -> unit

val test_end : unit -> unit

val test_ok : string -> unit

val assert_equal : ?cmp:('a -> 'a -> bool)
                -> ?printer:('a -> string)
                -> 'a
                -> 'a
                -> string
              -> unit

val assert_equal_int : int -> int -> string -> unit

val assert_true  : bool -> string -> unit

val assert_false : bool -> string -> unit

val assert_equal_string : string -> string -> string -> unit

val assert_exn : ('a -> 'b) -> exn -> 'a -> string -> unit
