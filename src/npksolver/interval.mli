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

type 'a t =
  private
    | Interval of 'a * 'a
    | Empty

val top_int : int t

val bottom_int : int t

val from_bounds : 'a -> 'a -> 'a t

(** `with_size n` = `from_bounds 0 (n-1)` *)
val with_size : int -> int t

val meet : 'a t -> 'a t -> 'a t

val join : 'a t -> 'a t -> 'a t

val widen : int t -> int t -> int t

val (<=%) : 'a t -> 'a t -> bool

val plus : int t -> int t -> int t

val mult : int t -> int t -> int t

val neg : int t -> int t

val to_string : int t -> string
