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
 * This datastructure is a mutable mapping from (string * int) couples
 * to arbitrary results (of type 'a).
 *)

type 'a t

(**
 * Constructor.
 * The list parameter is a list of strings and the associated number of cells.
 *)
val make : (string * int) list -> 'a -> 'a t

val size : 'a t -> string -> int

val get : 'a t -> string -> int -> 'a

val set : 'a t -> string -> int -> 'a -> unit

val fold : (string -> int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val map : ('a -> 'b) -> 'a t -> 'b t

val to_string : ('a -> string) -> 'a t -> string
