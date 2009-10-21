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

type t

val top : t

val bottom : t

val from_bounds : Newspeak.Nat.t -> Newspeak.Nat.t -> t

(* a C b *)
val (<=%) : t -> t -> bool

(* a \/ b *)
val join : t -> t -> t

(* a /\ b *)
val meet : t -> t -> t

val widen : t -> t -> t

val shift : Newspeak.Nat.t -> t -> t

val add_bound : ?min:Newspeak.Nat.t -> ?max:Newspeak.Nat.t -> t -> t

val to_string : t -> string
