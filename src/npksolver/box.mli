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

(** This is a cross product of several 'a *)

type 'a t

val top : 'a Domain.c_dom -> 'a t

val bottom : 'a t

val singleton : 'a Domain.c_dom -> Prog.lval -> 'a -> 'a t

val join : 'a Domain.c_dom -> 'a t -> 'a t -> 'a t

val meet : 'a Domain.c_dom -> 'a t -> 'a t -> 'a t

val widen : 'a Domain.c_dom -> 'a t -> 'a t -> 'a t

val guard : Prog.lval -> ('a -> 'a) -> 'a t -> 'a  t

val set_var : 'a Domain.c_dom -> Prog.lval -> 'a -> 'a t -> 'a t

val environment : 'a Domain.c_dom -> 'a t -> (Prog.lval -> 'a)

val push : 'a Domain.c_dom -> 'a t -> 'a t

val pop  : 'a Domain.c_dom -> 'a t -> 'a t

val to_string : 'a Domain.c_dom -> 'a t -> string

val yaml_dump : 'a Domain.c_dom -> 'a t -> string

val equal : 'a t -> 'a t -> bool
