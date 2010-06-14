(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans
  
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

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org
*)

open Lowspeak

type t

(** [universe] the store with no variable. *)
val universe: t

val join: t -> t -> t

val contains: t -> t -> bool

(* TODO: the int is the environment, maybe should remove it *)
val assign: (lval * exp * Newspeak.scalar_t) -> int -> t -> t

val copy: (lval * lval) -> int -> t -> t

val guard: Dom.addr -> t -> t

val remove_memloc: Memloc.t -> t -> t

val addr_is_valid: t -> Dom.addr -> bool

val to_string: t -> string

val build_transport: t -> Memloc.t list -> t -> Subst.t

val transport: Subst.t -> t -> t

val compose: t -> Memloc.t list -> t -> t

val split: Memloc.t list -> t -> (t * t)

val glue: t -> t -> t

(* TODO: this primitive is not well chosen, think about it
   raises Exceptions.Emptyset when is nil of uninitialized *)
val read_addr: t -> Dom.addr -> Dom.abptr

(* TODO: this primitive name is not well chosen, think about it *)
val read_fun: int -> t -> Lowspeak.lval -> string list

(* TODO: not good this primitive, think about removing it!! *)
val set_pointsto: Dom.addr -> Memloc.t -> t -> t

val normalize: Memloc.t list -> t -> Subst.t

val may_be_null: int -> t -> exp -> bool
