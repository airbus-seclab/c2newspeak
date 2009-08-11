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

open Newspeak

exception Unknown

type t

(* TODO: should abstract this type!! *)
type memloc = string

val universe: unit -> t

val emptyset: t

val join: t -> t -> t

val contains: t -> t -> bool

val addr_is_valid: t -> (memloc * int) -> bool

val prepare_call: t -> t -> (bool * t)

val apply: t -> t -> t

val set_pointsto: (memloc * int) -> memloc -> t -> t

val to_string: t -> string

val assign: (lval * exp * scalar_t) -> int -> t -> t

val lval_to_abaddr: int -> t -> lval -> (memloc * int option)

val abaddr_to_addr: (memloc * int option) -> (memloc * int)

val remove_local: int -> t -> t