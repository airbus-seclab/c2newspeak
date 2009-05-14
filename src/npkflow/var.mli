(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2009  Charles Hymans
 
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

type t

val main_tainted: string

val compare: t -> t -> int

val of_global: string -> t

val of_local: int -> t

val to_string: t -> string

val to_fid: t -> string

type var = t

module Set:
sig
  type t

  val empty: t 
  val singleton: var -> t
  val is_empty: t -> bool
  val subset: t -> t -> bool
  val remove: var -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val iter: (var -> unit) -> t -> unit
  val elements: t -> var list
  val to_string: t -> string
  val mem: var -> t -> bool
end
