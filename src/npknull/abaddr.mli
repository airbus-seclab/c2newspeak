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

type t

val singleton: Memloc.t -> t

val of_buffer: Dom.buffer -> t

val shift: int -> t -> t

val forget_offset: t -> t

(* raise Exceptions.Unknown if not singleton *)
val to_addr: t -> Dom.addr

val to_buffer: t -> Dom.buffer

val to_memloc: t -> Memloc.t

(* TODO: to have this primitive is a bit strange 
   should rather have a Ptr.addr_of!!!
*)
val to_exp: int -> t -> Dom.exp

val to_string: t -> string
