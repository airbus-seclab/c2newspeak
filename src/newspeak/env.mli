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

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org

  Olivier Levillain
  email: olivier.levillain@penjili.org
*)

type t

val create: Csyntax.glbdecls -> t
val extract_prog: t -> string -> Npkil.t

val push: t -> string -> Csyntax.typ -> Newspeak.location -> unit
val pop: t -> string -> unit
val get_var: t -> string -> (Npkil.lval * Csyntax.typ)
val get_locals: t -> (Csyntax.typ * string * Newspeak.location) list
val add_global: t -> string -> Npkil.ginfo -> unit

val get_ret_typ: t -> Csyntax.typ
val get_ret_name: unit -> string
val get_ret_lbl: unit -> Newspeak.lbl
val get_brk_lbl: unit -> Newspeak.lbl

val update_funbody: t -> string -> Npkil.blk -> unit

val update_ftyp: t -> string -> Npkil.ftyp -> unit
