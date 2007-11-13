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

(* TODO: hide this, cleanup this implementation *)
type t = {
  fun_env: Csyntax.fundefs;
  global_env: Csyntax.glbdecls;
  local_env: (string, int * Csyntax.typ * Newspeak.location) Hashtbl.t;
  mutable vcnt: int
}

val create: (Csyntax.glbdecls * Csyntax.fundefs) -> t

val push: t -> string -> Csyntax.typ -> Newspeak.location -> unit
val pop: t -> string -> unit
val get_ret: t -> int
val get_var: t -> int -> Npkil.lval
val get_locals: t -> (Csyntax.typ * string * Newspeak.location) list

val get_ret_typ: t -> Csyntax.typ
val get_ret_name: unit -> string
val get_ret_lbl: unit -> Newspeak.lbl
val get_brk_lbl: unit -> Newspeak.lbl
val get_ftyp: t -> string -> Csyntax.ftyp
