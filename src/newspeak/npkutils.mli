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


(** Npkutils regroups simple translation functions *)


module Int_set :
  sig
    type elt = int
    type t
    val empty : t
    val mem : elt -> t -> bool
    val add : elt -> t -> t
  end

(* TODO: should this name, dangerous *)
val incr : int ref -> int

(** The following functions handle binop translations *)

val translate_arith_binop : Cil.binop -> Newspeak.binop
val translate_float_binop : Newspeak.size_t -> Cil.binop -> Newspeak.binop
val translate_logical_binop : (Newspeak.sign_t * Newspeak.size_t) -> Cil.binop -> Newspeak.binop
val translate_rel_binop : Cil.typ -> Cil.typ -> Cil.binop -> Newspeak.binop


val translate_ikind : Cil.ikind -> Newspeak.ikind

(** [translate_typ cil_typ] returns the translation of [cil_typ] in
    Newspeak, if the transformation is possible *)
val translate_typ : Cil.typ -> Npkil.typ

val translate_ret_typ : Cil.typ -> Npkil.typ option

val isPtr : Cil.exp -> bool
