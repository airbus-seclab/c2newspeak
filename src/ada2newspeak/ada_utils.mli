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

  Jasmine Duchon
  email : jasmine . duchon AT free . fr
  
*)


exception NonStaticExpression

val log2_sup : Big_int.big_int -> int
val puiss : Syntax_ada.nat -> Syntax_ada.nat -> Syntax_ada.nat
val mod_ada : Syntax_ada.nat -> Syntax_ada.nat -> Syntax_ada.nat
val rem_ada : Syntax_ada.nat -> Syntax_ada.nat -> Syntax_ada.nat
val xor : bool -> bool -> bool
val nat_of_bool : bool -> Newspeak.Nat.t

val inf_val : Syntax_ada.value -> Syntax_ada.value -> bool
val eq_val : Syntax_ada.value -> Syntax_ada.value -> bool

val make_enum :
  Syntax_ada.identifier ->
  Syntax_ada.identifier list -> Syntax_ada.typ_declaration

val between : 'a -> 'a -> 'a -> bool
val constraint_is_constraint_compatible : Syntax_ada.contrainte ->
  Syntax_ada.contrainte -> bool
val value_is_static_constraint_compatible : 
  Syntax_ada.contrainte -> Syntax_ada.value -> bool

val check_static_subtyp: 
  Syntax_ada.subtyp -> Syntax_ada.value -> unit
val constraint_is_static: Syntax_ada.contrainte -> bool

val ikind_of_range : Syntax_ada.nat -> Syntax_ada.nat -> Newspeak.ikind
val make_range :
  Syntax_ada.identifier ->
  Syntax_ada.expression ->
  Syntax_ada.expression -> Syntax_ada.typ_declaration
val check_typ : 
  Syntax_ada.typ option -> Syntax_ada.typ -> Syntax_ada.typ

val base_typ : Syntax_ada.subtyp -> Syntax_ada.typ

val extract_subtyp : 
  Syntax_ada.subtyp_indication -> Syntax_ada.subtyp

val extract_typ : Syntax_ada.subtyp_indication -> Syntax_ada.typ

val eq_base_typ :
  Syntax_ada.subtyp -> Syntax_ada.subtyp -> bool

val known_compatible_typ :
  Syntax_ada.typ option -> Syntax_ada.typ -> bool

val integer_class : Syntax_ada.typ -> bool

val typ_operand : 
  Syntax_ada.binary_op -> 
  Syntax_ada.typ option -> Syntax_ada.typ option

val check_operand_typ :
  Syntax_ada.binary_op -> 
  Syntax_ada.typ -> unit

val check_compil_unit_name : 
  Syntax_ada.compilation_unit -> string -> bool
