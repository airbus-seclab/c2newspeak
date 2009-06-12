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

  Etienne Millon
  email: etienne.millon AT gmail . com

  Jasmine Duchon
  email : jasmine . duchon AT free . fr

*)

type constant_symb =
  | Number       of Syntax_ada.value*bool      (** bool = global? *)
  | StaticConst  of Syntax_ada.value*Syntax_ada.typ*bool  (** bool = global? *)
  | EnumLitteral of Syntax_ada.typ*Syntax_ada.nat*bool    (** bool = global? *)
  | VarSymb      of bool            (** bool = global? *)
  | FunSymb      of Syntax_ada.typ option*bool (** bool = extern? *)

val eval_static :  Ast.expression -> Syntax_ada.typ option
                -> (Syntax_ada.name,constant_symb) Hashtbl.t
                -> Syntax_ada.package list
                -> Ada_utils.package_manager
                -> bool
  -> Syntax_ada.value*Syntax_ada.typ


val eval_static_integer_exp :  Ast.expression
                            -> (Syntax_ada.name, constant_symb) Hashtbl.t
                            -> Syntax_ada.package list
                            -> Ada_utils.package_manager
                            -> bool
    -> Newspeak.Nat.t

val eval_static_number  :  Ast.expression
                        -> (Syntax_ada.name, constant_symb) Hashtbl.t
                        -> Syntax_ada.package list
                        -> Ada_utils.package_manager
                        -> bool
    -> Syntax_ada.value
