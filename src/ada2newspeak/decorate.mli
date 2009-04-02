(*
  Ada2Newspeak: compiles Ada code into Newspeak. Newspeak is a minimal language
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
  email : etienne.millon AT gmail.com

*)

(**
 * AST decoration.
 *
 * This module is used to give the AST more useful information
 * about semantic analysis.
 *)

val d_basic_decl : Syntax_ada.basic_declaration
                -> Syntax_ada.basic_declaration

val d_package_spec : Syntax_ada.package_spec
                  -> Syntax_ada.package_spec

val d_package_body : Syntax_ada.package_body
                  -> Syntax_ada.package_body

val d_subprogram_spec : Syntax_ada.sub_program_spec
                     -> Syntax_ada.sub_program_spec

val d_subprogram_body : Syntax_ada.sub_program_body
                     -> Syntax_ada.sub_program_body

val d_library : Syntax_ada.library_item
             -> Syntax_ada.library_item

val decorate : Syntax_ada.compilation_unit
            -> Syntax_ada.compilation_unit
