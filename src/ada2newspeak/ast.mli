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

  Jasmine Duchon
  email : jasmine . duchon AT free . fr

  Etienne Millon
  email : etienne.millon AT gmail.com

*)

type block = (instruction * Newspeak.location) list

and  instruction =
  | NullInstr
  | Assign        of Syntax_ada.lval
                   * Syntax_ada.expression
  | Return        of Syntax_ada.expression
  | ReturnSimple
  | If            of Syntax_ada.expression
                   * block       (* then *)
                   * block       (* else *)
  | Loop          of Syntax_ada.iteration_scheme
                   * block
  | Exit
  | ProcedureCall of Syntax_ada.name
                   * Syntax_ada.argument list
  | Case          of Syntax_ada.expression
                   * (Syntax_ada.expression*block) list
                   * block option
  | Block         of declarative_part
                   * block

and  declarative_part = Ada_types.table
                     * (declarative_item*Newspeak.location) list

and  body =
  | SubProgramBody of Syntax_ada.sub_program_spec
                    * declarative_part
                    * block
  |    PackageBody of Syntax_ada.name
                    * Syntax_ada.package_spec option
                    * declarative_part
                    * block

and  declarative_item =
  | BasicDecl of Syntax_ada.basic_declaration
  |  BodyDecl of body

and  library_item =
  | Spec of Syntax_ada.spec
  | Body of body

type compilation_unit = Syntax_ada.context_clause list
                      * library_item
                      * Newspeak.location

type programme = compilation_unit list
