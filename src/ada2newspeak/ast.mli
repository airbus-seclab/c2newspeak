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

type unary_op =
| UPlus
| UMinus
| Abs
| Not

type binary_op =
| Plus
| Minus
| Mult
| Div
| Mod
| Rem
| Eq
| Gt
| And
| Or
| Xor
| AndThen
| OrElse
| Power

type block = (instruction * Newspeak.location) list

and instruction =
  | NullInstr
  | Assign        of lval
                   * expression
  | Return        of expression
  | ReturnSimple
  | If            of expression
                   * block       (* then *)
                   * block       (* else *)
  | Loop          of iteration_scheme
                   * block
  | Exit
  | ProcedureCall of Syntax_ada.name
                   * argument list
  | Case          of expression
                   * (expression*block) list
                   * block option
  | Block         of declarative_part
                   * block

and lval =
  | Lval        of Syntax_ada.name
  | ArrayAccess of lval
                 * expression

and iteration_scheme =
  | NoScheme
  | While of expression
  | For   of Syntax_ada.identifier
           * expression
           * expression
           * bool

and argument = Syntax_ada.identifier option*expression

and expression = exp_value * Ada_types.t

and exp_value =
  | CInt         of Newspeak.Nat.t
  | CFloat       of Syntax_ada.float_number
  | CBool        of bool
  | CChar        of int
  | Var          of Syntax_ada.name
  | FunctionCall of Syntax_ada.name
                  * argument list
  | Unary        of unary_op
                  * expression
  | Binary       of binary_op
                  * expression
                  * expression
  | Qualified    of Syntax_ada.subtyp
                  * expression
  | Attribute    of Syntax_ada.attribute_reference

and  declarative_part = Ada_types.table
                     * (declarative_item*Newspeak.location) list

and param = {
        formal_name   : Syntax_ada.identifier;
        mode          : Syntax_ada.param_mode;
        param_type    : Syntax_ada.subtyp;
        default_value : expression option;
}

and  body =
  | SubProgramBody of sub_program_spec
                    * declarative_part
                    * block
  |    PackageBody of Syntax_ada.name
                    * package_spec option
                    * declarative_part
                    * block

and  declarative_item =
  | BasicDecl of basic_declaration
  |  BodyDecl of body

and basic_declaration =
  | ObjectDecl      of Syntax_ada.identifier list
                     * Syntax_ada.subtyp_indication
                     * expression option
                     * Syntax_ada.object_state
  | TypeDecl        of Syntax_ada.identifier*Syntax_ada.typ_declaration
  | UseDecl         of Syntax_ada.name
  | SpecDecl        of spec
  | NumberDecl      of Syntax_ada.identifier
                     * expression
                     * Syntax_ada.value option
  | SubtypDecl      of Syntax_ada.identifier
                     * Syntax_ada.subtyp_indication
  | RepresentClause of Syntax_ada.representation_clause

and  library_item =
  | Spec of spec
  | Body of body

and spec =
  | SubProgramSpec of sub_program_spec
  |    PackageSpec of package_spec

and context_clause =
  | With       of Syntax_ada.name
                * Newspeak.location
                * (spec*Newspeak.location) option
  | UseContext of Syntax_ada.name

and sub_program_spec =
  | Function  of Syntax_ada.name*param list*Syntax_ada.subtyp
  | Procedure of Syntax_ada.name*param list

and package_spec = Syntax_ada.name
                 * (basic_declaration*Newspeak.location) list

type compilation_unit = context_clause list
                      * library_item
                      * Newspeak.location

type programme = compilation_unit list
