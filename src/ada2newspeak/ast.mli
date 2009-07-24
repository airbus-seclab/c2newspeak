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
| Power

type block = (instruction * Newspeak.location) list

and instruction =
  | Assign        of lval
                   * expression
                   * bool (* If true, ro-checks will not be performed *
                           * Useful for example for i++ in loops or   *
                           * constant initializers                    *)
  | Return        of expression
  | ReturnSimple
  | If            of expression
                   * block       (* then *)
                   * block       (* else *)
  | Loop          of iteration_scheme
                   * block
  | Exit
  | ProcedureCall of Symboltbl.scope * string
                   * argument list
  | Case          of expression
                   * (expression*block) list
                   * block option
  | Block         of declarative_part
                   * Symboltbl.context
                   * block

and lval =
  | Lval        of Symboltbl.scope * string * Ada_types.t
  | ArrayAccess of lval
                 * expression

and iteration_scheme =
  | NoScheme
  | While of expression

and argument = string option*expression

and expression = exp_value * Ada_types.t

and exp_value =
  | CInt         of Newspeak.Nat.t
  | CFloat       of float
  | CBool        of bool
  | CChar        of int
  | Var          of Symboltbl.scope * string * Ada_types.t
  | FunctionCall of Symboltbl.scope * string
                  * argument list
  | Not          of expression
  | Binary       of binary_op
                  * expression
                  * expression
  | CondExp      of expression (** a ? b : c *)
                  * expression
                  * expression

and  declarative_part = (declarative_item*Newspeak.location) list

and param = {
        formal_name   : string;
        mode          : Syntax_ada.param_mode;
        param_type    : Ada_types.t;
        default_value : expression option;
}

and  body =
  | SubProgramBody of sub_program_spec
                    * declarative_part
                    * Symboltbl.context (* parameters *)
                    * Symboltbl.context (* locals     *)
                    * block
  |    PackageBody of string
                    * package_spec option
                    * Symboltbl.context
                    * declarative_part

and  declarative_item =
  | BasicDecl of basic_declaration
  |  BodyDecl of body

and object_state =
  | Variable
  | Constant
  | StaticVal of Ada_types.data_t (*constante statique*)

and basic_declaration =
  | ObjectDecl      of string
                     * Ada_types.t
                     * object_state
  | TypeDecl        of string*Syntax_ada.typ_declaration*Ada_types.t
  | UseDecl         of string
  | SpecDecl        of spec
  | NumberDecl      of string
                     * Ada_types.data_t

and  library_item =
  | Spec of spec
  | Body of body

and spec =
  | SubProgramSpec of sub_program_spec
  |    PackageSpec of package_spec

and context_clause =
  | With       of string
                * Newspeak.location
                * (spec*Newspeak.location) option
  | UseContext of string

and sub_program_spec =
  | Function  of Syntax_ada.name*param list*Ada_types.t
  | Procedure of Syntax_ada.name*param list

and package_spec = string
                 * (basic_declaration*Newspeak.location) list
                 * Symboltbl.context
                 * (string * expression) list (* initializers *)

type compilation_unit = context_clause list
                      * library_item
                      * Newspeak.location

type programme = compilation_unit list
