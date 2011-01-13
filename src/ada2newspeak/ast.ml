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

type lval =
  | Var          of Symboltbl.scope * string * AdaTypes.t
  | ArrayAccess  of lval
                  * expression list
  | RecordAccess of lval
                  * int         (* offset *)
                  * AdaTypes.t (* Field type *)
  | PtrDeref     of lval
                  * AdaTypes.t
  | BlkLval       of block * lval

and expression = exp_value * AdaTypes.t

and exp_value =
  | CInt         of Newspeak.Nat.t
  | CFloat       of float
  | CBool        of bool
  | CChar        of int
  | Lval         of lval
  | Not          of expression
  | Binary       of binary_op
                  * expression
                  * expression
  | CondExp      of expression (** a ? b : c *)
                  * expression
                  * expression
  | AddressOf    of lval
                  * AdaTypes.t
  | FunctionCall of Symboltbl.scope * string
                  * argument list
                  * AdaTypes.t (* return type *)
  | Cast         of AdaTypes.t * AdaTypes.t * exp_value
  | BlkExp       of block * expression

and argument = AdaTypes.t * arg_mode

and arg_mode = 
  | In    of expression
  | Out   of lval
  | InOut of lval

and iteration_scheme =
  | NoScheme
  | While of expression

and object_state =
  | Variable
  | Constant
  | StaticVal of AdaTypes.data_t (*constante statique*)

and block = (instruction * Newspeak.location) list

and instruction =
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
  | ProcedureCall of Symboltbl.scope * string
                   * argument list
  | Case          of expression
                   * (expression * block) list
                   * block option
  | Block         of declarative_part
                   * block

and declarative_part = (declarative_item * Newspeak.location) list

and param = {
  formal_name   : string;
  param_type    : AdaTypes.t;
}

and body =
  | SubProgramBody of (sub_program_spec * block)
  | PackageBody    of string
                    * package_spec option
                    * declarative_part

and declarative_item =
  | BasicDecl of basic_declaration
  | BodyDecl  of body

and basic_declaration =
  | ObjectDecl      of string
                     * AdaTypes.t
                     * object_state
                     * block option
  | SpecDecl        of spec
  | NumberDecl      of string
                     * AdaTypes.data_t

and library_item =
  | Spec of spec
  | Body of body

and spec =
  | SubProgramSpec of sub_program_spec
  | PackageSpec    of package_spec

and sub_program_spec = {
  name: AdaSyntax.name;
  arguments: param list;
(* TODO: think about it, but isn't ret redundant with out argument? *)
  return_type: AdaTypes.t option
}

and package_spec = string
                 * (basic_declaration * Newspeak.location) list

type compilation_unit = (spec * Newspeak.location) list
                      * library_item
                      * Newspeak.location

(* TODO: think about it, but shouldn't the name be factored in all these types
   at the basic_declaration level? *)
let name_of_spec spec = 
  match spec with
    | ObjectDecl (i, _, _, _)
    | NumberDecl (i, _) -> i
(* TODO: look all the places where name_to_string is used => reduce its scope
   if possible *)
    | SpecDecl (SubProgramSpec spec) -> Ada_utils.name_to_string spec.name
    | SpecDecl (PackageSpec (n, _)) -> n
