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

(** This module defines types for the parse tree. *)

(**
 * A qualified string.
 * For example, "Ada.Text_IO.Put_line" maps to
 * [ ["Ada","Text_IO"],"Put_Line"].
 *)
type name = string option * string

(** Modes for subprogram parameters. *)
type param_mode =
| In    (** Read-only  *)
|   Out (** Write-only *)
| InOut (** Read-write *)

(** Unary operators. *)
type unary_op =
| UPlus     (** Unary "+"        *)
| UMinus    (** Unary "-"        *)
| Abs       (** Absolute value   *)
| Not       (** Logical negation *)

(** Binary operators. *)
type binary_op =
| Plus          (** "+",        addition                              *)
| Minus         (** "-",        difference                            *)
| Mult          (** "*",        multiplication                        *)
| Div           (** "/",        division                              *)
| Power         (** "**",       exponentiation                        *)
| Mod           (** "mod",      modulus                               *)
| Rem           (** "rem",      division reminder                     *)
| Eq            (** "=",        equality test                         *)
| Neq           (** "/=",       difference test                       *)
| Le            (** "<=",       less or equal                         *)
| Lt            (** "<",        less than                             *)
| Ge            (** ">=",       greater or equal                      *)
| Gt            (** ">",        greater than                          *)
| And           (** "and",      logical conjunction                   *)
| Or            (** "or",       logical disjunction                   *)
| Xor           (** "xor",      logical exclusive disjunction         *)
| AndThen       (** "and then", logical conjunction (w/short-circuit) *)
| OrElse        (** "or else",  logical disjunction (w/short-circuit) *)

(** Type declaration. *)
type typ_declaration =
  | Enum         of (string*Newspeak.Nat.t) list
  | DerivedType  of subtyp_indication
  | IntegerRange of expression*expression
  | Record       of (string*subtyp) list
  | Array        of subtyp_indication list (* Indices   *)
                  * subtyp_indication      (* Component *)

and subtyp = name

(** Expressions. *)
and expression =
  | CInt         of Newspeak.Nat.t
  | CFloat       of float
  | CBool        of bool
  | CChar        of int
  | SelectedName of name
  | FunctionCall of name
                  * argument list
  | Unary        of unary_op
                  * expression
  | Binary       of binary_op
                  * expression
                  * expression
  | Qualified    of name
                  * expression
  | Attribute    of name * string * expression option

(** Constraint *)
and contrainte =
  | IntegerRangeConstraint of Newspeak.bounds
  |   FloatRangeConstraint of float
                            * float

and subtyp_indication = subtyp
                      * (expression*expression) option

(** Left-value *)
and lval =
| SelectedLval of name
| ArrayAccess  of lval
                * expression

(** Subprogram parameter *)
and param = {
        formal_name   : string;            (** Formal name              *)
        mode          : param_mode;        (** Mode (In, Out, or InOut) *)
        param_type    : subtyp;            (** Type                     *)
        default_value : expression option; (** Default value (optional) *)
}

(**
 * The way a loop iterates over values :
 *  loop                        ->  NoScheme
 *  for I in reverse 1..5 loop  ->  For I,1,5,true
 *  for I in 15..10             ->  While false
 *  for I in reverse 15..10     ->  While false
 *  while exp                   ->  While exp
 *  for I in 4..8               ->  For I,5,8,false
 *)
and iteration_scheme =
  | NoScheme                       (* Forever *)
  | While of expression            (* While [expression] evaluates to [true] *)
  | For   of string
           * for_loop_range
           * bool

and for_loop_range =
  | DirectRange of expression * expression
  | ArrayRange  of name

and block = (instruction * Newspeak.location) list

(** Effective argument for a function or procedure call.
    The optional string is the formal name in case of
    a named argument ("name => value") *)
and argument = string option*expression

(** An instruction *)
and instruction =
  | NullInstr                    (** The null instruction (do nothing)    *)
  | Assign        of lval
                   * expression
  | Return        of expression  (** Return from function                 *)
  | ReturnSimple                 (** Return from procedure                *)
  | If            of expression
                   * block       (* then *)
                   * block       (* else *)
  | Loop          of iteration_scheme
                   * block
  | Exit
  | ProcedureCall of name
                   * argument list
  | Case          of expression
                   * (expression*block) list
                   * block option
  | Block         of declarative_part
                   * block                (** "declare" block *)

(** Subprogram declaration *)
and sub_program_spec =
  | Function  of string*param list*subtyp (** A Function returns a value *)
  | Procedure of string*param list        (** A Procedure does not       *)

(* the string is the one that choose the element :
   there are other possibilities for this choice, not yet implemented *)
and array_aggregate = NamedArrayAggregate of (string * expression) list

and object_state =
  | Variable
  | Constant

and context_clause =
  | With       of string
                * (spec*Newspeak.location) option
  | UseContext of string

and package_spec = string
                 * (basic_declaration*Newspeak.location) list

and spec =
  | SubProgramSpec of sub_program_spec
  |    PackageSpec of package_spec

and body =
  | SubProgramBody of sub_program_spec
                    * declarative_part
                    * block
  |    PackageBody of string
                    * package_spec option
                    * declarative_part

and basic_declaration =
  | ObjectDecl      of string list
                     * subtyp_indication
                     * expression option
                     * object_state
  | TypeDecl        of string*typ_declaration
  | UseDecl         of string
  | SpecDecl        of spec
  | NumberDecl      of string
                     * expression
  | SubtypDecl      of string
                     * subtyp_indication
  | RepresentClause of string * array_aggregate
  | RenamingDecl    of string (* new name *)
                     * name   (* old name *)

and declarative_item =
  | BasicDecl of basic_declaration
  |  BodyDecl of body

and declarative_part = (declarative_item*Newspeak.location) list

type library_item =
  | Spec of spec
  | Body of body

type compilation_unit = context_clause list
                      * library_item
                      * Newspeak.location

type programme = compilation_unit list
