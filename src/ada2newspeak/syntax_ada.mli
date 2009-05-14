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

(** This module defines types for the abstract syntax tree. *)

(** Location in the source code. *)
type location = Newspeak.location

type nat = Newspeak.Nat.t

(** A floating-point number *)
type float_number = float*string

(* FIXME : for speed's sake, the list should be reverted. *)
type package = string list

(**
 * A qualified string.
 * For example, "Ada.Text_IO.Put_line" maps to
 * [ ["Ada","Text_IO"],"Put_Line"].
 *)
type name = package * string

(** Modes for subprogram parameters. *)
type param_mode =
| In    (** Read-only  *)
|   Out (** Write-only *)
| InOut (** Read-write *)

type value =
| IntVal   of nat           (** Integer value        *)
| FloatVal of float_number  (** Floating-point value *)
| BoolVal  of bool          (** Boolean value        *)

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

(** Builtin types. *)
type typ =
  | Integer                                         (** Integer               *)
  | IntegerConst                                    (** Integer constant      *)
  | Float                                           (** Floating-point number *)
  | Boolean                                         (** Boolean               *)
  | Character                                       (** Character             *)
  | String                                          (** String                *)
  | Declared of string*typ_declaration*location (** User-defined type     *)

(** Type declaration. *)
and typ_declaration =
  | Enum          of ((string*nat) list)
                    * Newspeak.ikind
  | DerivedType  of subtyp_indication
  | IntegerRange of contrainte
                  * Newspeak.ikind option
  | Array  of array_type_definition
  | Record of field list

(** A record field. *)
and field = string list
          * subtyp_indication
          * expression option
    (* TODO + object_state ? *)

(** Array type definition *)
and array_type_definition =
  | ConstrainedArray of subtyp_indication*subtyp_indication*int option
    (** Constrained array : discrete interval, type of elements, array size *)

(** Subtype definition. *)
and subtyp =
  | Unconstrained of typ               (** Unconstrained. *)
  | Constrained of typ*contrainte*bool (** Constrained. The boolean parameters
                                           means the subtype is static *)
  | SubtypName of name                 (** Subtype *)

(** Attribute, eg Day'First *)
and attribute_reference = subtyp * attribute_designator

(** Attribute designators like "First", "Range" or "Digits" *)
and attribute_designator =
 | AttributeDesignator of string * expression option
   (** The option is for the optional parameter. It shall be static. *)

(** Expressions. *)
and expression =
  | CInt         of nat
  | CFloat       of float_number
  | CBool        of bool
  | CChar        of int
  | Var          of name
  | FunctionCall of name
                  * argument list
  | Unary        of unary_op
                  * expression
  | Binary       of binary_op
                  * expression
                  * expression
  | Qualified    of subtyp
                  * expression
  | Attribute    of attribute_reference

(** Constraint *)
and contrainte =
  |        RangeConstraint of expression
                            * expression
  | IntegerRangeConstraint of Newspeak.bounds
  |   FloatRangeConstraint of float_number
                            * float_number

and subtyp_indication = subtyp
                      * contrainte option
                      * subtyp     option

(** Left-value *)
and lval =
| Lval        of name
| ArrayAccess of lval
               * expression

(** Subprogram parameter *)
and param = {
        formal_name   : string;        (** Formal name              *)
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
           * expression
           * expression
           * bool

and block = (instruction * location) list

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
  | Function  of name*param list*subtyp (** A Function returns a value *)
  | Procedure of name*param list        (** A Procedure does not       *)

(* the string is the one that choose the element :
   there are other possibilities for this choice, not yet implemented *)
and array_aggregate = NamedArrayAggregate of (string * expression) list

and representation_clause =
  | EnumerationRepresentation of string
                               * array_aggregate
  | AttributeDefinitionClause of subtyp
                               * string
                               * expression

and object_state =
  | Variable
  | Constant (*constante dynamique, ou non encore evaluee*)
  | StaticVal of value (*constante statique*)

and context_clause =
  | With       of name
                * location
                * (spec*location) option
  | UseContext of name

and package_spec = name
                 * (basic_declaration*location) list

and spec =
  | SubProgramSpec of sub_program_spec
  |    PackageSpec of package_spec

and body =
  | SubProgramBody of sub_program_spec
                    * declarative_part
                    * block
  |    PackageBody of name
                    * package_spec option
                    * declarative_part
                    * block

and basic_declaration =
  | ObjectDecl      of string list
                     * subtyp_indication
                     * expression option
                     * object_state
  | TypeDecl        of string*typ_declaration
  | UseDecl         of name
  | SpecDecl        of spec
  | NumberDecl      of string
                     * expression
                     * value option
  | SubtypDecl      of string
                     * subtyp_indication
  | RepresentClause of representation_clause

and declarative_item =
  | BasicDecl of basic_declaration
  |  BodyDecl of body

and declarative_part = (declarative_item*location) list

type library_item =
  | Spec of spec
  | Body of body

type compilation_unit = context_clause list
                      * library_item
                      * location

type programme = compilation_unit list
