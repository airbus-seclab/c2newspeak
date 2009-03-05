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

(** This module defines types for the abstract syntax tree. *)

(** Location in the source code. *)
type location = Newspeak.location


type nat = Newspeak.Nat.t

(** A floating-point number *)
type float_number = float*string

type identifier = string

(* TODO doc : overload ? *)
type name = identifier list*identifier

(** Modes for subprogram parameters. *)
type param_mode =
| In    (** Read-only  *)
| Out   (** Write-only *)
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
| Plus          (** "+",        addition                                 *)
| Minus         (** "-",        difference                               *)
| Mult          (** "*",        multiplication                           *)
| Div           (** "/",        division                                 *)
| Power         (** "**",       exponentiation                           *)
| Concat        (** "&",        concatenation                            *)
| Mod           (** "mod",      modulus                                  *)
| Rem           (** "rem",      division reminder                        *)
| Eq            (** "=",        equality test                            *)
| Neq           (** "/=",       difference test                          *)
| Le            (** "<=",       less or equal                            *)
| Lt            (** "<",        less than                                *)
| Ge            (** ">=",       greater or equal                         *)
| Gt            (** ">",        greater than                             *)
| And           (** "and",      logical conjunction                      *)
| Or            (** "or",       logical disjunction                      *)
| Xor           (** "xor",      logical exclusive disjunction            *)
| AndThen       (** "and then", logical conjunction with lazy evaluation *)
| OrElse        (** "or else",  logical disjunction with lazy evaluation *)

(** Builtin types. *)
type typ =
  | Integer                              (** Integer               *)
  | IntegerConst                         (** Integer constant      *)
  | Float                                (** Floating-point number *)
  | Boolean                              (** Boolean               *)
  | Character                            (** Character             *)
  | String                               (** String                *)
  | Declared of typ_declaration*location (** User-defined type     *)

(** Type declaration. *)
and typ_declaration =
  | Enum          of   identifier
                      *((identifier*nat) list)
                      *Newspeak.ikind             (** Enumerated type, eg
                                                [type Day is (Mon,...,Sun)] *)
  | DerivedType  of identifier
                   *subtyp_indication  (** Derived type, eg
                                [type WeekEndDay is Day range Sat..Sun] *)
  | IntegerRange of identifier*contrainte*Newspeak.ikind option
    (** Integer range, eg [type Byte is range 0..255] *)
  | Array  of identifier*array_type_definition   (* TODO *)
  | Record of identifier*record_type_definition (* TODO *)

(** Record type definition, as in {[
type Date is
    record
        Day   : Integer range 1 .. 31;
        Month : Month_Name;
        Year  : Integer;
    end record;
]} *)
and record_type_definition = field list

(** A record field. *)
and field = identifier list*subtyp_indication*expression option
 (* | FielDecl of identifier list*subtyp_indication*expression option *)
    (*object_state  TO DO check need for object_st *)
(* TODO +mutable *)

(** Array type definition *)
and array_type_definition =
  | ConstrainedArray of subtyp_indication*subtyp_indication*int option
    (** Constrained array : discrete interval, type of elements, array size *)

(** Subtype definition. *)
and subtyp =
  | Unconstrained of typ               (** Unconstrained. *)
  | Constrained of typ*contrainte*bool (** Constrained. The boolean parameters
                                           means if the subtype is static *)
  | SubtypName of name                 (** Subtype *)

(** Attribute, eg Day'First *)
and attribute_reference = subtyp * attribute_designator

(** Attribute designators like "First", "Range" or "Digits" *)
and attribute_designator =
 | AttributeDesignator of identifier * expression option
   (** The option is for the optional parameter. It shall be static. *)

(** Expressions. *)
and expression =
  | NullExpr                             (** The [null] expression    *)
  | CInt of nat                          (** Integer constant         *)
  | CFloat of float_number               (** Floating-point constant  *)
  | CBool of bool                        (** Boolean constant
                                             ([True] or [False])      *)
  | CChar of int                         (** Character constant       *)
  | CString of string                    (** String constant          *)
  | Var of name                          (** Variable name            *)
  | FunctionCall of name*expression list (** Function call            *)
  | Unary of unary_op*expression         (** A unary operator applied
                                             to another expression    *)
  | Binary of binary_op*expression*expression (** A binary operator
                                           applied to two expressions *)
  | Qualified of subtyp*expression       (** Qualified expression     *)
  | Attribute of attribute_reference     (** Attribute reference      *)

(** Constraint *)
and contrainte =
  | RangeConstraint of expression*expression
  | IntegerRangeConstraint of Newspeak.bounds
  | FloatRangeConstraint of float_number*float_number

and subtyp_indication = subtyp*contrainte option*subtyp option

(** Left-value *)
type lval =
| Lval of name                   (** Named lvalue *)
| ArrayAccess of lval*expression (** Array access *)

(** Subprogram parameter *)
type param = {
        formal_name   : identifier list;   (** Formal name *)
        mode          : param_mode;        (** Mode (In, Out, or InOut) *)
        param_type    : subtyp;            (** Type *)
        default_value : expression option; (** Default value (optional) *)
}

(** The way a loop iterates over values:            *)
(** loop                        ->  NoScheme        *)
(** for I in reverse 1..5 loop  ->  For I,1,5,true  *)
(** for I in 15..10             ->  While false     *)
(** for I in reverse 15..10     ->  While false     *)
(** while exp                   ->  While exp       *)
(** for I in 4..8               ->  For I,5,8,false *)
type iteration_scheme =
  | NoScheme                       (* Forever *)
  | While of expression            (* While [expression] evaluates to [true] *)
  | For of name * expression * expression * bool           (* In an interval *)

(** An instruction *)
type instruction_atom =
  | NullInstr                 (** The null instruction (do nothing)    *)
  | Assign of lval*expression (** Assignment                           *)
  | Return of expression      (** Return from function                 *)
  | ReturnSimple              (** Return from procedure                *)
  | If of expression*instruction list*instruction list (** Conditional *)
  | Loop of iteration_scheme*(instruction list) (** Loops              *)
  | Exit of expression option                   (** Loop exit          *)
  | ProcedureCall of name*expression list       (** Procedure call     *)
(* TODO : change ProcedureCall to be able to evaluate any expression *)

(** An instruction with its location *)
and instruction = instruction_atom*location

(** Subprogram declaration *)
type sub_program_spec =
  | Function of name*param list*subtyp (** A Function returns a value *)
  | Procedure of name*(param list)     (** A Procedure does not       *)

(* the identifier is the one that choose the element :
   there are other possibilities for this choice, not yet implemented *)
type array_aggregate = NamedArrayAggregate of (identifier * expression) list

type representation_clause =
  | EnumerationRepresentation of identifier*array_aggregate
type use_clause = name list

type object_state =
  | Variable
  | Constant (*constante dynamique, ou non encore evaluee*)
  | StaticVal of value (*constante statique*)

type context_clause =
  | With of name*location*(spec*location) option
  | UseContext of use_clause

and context = context_clause list

and sub_program_body = sub_program_spec*declarative_part*instruction list

and package_spec = name*(basic_declaration*location) list

and package_body = name*package_spec option*declarative_part*instruction list

and spec =
  | SubProgramSpec of sub_program_spec
  | PackageSpec of package_spec

and body =
  | SubProgramBody of sub_program_body
  | PackageBody of package_body

and basic_declaration =
  | ObjectDecl of identifier list*subtyp_indication
      *expression option*object_state
  | TypeDecl of typ_declaration
  | UseDecl of use_clause
  | SpecDecl of spec
  | NumberDecl of identifier list*expression*value option
  | SubtypDecl of identifier*subtyp_indication
  | RepresentClause of representation_clause

and declarative_item =
  | BasicDecl of basic_declaration
  | BodyDecl of body

and declarative_part = (declarative_item*location) list

type library_item =
  | Spec of spec
  | Body of body

type compilation_unit = context*library_item*location

type programme = compilation_unit list
