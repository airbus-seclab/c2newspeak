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

(** TODO rename *)
type flottant = float*string

type identifier = string

(** TODO doc : overload ? *)
type name = identifier list*identifier

(** Modes for subprogram parameters. *)
type param_mode =
| In    (** Read-only  *)
| Out   (** Write-only *)
| InOut (** Read-write *)


type value =
| IntVal   of nat       (** Integer value        *)
| FloatVal of flottant  (** Floating-point value *)
| BoolVal  of bool      (** Boolean value        *)

(** Unary operators. *)
type unary_op =
| UPlus     (** Unary "+"        *)
| UMoins    (** Unary "-"        *)
| Abs       (** Absolute value   *)
| Not       (** Logical negation *)

(** Binary operators. TODO rename *)
type binary_op =
| Plus          (** "+",        addition                                 *)
| Moins         (** "-",        difference                               *)
| Fois          (** "*",        multiplication                           *)
| Div           (** "/",        division                                 *)
| Puissance     (** "**",       exponentiation                           *)
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
    (** Integer range, eg [type Byte is range 0..255] *) (*TODO ikind ? *)
  | Array of identifier*array_type_definition   (** TODO *)
  | Record of identifier*record_type_definition (** TODO *)

(** Record type definition, as in {[.
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
  | CFloat of flottant                   (** Floating-point constant  *)
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


and contrainte =
  | RangeConstraint of expression*expression
  | IntegerRangeConstraint of Newspeak.bounds
  | FloatRangeConstraint of flottant*flottant

and subtyp_indication = subtyp*contrainte option*subtyp option

type lval = Lval of name | ArrayAccess of lval*expression

type param = {
        pnom:identifier list;
        mode:param_mode;
        ptype:subtyp;
        pdef:expression option
}

type iteration_scheme =
  | NoScheme
  | While of expression
  | For of name * expression * expression * bool
(* loop                        ->  NoScheme        *)
(* for I in reverse 1..5 loop  ->  For I,1,5,true  *)
(* for I in 15..10             ->  While false     *)
(* for I in reverse 15..10     ->  While false     *)
(* while exp                   ->  While exp       *)
(* for I in 4..8               ->  For I,5,8,false *)

type instruction_atom =
  | NullInstr
  | Affect of lval*expression
  | Return of expression
  | ReturnSimple
  | If of expression*instruction list*instruction list
  | Loop of iteration_scheme*(instruction list)
  | Exit of expression option
  | ProcedureCall of name*expression list

and instruction = instruction_atom*location

type sub_program_spec =
  | Function of name*param list*subtyp
  | Procedure of name*(param list)

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
