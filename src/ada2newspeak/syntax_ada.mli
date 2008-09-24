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

(*définition des types*)
type location = Newspeak.location
type nat = Newspeak.Nat.t
type flottant = float*string

type identifier = string
type name = identifier list*identifier

type param_mode = In | Out | InOut

type value = | IntVal of nat 
	     | FloatVal of flottant
	     | BoolVal of bool


type unary_op = UPlus | UMoins | Abs | Not
type binary_op = 
  | Plus | Moins | Fois | Div | Puissance 
  | Concat | Mod | Rem
  | Eq | Neq | Le | Lt | Ge | Gt
  | And | Or | Xor | AndThen | OrElse

type typ = 
  | Integer
  | IntegerConst 
  | Float
  | Boolean 
  | Character
  | Declared of typ_declaration*location
  | String

and typ_declaration =
  | Enum of identifier*((identifier*nat) list)*Newspeak.ikind
  | DerivedType of identifier*subtyp_indication
  | IntegerRange of identifier*contrainte*Newspeak.ikind option
  | Array of identifier*array_type_definition

and array_type_definition = 
    (* intervalle discret du tableau * type des éléments * taille du tableau*)
  | ConstrainedArray of subtyp_indication*subtyp_indication*int option

and subtyp =
  | Unconstrained of typ
  (* le paramêtre booléen indique si le sous-type est static *)
  | Constrained of typ*contrainte*bool
  | SubtypName of name

and expression = 

  | NullExpr 
  | CInt of nat
  | CFloat of flottant
  | CBool of bool
  | CChar of int
  | CString of string
  | Var of name
  | FunctionCall of name*expression list
  | Unary of unary_op*expression
  | Binary of binary_op*expression*expression
  | Qualified of subtyp*expression

and contrainte = 
  | RangeConstraint of expression*expression
  | IntegerRangeConstraint of Newspeak.bounds
  | FloatRangeConstraint of flottant*flottant

and subtyp_indication = subtyp*contrainte option*subtyp option

type param = {pnom:identifier list;mode:param_mode;ptype:subtyp;pdef:expression option}

type iteration_scheme = NoScheme | While of expression

type instruction_atom = 
  | NullInstr
  | Affect of name*expression
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

type representation_clause = EnumerationRepresentation of identifier*array_aggregate

type use_clause = name list

type object_state = 
  | Variable
  | Constant (*constante dynamique, ou non encore évaluée*)
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
