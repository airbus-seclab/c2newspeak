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


open Syntax_ada
open Big_int
module Nat = Newspeak.Nat

exception NonStaticExpression
exception AmbiguousTypeException

type verbose_level =
  | Silent
  | Debug
  | Warning
  | Error

(** Generic error. *)
let mkerror lev modulename = match lev with
  | Silent  -> (fun _ -> ())
  | Debug   -> Npkcontext.print_debug
  | Warning -> Npkcontext.report_warning modulename
  | Error   -> Npkcontext.report_error   modulename

(* arithmetique*)

(* fonction propre a Ada *)
let puiss a b =
  let a = Nat.to_big_int a
  and b = Nat.to_big_int b in
    if (Big_int.sign_big_int b)<0
    then begin
      Npkcontext.report_error "Ada_utils.puiss"
        "integer exponent negative"
    end else Nat.of_big_int (Big_int.power_big_int_positive_big_int a b)

let mod_rem_aux ~is_mod na nb =
  let a = Nat.to_big_int na in
  let b = Nat.to_big_int nb in
  let r_mod =  mod_big_int a b in
  Nat.of_big_int (if (sign_big_int (if is_mod then b else a)) > 0
                  then
                    r_mod
                  else
                    sub_big_int r_mod (abs_big_int b)
                 )

let rem_ada = mod_rem_aux ~is_mod:false
let mod_ada = mod_rem_aux ~is_mod:true

let eq_val v1 v2 =
    match (v1, v2) with
      | IntVal   v1, IntVal   v2 -> v1 = v2
      | BoolVal  v1, BoolVal  v2 -> v1 = v2
      | FloatVal v1, FloatVal v2 -> v1 = v2
      | _ ->
          Npkcontext.report_error "Ada_utils.eq_val"
            "internal error : uncompatible value"

let inf_val v1 v2 =
  let inf a b = a<b in
    match (v1, v2) with
      |   IntVal v1,   IntVal v2 -> (Nat.compare v1 v2) < 0
      |  BoolVal v1,  BoolVal v2 -> inf v1 v2
      | FloatVal v1, FloatVal v2 -> inf v1 v2
      | _ ->
          Npkcontext.report_error "Ada_utils.inf_val"
            "internal error : uncompatible value"

let nat_of_bool b =
  if b then Newspeak.Nat.one
       else Newspeak.Nat.zero

let between a b n  =
  a <= n && n <= b

let between_nat a b n =
  (Nat.compare n a)>=0 && (Nat.compare b n)>=0

let numeric_constraint_compatibility ref1 ref2 fils1 fils2 =
  if ref2<ref1
  then fils2<fils1
  else
    (between ref1 ref2 fils1) && (between ref1 ref2 fils2)

let nat_constraint_compatibility ref1 ref2 fils1 fils2 =
  if (Nat.compare ref2 ref1) < 0
  then (Nat.compare fils2 fils1) < 0
  else
    (between_nat ref1 ref2 fils1) && (between_nat ref1 ref2 fils2)

(* verifie que la contrainte courante est compatible avec cref *)
let constraint_check_compatibility cref courante =
  if not (
  match (cref, courante) with
    | (IntegerRangeConstraint(ref1, ref2),
       IntegerRangeConstraint(cour1, cour2)) ->
        nat_constraint_compatibility ref1 ref2 cour1 cour2
    | (FloatRangeConstraint(ref1, ref2),
       FloatRangeConstraint(cour1, cour2)) ->
        numeric_constraint_compatibility ref1 ref2 cour1 cour2
    | ((FloatRangeConstraint _| IntegerRangeConstraint _),
           RangeConstraint _)
    | (RangeConstraint _, RangeConstraint _)
    | (RangeConstraint _,
       (FloatRangeConstraint _| IntegerRangeConstraint _)) -> true
    | (IntegerRangeConstraint _, FloatRangeConstraint _)
    | (FloatRangeConstraint _, IntegerRangeConstraint _) ->
        Npkcontext.report_error "Ada_utils.check_constraint"
          "internal error : uncompatible constraints"
  ) then Npkcontext.report_error
         "Constraint_is_compatible"
         "constraint error : uncompatible constraint"

(* verifie que la valeur value respecte bien la contrainte.
   la contrainte est supposee statique, et le type de value
   compatible avec cette contrainte. Ce qui implique :
   - soit on a une valeur, et la contrainte est numerique
     et de meme type
   - soit on a pas de valeur *)
let value_is_static_constraint_compatible contrainte value =
  match (value,contrainte) with
(*    | (EnumVal(n), IntegerRangeConstraint(inf,sup)) ->
        between_nat inf sup (Newspeak.Nat.of_int n)*)
    | (IntVal(n), IntegerRangeConstraint(inf,sup)) ->
        between_nat inf sup n
    | (BoolVal(b), IntegerRangeConstraint(inf,sup)) ->
        between_nat inf sup (nat_of_bool b)
    | (FloatVal(n), FloatRangeConstraint(inf,sup)) ->
        between inf sup n
    | ((BoolVal _|IntVal _), FloatRangeConstraint _)
    | (FloatVal _, IntegerRangeConstraint _) ->
        Npkcontext.report_error "Ada_utils.check_static_constraint"
          "internal error : uncompatible value and constraint types"
    | (_,RangeConstraint _) ->
        Npkcontext.report_error "Ada_utils.check_static_constraint"
          "internal error : value with non static constraint"

let check_static_subtyp subtyp value =
  match subtyp with
    | Unconstrained(_) -> ()
    | Constrained(_, contrainte, true,_) ->
        if not
          (value_is_static_constraint_compatible
             contrainte value)
        then begin
          Npkcontext.report_error "Ada_utils.check_static_subtyp"
            "constraint error : value not in range"
        end
    | Constrained(_, _, false,_) ->
        raise NonStaticExpression
    | SubtypName _ ->
        Npkcontext.report_error "Ada_utils.check_static_subtyp"
          "internal error : unexpected subtype name"

(* fonctions pour la gestion des types *)

let base_typ subtyp = match subtyp with
  | Unconstrained typ -> typ
  | Constrained (typ, _, _,_) -> typ
  | SubtypName _ ->
      Npkcontext.report_error "Ada_utils.base_type"
        "internal error : unexpected subtyp name"

let extract_subtyp (_, _, subtyp,_) =
  match subtyp with
  | None ->
      Npkcontext.report_error
        "Ada_utils.extract_subtyp"
        "internal error : no subtyp provided"
  | Some st -> st

let extract_typ subtyp_ind =
  base_typ (extract_subtyp subtyp_ind)

let eq_base_typ subtyp1 subtyp2 =
  (base_typ subtyp1) = (base_typ subtyp2)

let rec integer_class typ = match typ with
  | Float
  | Boolean
  | Character -> false
  | Integer
  | IntegerConst -> true
  | Declared(_,typdef,_,_) ->
      (match typdef with
         | Enum _
         | Array _ -> false
         | IntegerRange _ -> true
         | DerivedType(_,_,Some(subtyp),_) -> integer_class (base_typ subtyp)
         | DerivedType(_,_,None,_) -> Npkcontext.report_error
                               "Ada_utils.integer_class"
                               "internal error : no subtype provided"
      )

let check_typ expected found =
  match (expected, found) with
    | (None, t ) -> t
    | (Some(t1), t2) when t1=t2 -> t1
    | (Some(IntegerConst), t) when (integer_class t) -> t
    | (Some(t), IntegerConst) when (integer_class t) -> t
    | _ ->
        Npkcontext.report_error "Ada_utils.check_typ"
          "uncompatible types"

and known_compatible_typ expected found =
  match (expected, found) with
    | (Some(t1), t2) when t1=t2                      -> true
    | (Some(IntegerConst), t) when (integer_class t) -> true
    | (Some(t), IntegerConst) when (integer_class t) -> true
    | _                                              -> false


(* determine, si possible, le type des operandes d'une
   operations binaire, en fonction du type attendu pour
   le resultat et de l'operateur.*)
let typ_operand op expected_typ = match op with
  | Ast.Plus | Ast.Minus | Ast.Mult | Ast.Div | Ast.Power ->
      (* si il y a un type attendu, on verifie qu'il est
         entier ou flottant *)
      (match expected_typ with
         | None -> None
         | Some(t) when (integer_class t) -> Some(t)
         | Some(Float) -> Some(Float)
         | Some(_) ->
             Npkcontext.report_error
               "Firstpass.translate_binop"
               "invalid operator and argument")

  | Ast.Rem | Ast.Mod ->
      (* type entier uniquement *)
      (match expected_typ with
         | None -> None
         | Some t when integer_class t -> Some(t)
         | Some _ ->
             Npkcontext.report_error
               "Firstpass.translate_binop"
               "invalid operator and argument")

  | Ast.Gt | Ast.Eq ->
      (* type attendu booleen,
         on ne sait rien sur les operandes *)
      (match expected_typ with
         | None -> None
         | Some(Boolean) -> None
         | Some(_) ->
             Npkcontext.report_error
               "Firstpass.translate_binop"
               "invalid operator and argument")

  | Ast.And | Ast.Or ->
      (* type attendu : booleen
         type de l'operande : booleen *)
      (match expected_typ with
         | None -> Some(Boolean)
         | Some(Boolean) -> Some(Boolean)
         | Some(_) ->
             Npkcontext.report_error
               "Firstpass.translate_binop"
               "invalid operator and argument")

let check_operand_typ op typ = match op with
  | Ast.Plus | Ast.Minus | Ast.Mult | Ast.Div | Ast.Power ->
      (* type entier ou flottant *)
      (match typ with
         | t when (integer_class t) -> ()
         | Float -> ()
         | _ ->
             Npkcontext.report_error
               "Firstpass.translate_binop"
               "invalid operator and argument")

  | Ast.Rem | Ast.Mod ->
      (* type entier uniquement *)
      (match typ with
         | t when (integer_class t) -> ()
         | _ ->
             Npkcontext.report_error
               "Firstpass.translate_binop"
               "invalid operator and argument")

  | Ast.Gt | Ast.Eq -> ()

  | Ast.And | Ast.Or ->
      (* type booleen *)
      if typ <> Boolean then Npkcontext.report_error
                             "Firstpass.translate_binop"
                             "invalid operator and argument"

let ikind_of_range inf sup = (Newspeak.Signed,
                              Ada_config.size_of_range inf sup)

let check_compil_unit_name compil_unit file_name =
  let expected_name = Filename.chop_extension file_name in
  let subprog_name spec = match spec with
    | Function(name,_,_) -> name
    | Procedure(name,_) -> name in
  let (_,lib_item,_) = compil_unit in
  let name =
    match lib_item with
      | Spec(SubProgramSpec(spec))     -> subprog_name spec
      | Body(SubProgramBody(spec,_,_)) -> subprog_name spec
      | Spec(PackageSpec(name,_))   -> name
      | Body(PackageBody(name,_,_)) -> name
  in
    name = expected_name


let extract_representation_clause_name rep_clause = match rep_clause with
  | EnumerationRepresentation(ident, _) -> ident

let with_default (opt:'a option) (def_value:'a):'a = match opt with
    | None   -> def_value
    | Some x -> x

let list_to_string l to_string sep crochet =
  let r = String.concat sep (List.map to_string l) in
  if crochet then "["^r^"]" else r

let ident_list_to_string l =
  list_to_string l (fun x -> x) "." false

let name_to_string (package, ident) =
  match package with
    | None   -> ident
    | Some p -> ident_list_to_string (p::ident::[])

let make_operator_name op =
  let ada2npk_operator_prefix = "__ada2npk_operator_" in
  ada2npk_operator_prefix^
   (match op with
   | And| AndThen -> "logical_and"
   | Or | OrElse  -> "logical_and"
   | Xor          -> "xor"
   | Eq           -> "equals"
   | Neq          -> "not_equals"
   | Lt           -> "lt"
   | Le           -> "le"
   | Gt           -> "gt"
   | Ge           -> "ge"
   | Plus         -> "plus"
   | Minus        -> "minus"
   | Mult         -> "times"
   | Div          -> "div"
   | Mod          -> "mod"
   | Rem          -> "rem"
   | Power        -> "pow"
   )

let operator_of_string s = match s with
  | "and" -> And
  | "or"  -> Or
  | "xor" -> Xor
  | "="   -> Eq
  | "/="  -> Neq
  | "<"   -> Lt
  | "<="  -> Le
  | ">"   -> Gt
  | ">="  -> Ge
  | "+"   -> Plus
  | "-"   -> Minus
  | "*"   -> Mult
  | "/"   -> Div
  | "mod" -> Mod
  | "rem" -> Rem
  | "**"  -> Power
  |_ -> Npkcontext.report_error "operator_of_string"
         ("\""^s^"\" does not name an operator")

let may f = function
  | None -> None
  | Some v -> Some (f v)
