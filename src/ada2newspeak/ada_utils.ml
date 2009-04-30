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
module Nat = Newspeak.Nat

exception NonStaticExpression

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

let mod_ada a b =
  let a = Nat.to_big_int a
  and b = Nat.to_big_int b in
  let r = Big_int.mod_big_int a b in
  let r_mod =
    if (Big_int.sign_big_int b) < 0
    then Big_int.sub_big_int r b
    else r
  in
    Nat.of_big_int r_mod

let rem_ada na nb =
  let a = Nat.to_big_int na
  and b = Nat.to_big_int nb in
  let r = Big_int.mod_big_int a b in
  let r_mod =
    if (Big_int.sign_big_int a) < 0
    then Big_int.sub_big_int r b
    else r
  in
    Nat.of_big_int r_mod

(* calcul sur les value *)

let eq_val v1 v2 =
  let eq a b = a=b in
    match (v1, v2) with
      | (IntVal v1, IntVal v2) -> eq v1 v2
      | (BoolVal v1, BoolVal v2) -> eq v1 v2
      | (FloatVal v1, FloatVal v2) -> eq v1 v2
      | _ ->
          Npkcontext.report_error "Ada_utils.eq_val"
            "internal error : uncompatible value"

let inf_val v1 v2 =
  let inf a b = a<b in
    match (v1, v2) with
      |   IntVal v1,   IntVal v2 -> (Nat.compare v1 v2) < 0
      |  BoolVal v1,  BoolVal v2 -> inf v1 v2
      | FloatVal v1, FloatVal v2 -> inf v1 v2
 (*     | (EnumVal(v1), EnumVal(v2)) -> inf v1 v2*)
      | _ ->
          Npkcontext.report_error "Ada_utils.inf_val"
            "internal error : uncompatible value"

(* contraintes *)

let nat_of_bool b = match b with
  | true -> Newspeak.Nat.one
  | false -> Newspeak.Nat.zero

(* verifie que a <= n <= b *)
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
let constraint_is_constraint_compatible cref courante =
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
    | Constrained(_, contrainte, true) ->
        if not
          (value_is_static_constraint_compatible
             contrainte value)
        then begin
          Npkcontext.report_error "Ada_utils.check_static_subtyp"
            "constraint error : value not in range"
        end
    | Constrained(_, _, false) ->
        raise NonStaticExpression
    | SubtypName _ ->
        Npkcontext.report_error "Ada_utils.check_static_subtyp"
          "internal error : unexpected subtype name"

let constraint_is_static contrainte = match contrainte with
  | FloatRangeConstraint   _ -> true
  | IntegerRangeConstraint _ -> true
  | RangeConstraint        _ -> false

(* fonctions pour la gestion des types *)

let base_typ subtyp = match subtyp with
  | Unconstrained typ -> typ
  | Constrained (typ, _, _) -> typ
  | SubtypName _ ->
      Npkcontext.report_error "Ada_utils.base_type"
        "internal error : unexpected subtyp name"

let extract_subtyp (_, _, subtyp) =
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
  | Character
  | String       -> false
  | Integer
  | IntegerConst -> true
  | Declared(_,typdef,_) ->
      (match typdef with
         | Enum _
         | Array _
         | Record _ -> false
         | IntegerRange _ -> true
         | DerivedType(_,_,Some(subtyp)) -> integer_class (base_typ subtyp)
         | DerivedType(_,_,None) -> Npkcontext.report_error
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
  | Plus | Minus | Mult | Div | Power ->
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

  | Rem | Mod ->
      (* type entier uniquement *)
      (match expected_typ with
         | None -> None
         | Some t when integer_class t -> Some(t)
         | Some _ ->
             Npkcontext.report_error
               "Firstpass.translate_binop"
               "invalid operator and argument")

  | Lt | Gt | Le | Ge | Eq | Neq ->
      (* type attendu booleen,
         on ne sait rien sur les operandes *)
      (match expected_typ with
         | None -> None
         | Some(Boolean) -> None
         | Some(_) ->
             Npkcontext.report_error
               "Firstpass.translate_binop"
               "invalid operator and argument")

  | And | Or | Xor | OrElse | AndThen ->
      (* type attendu : booleen
         type de l'operande : booleen *)
      (match expected_typ with
         | None -> Some(Boolean)
         | Some(Boolean) -> Some(Boolean)
         | Some(_) ->
             Npkcontext.report_error
               "Firstpass.translate_binop"
               "invalid operator and argument")

  | Concat ->
      Npkcontext.report_error
        "Firstpass.translate_binop"
        "concat not implemented"

let check_operand_typ op typ = match op with
  | Plus | Minus | Mult | Div | Power ->
      (* type entier ou flottant *)
      (match typ with
         | t when (integer_class t) -> ()
         | Float -> ()
         | _ ->
             Npkcontext.report_error
               "Firstpass.translate_binop"
               "invalid operator and argument")

  | Rem | Mod ->
      (* type entier uniquement *)
      (match typ with
         | t when (integer_class t) -> ()
         | _ ->
             Npkcontext.report_error
               "Firstpass.translate_binop"
               "invalid operator and argument")

  | Lt  | Gt | Le  | Ge  | Eq | Neq -> ()

  | And | Or | Xor | OrElse | AndThen ->
      (* type booleen *)
      (match typ with
         | Boolean -> ()
         | _ ->
             Npkcontext.report_error
               "Firstpass.translate_binop"
               "invalid operator and argument")
  | Concat ->
      Npkcontext.report_error
        "Firstpass.translate_binop"
        "concat not implemented"

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
      | Spec(SubProgramSpec(spec)) -> subprog_name spec
      | Spec(PackageSpec(name,_)) -> name
      | Body(SubProgramBody(spec,_,_)) -> subprog_name spec
      | Body(PackageBody(name,_,_,_)) -> name
  in
    match name with
      | ([],ident) -> ident=expected_name
      | _ -> false


let extract_representation_clause_name rep_clause = match rep_clause with
  | EnumerationRepresentation(ident, _) -> ident
  | AttributeDefinitionClause _ -> Npkcontext.report_error
                "extract_representation_clause_name"
        "not yet implemented for AttributeDefinitionClause" (* FIXME *)

let with_default (opt:'a option) (def_value:'a):'a = match opt with
    | None   -> def_value
    | Some x -> x

let list_to_string l to_string sep crochet =
  let r = String.concat sep (List.map to_string l) in
  if crochet then "["^r^"]" else r

let ident_list_to_string l =
  list_to_string l (fun x -> x) "." false

let name_to_string (packages, ident) =
  ident_list_to_string (packages@[ident])

class package_manager =
    object (self)
        val mutable current_pkg:package                 = []
        val mutable    with_pkg:package list            = []
        val             context:(package,int) Hashtbl.t = Hashtbl.create 3
        val mutable    extflag :bool                    = false

        method set_current (x,y) :unit =
          let p = x@[y] in
            current_pkg <- p

        method reset_current =
            current_pkg <- []

        method current =
            current_pkg

        method add_with (x,y) =
          let p = x@[y] in
            with_pkg <- p::with_pkg

        method is_with pkg =
            List.mem pkg with_pkg

        method add_use (x,y) =
          let p = x@[y] in
          if (self#current <> p) then begin
              if (not (self#is_with p)) then begin
                Npkcontext.report_error "Ada_normalize.add_context"
                  ((ident_list_to_string p)^" is undefined")
              end;
              let old_count = try Hashtbl.find context p with Not_found -> 0 in
              Hashtbl.replace context p (old_count + 1)
          end

        method remove_use (x,y) =
          let p = x@[y] in
          try
            let old_count = Hashtbl.find context p in
            if old_count = 1 then Hashtbl.remove context p
            else Hashtbl.replace context p (old_count - 1)
          with Not_found -> ()

        method get_use =
          Hashtbl.fold (fun pkg _ res -> pkg::res) context []

        method is_extern =
          extflag

        method as_extern_do (f:unit->unit) =
          extflag <- true;
          f ();
          extflag <- false

    end

let make_operator_name opname =
    let ada2npk_operator_prefix = "__ada2npk_operator_" in
     match opname with
     | "and" -> ada2npk_operator_prefix ^ "logical_and"
     | "or"  -> ada2npk_operator_prefix ^ "logical_and"
     | "xor" -> ada2npk_operator_prefix ^ "xor"
     | "="   -> ada2npk_operator_prefix ^ "equals"
     | "/="  -> ada2npk_operator_prefix ^ "not_equals"
     | "<"   -> ada2npk_operator_prefix ^ "lt"
     | "<="  -> ada2npk_operator_prefix ^ "le"
     | ">"   -> ada2npk_operator_prefix ^ "gt"
     | ">="  -> ada2npk_operator_prefix ^ "ge"
     | "+"   -> ada2npk_operator_prefix ^ "plus"
     | "-"   -> ada2npk_operator_prefix ^ "minus"
     | "&"   -> ada2npk_operator_prefix ^ "binary_and"
     | "*"   -> ada2npk_operator_prefix ^ "times"
     | "/"   -> ada2npk_operator_prefix ^ "div"
     | "mod" -> ada2npk_operator_prefix ^ "mod"
     | "rem" -> ada2npk_operator_prefix ^ "rem"
     | "**"  -> ada2npk_operator_prefix ^ "pow"
     | "abs" -> ada2npk_operator_prefix ^ "abs"
     | "not" -> ada2npk_operator_prefix ^ "not"
     | _     -> Npkcontext.report_error "Parser.make_operator_name"
             ("Cannot overload '"^opname^"' : it is not an operator")

let operator_of_binop = function
  | Plus          -> "+"
  | Minus         -> "-"
  | Mult          -> "*"
  | Div           -> "/"
  | Power         -> "**"
  | Concat        -> "^"
  | Mod           -> "mod"
  | Rem           -> "rem"
  | Eq            -> "="
  | Neq           -> "/="
  | Le            -> "<="
  | Lt            -> "<"
  | Ge            -> ">="
  | Gt            -> ">"
  | And           -> "and"
  | Or            -> "or"
  | Xor           -> "xor"
  | AndThen       -> "and then"
  | OrElse        -> "or else"
