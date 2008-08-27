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
(* arithmétique*)
let log2_sup n = 
  let zero = Big_int.zero_big_int
  and one = Big_int.unit_big_int in
  let two = Big_int.succ_big_int one
  and eq_big_int a b = (Big_int.compare_big_int a b)=0 
  in
  let rec aux n p = 
    if eq_big_int n zero then one
    else
      if eq_big_int n one then p
      else aux (Big_int.add_big_int 
		  (Big_int.div_big_int n two) (Big_int.mod_big_int n two)) 
	(Big_int.succ_big_int p)
  in Big_int.int_of_big_int (aux n zero)
	
(* fonction propre à Ada *)
let puiss a b = 
  let a = Nat.to_big_int a
  and b = Nat.to_big_int b in
    if (Big_int.sign_big_int b)<0
    then
      Npkcontext.error
	"Ada_utils.puiss"
	"integer exponent negative"
    else
      Nat.of_big_int (Big_int.power_big_int_positive_big_int a b)
      
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

let xor a b = (a && (not b)) || ((not a) && b)

(* calcul sur les value *)

let eq_val v1 v2 = 
  let eq a b = a=b in
    match (v1, v2) with
      | (IntVal(v1), IntVal(v2)) -> eq v1 v2
      | (BoolVal(v1), BoolVal(v2)) -> eq v1 v2
      | (FloatVal(v1), FloatVal(v2)) -> eq v1 v2
      | (EnumVal(v1), EnumVal(v2)) -> eq v1 v2
      | _ ->
	  Npkcontext.error
	    "Ada_utils.eq_val"
	    "internal error : uncompatible value"
	  
let inf_val v1 v2 =
  let inf a b = a<b in
    match (v1, v2) with
      | (IntVal(v1), IntVal(v2)) -> (Nat.compare v1 v2) < 0
      | (BoolVal(v1), BoolVal(v2)) -> inf v1 v2
      | (FloatVal(v1), FloatVal(v2)) -> inf v1 v2
      | (EnumVal(v1), EnumVal(v2)) -> inf v1 v2
      | _ ->
	  Npkcontext.error
	    "Ada_utils.inf_val"
	    "internal error : uncompatible value"

(* contraintes *)

let nat_of_bool b = match b with
  | true -> Newspeak.Nat.one
  | false -> Newspeak.Nat.zero


(* vérifie que a <= n <= b *)
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


(* vérifie que la contrainte courante est compatible avec cref *)
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
    | (NullRange, RangeConstraint _) -> true 
	(* non connu à la compilation *)
    | (NullRange, IntegerRangeConstraint(v1, v2)) -> (Nat.compare v2 v1) < 0
    | (NullRange, FloatRangeConstraint(v1, v2)) -> v2 < v1
    | (_, NullRange) -> true
    | (IntegerRangeConstraint _, FloatRangeConstraint _) 
    | (FloatRangeConstraint _, IntegerRangeConstraint _) -> 
	Npkcontext.error
	  "Ada_utils.check_constraint"
	  "internal error : uncompatible constraints"

(* vérifie que la valeur value respecte bien la contrainte.
   la contrainte est supposé statique, et le type de value
   compatible avec cette contrainte. Ce qui implique :
   - soit on a une valeur, et la contrainte est numérique
     et de meme type
   - soit on a pas de valeur *)
let value_is_static_constraint_compatible contrainte value = 
  match (value,contrainte) with
    | (_, NullRange) -> false
    | (EnumVal(n), IntegerRangeConstraint(inf,sup)) ->
	between_nat inf sup (Newspeak.Nat.of_int n)
    | (IntVal(n), IntegerRangeConstraint(inf,sup)) ->
	between_nat inf sup n
    | (BoolVal(b), IntegerRangeConstraint(inf,sup)) ->
	between_nat inf sup (nat_of_bool b)
    | (FloatVal(n), FloatRangeConstraint(inf,sup)) ->
	between inf sup n
    | ((BoolVal _|IntVal _| EnumVal _), FloatRangeConstraint _) 
    | (FloatVal _, IntegerRangeConstraint _) -> 
	Npkcontext.error
	  "Ada_utils.check_static_constraint"
	  "internal error : uncompatible value and constraint types"
    | (_,RangeConstraint _) -> 
	Npkcontext.error
	  "Ada_utils.check_static_constraint"
	  "internal error : value with non static constraint"
	  
let check_static_subtyp subtyp value = 
  match subtyp with
    | Unconstrained(_) -> ()
    | Constrained(_, contrainte, true) ->
	if not 
	  (value_is_static_constraint_compatible
	     contrainte value)
	then
	  Npkcontext.error
	    "Ada_utils.check_static_subtyp"
	    "constraint error : value not in range"
    | Constrained(_, _, false) ->
	raise NonStaticExpression
    | SubtypName _ ->
	Npkcontext.error
	  "Ada_utils.check_static_subtyp"
	  "internal error : unexpected subtype name"
	  
let constraint_is_static contrainte = match contrainte with
  | FloatRangeConstraint _ -> true
  | IntegerRangeConstraint _ -> true
  | RangeConstraint _ -> false
  | NullRange -> true

(* fonctions pour la gestion des types *)
  
let base_typ subtyp = match subtyp with
  | Unconstrained(typ) -> typ
  | Constrained(typ,_,_) -> typ
  | SubtypName(_) ->
      Npkcontext.error
	"Ada_utils.base_type"
	"internal error : unexpected subtyp name"

let extract_subtyp (_,_,subtyp) = match subtyp with
  | None -> Npkcontext.error
      "Ada_utils.extract_subtyp"
	"internal error : no subtyp provided"
  | Some(st) -> st

let extract_typ subtyp_ind = base_typ (extract_subtyp subtyp_ind)

let eq_base_typ subtyp1 subtyp2 =
  (base_typ subtyp1) = (base_typ subtyp2)

let rec integer_class typ = match typ with
  | Integer -> true
  | IntegerConst -> true
  | Declared(typdef, _) ->
      (match typdef with
	 | Enum(_) -> false
	 | IntegerRange(_) -> true
	 | DerivedType(_,(_,_,Some(subtyp))) -> integer_class 
	     (base_typ subtyp)
	 | DerivedType(_,(_,_,None)) ->
	     Npkcontext.error
	       "Ada_utils.integer_class"
	       "internal error : no subtype provided")
  | Float -> false
  | Boolean -> false
  | Character -> false 
  | String -> false

  
let check_typ expected found = 
  match (expected, found) with
    | (None, t ) -> t
    | (Some(t1), t2) when t1=t2 -> t1
    | (Some(IntegerConst), t) when (integer_class t) -> t
    | (Some(t), IntegerConst) when (integer_class t) -> t
    | _ -> Npkcontext.error "Ada_utils.check_typ"
	"uncompatible types"
	  
and known_compatible_typ expected found = 
  match (expected, found) with
    | (Some(t1), t2) when t1=t2 ->true
    | (Some(IntegerConst), t) when (integer_class t) -> true
    | (Some(t), IntegerConst) when (integer_class t) -> true
    | (Some(_), _) -> false
    | (None,_) -> false

 
(* détermine, si possible, le type des opérandes d'une
   opérations binaire, en fonction du type attendu pour
   le résultat et de l'opérateur.*)
let typ_operand op expected_typ = match op with
  | Plus | Moins | Fois | Div | Puissance ->
      (* si il y a un type attendu, on vérifie qu'il est
	 entier ou flottant *)
      (match expected_typ with
	 | None -> None
	 | Some(t) when (integer_class t) -> Some(t)
	 | Some(Float) -> Some(Float)
	 | Some(_) -> Npkcontext.error 
	     "Firstpass.translate_binop" 
	       "invalid operator and argument")
	
  | Rem | Mod -> 
      (* type entier uniquement *)
      (match expected_typ with
	 | None -> None
	 | Some(t) when (integer_class t) -> Some(t)
	 | Some(_) -> Npkcontext.error 
	     "Firstpass.translate_binop" 
	       "invalid operator and argument")
	
  | Lt | Gt | Le | Ge | Eq | Neq -> 
      (* type attendu booléen, 
	 on ne sait rien sur les opérandes *)
      (match expected_typ with
	 | None -> None
	 | Some(Boolean) -> None
	 | Some(_) -> Npkcontext.error 
	     "Firstpass.translate_binop" 
	       "invalid operator and argument")
	
  | And | Or | Xor | OrElse | AndThen -> 
      (* type attendu : booléen
	 type de l'opérande : booléen *)
      (match expected_typ with
	 | None -> Some(Boolean)
	 | Some(Boolean) -> Some(Boolean)
	 | Some(_) -> Npkcontext.error 
	     "Firstpass.translate_binop" 
	       "invalid operator and argument")
	
  | Concat -> 
      Npkcontext.error 
	"Firstpass.translate_binop" 
	"concat not implemented"
	
let check_operand_typ op typ = match op with
  | Plus | Moins | Fois | Div | Puissance ->
      (* type entier ou flottant *)
      (match typ with
	 | t when (integer_class t) -> ()
	 | Float -> ()
	 | _ -> Npkcontext.error 
	     "Firstpass.translate_binop" 
	       "invalid operator and argument")
	
  | Rem | Mod -> 
      (* type entier uniquement *)
      (match typ with
	 | t when (integer_class t) -> ()
	 | _ -> Npkcontext.error 
	     "Firstpass.translate_binop" 
	       "invalid operator and argument")
	
  | Lt | Gt | Le | Ge | Eq | Neq -> ()
      
  | And | Or | Xor | OrElse | AndThen -> 
      (* type booléen *)
      (match typ with
	 | Boolean -> ()
	 | _ -> Npkcontext.error 
	     "Firstpass.translate_binop" 
	       "invalid operator and argument")
	
  | Concat -> 
      Npkcontext.error 
	"Firstpass.translate_binop" 
	"concat not implemented"
	
let make_enum nom list_val = 
  let rec make_id list_val list_val_id next_id = 
    match list_val with
       | [] -> (list_val_id, next_id)
       | v::r -> make_id r ((v,next_id)::list_val_id) (next_id +1)
  in 
  let (list_assoc,taille) = make_id list_val [] 0
  in Enum(nom, list_assoc, log2_sup (Big_int.big_int_of_int taille))
 

let ikind_of_range inf sup = 
  let (b_inf, b_sup) = 
    if (Nat.compare inf sup)<=0 then (inf, sup)
    else (sup, inf)
  in
    begin
      let (bit_signe,signe) = 
	if (Nat.compare b_inf Nat.zero)<0 then (1,Newspeak.Signed)
	else (0,Newspeak.Unsigned)
      in
      let max = Big_int.max_big_int
	(Big_int.abs_big_int (Nat.to_big_int b_inf))
	(Big_int.abs_big_int (Big_int.succ_big_int (Nat.to_big_int b_sup)))
      in
      let nb_bit = (log2_sup max) + bit_signe
      in 
	(signe, nb_bit)
    end
	
let make_range nom exp_b_inf exp_b_sup = 
  IntegerRange(nom, RangeConstraint(exp_b_inf, exp_b_sup), None)

      
    
