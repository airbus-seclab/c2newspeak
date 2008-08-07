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
exception NonStaticExpression
(* arithmétique*)
let log2_sup n = 
  let rec aux n p = match n with
    | 0 -> 1
    | 1 -> p
    | _ -> aux (n/2 + (n mod 2)) (p+1)
   in aux n 0  
	
(* fonction propre à Ada *)
let puiss a b =
  let rec aux p b = match b with
    | 0 -> p
    | _ -> aux (p*a) (b-1)
  in
    if b<0
    then
      Npkcontext.error
	"Ada_utils.puiss"
	"integer exponent negative"
    else
      aux 1 b
      
let mod_ada a b = 
  (a mod b) + (if a<0 or b<0 then b
	       else 0)

let rem_ada = (mod)

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
      | (IntVal(v1), IntVal(v2)) -> inf v1 v2
      | (BoolVal(v1), BoolVal(v2)) -> inf v1 v2
      | (FloatVal(v1), FloatVal(v2)) -> inf v1 v2
      | (EnumVal(v1), EnumVal(v2)) -> inf v1 v2
      | _ ->
	  Npkcontext.error
	    "Ada_utils.inf_val"
	    "internal error : uncompatible value"

(* contraintes *)

let int_of_bool b = match b with
  | true -> 1
  | false -> 0  


(* vérifie que a <= n <= b *)
let between a b n = a <= n && n <= b

let numeric_constraint_compatibility ref1 ref2 fils1 fils2 = 
  if ref2<ref1
  then fils2<fils1
  else (between ref1 ref2 fils1) && (between ref1 ref2 fils2)

(* vérifie que la contrainte courante est compatible avec cref *)
let constraint_is_constraint_compatible cref courante = 
  match (cref, courante) with
    | (IntegerRangeConstraint(ref1, ref2, _),
       IntegerRangeConstraint(cour1, cour2, _)) ->
	numeric_constraint_compatibility ref1 ref2 cour1 cour2
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
    | (NullRange, IntegerRangeConstraint(v1, v2, _)) -> v2 < v1
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
    | ((EnumVal(n)|IntVal(n)), IntegerRangeConstraint(inf,sup,_)) ->
	between inf sup n
    | (BoolVal(b), IntegerRangeConstraint(inf,sup,_)) ->
	between inf sup (int_of_bool b)
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
  | TypName(_) -> Npkcontext.error 
      "Ada_utils.integer_class"
	"internal error : unexpected Type Ident"
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
  in Enum(nom, list_assoc, log2_sup taille)
 

let ikind_of_range inf sup = 
  let (b_inf, b_sup) = 
    if inf <= sup then (inf, sup)
    else (sup, inf)
  in
    if b_inf > b_sup
    then 
      Npkcontext.error 
	"Ada_utils.ikind_of_range" 
	"invalid range"
    else
      begin
	let (bit_signe,signe) = 
	  if b_inf < 0 then (1,Newspeak.Signed)
	  else (0,Newspeak.Unsigned)
	in
	let max = max (abs b_inf) (abs b_sup + 1)
	in
	let nb_bit = (log2_sup max) + bit_signe
	in 
	  (signe, nb_bit)
      end
	
let make_range nom exp_b_inf exp_b_sup = 
  IntegerRange(nom, RangeConstraint(exp_b_inf, exp_b_sup), None)

      
    
