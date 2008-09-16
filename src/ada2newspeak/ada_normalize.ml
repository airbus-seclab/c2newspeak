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

exception AmbiguousTypeException
exception NonStaticExpression = Ada_utils.NonStaticExpression

let float_to_flottant f = (f, string_of_float f) 

let tmp_cnt = ref 0

let gen_tmp _ = 
  let x = "tmp_range"^(string_of_int !tmp_cnt)
  in 
    incr tmp_cnt;
    x
       

let base_typ = Ada_utils.base_typ
let check_typ = Ada_utils.check_typ
let known_compatible_typ = Ada_utils.known_compatible_typ

(* variable booléenne :
   cas fonction : extern
   autres : global *)
type constant_symb = 
  | Number of value*bool
  | StaticConst of value*typ*bool
  | EnumLitteral of typ*int*bool
  | VarSymb of bool
  | FunSymb of typ option*bool

let string_of_name = Print_syntax_ada.name_to_string

let normalize_extern_ident ident package = (package, ident)

let normalize_ident ident package extern = match extern with
  | false -> ([], ident)
  | true -> normalize_extern_ident ident package

let normalize_name name with_package current_package extern = 
  let add_package (parents, ident) pack = match parents with
    | [] -> (pack, ident)
    | a when a=pack -> (pack, ident)
    | a when (List.mem a with_package) -> (a, ident)
    | _ -> Npkcontext.error 
	"ada_normalize.normalize_name.add_package"
	  ("unknown package "
	   ^(Print_syntax_ada.ident_list_to_string parents)) in
    match extern with
      | false -> name (*suppr_package name (!current_package)*)
      | true -> add_package name current_package


let eval_static exp expected_typ csttbl context with_package
    current_package extern = 
  
  let find_all_cst nom = Hashtbl.find_all csttbl nom in

  let find_all_use ident = 
    List.flatten
      (List.map
	 (fun pack -> find_all_cst (pack, ident))
	 context) in

  let rec eval_static_exp exp expected_typ :(Syntax_ada.value * Syntax_ada.typ) = 
    match exp with
      | CInt(i) -> 
	  let typ = Ada_utils.check_typ expected_typ IntegerConst
	  in (IntVal(i), typ)
	       
      | CFloat(f,s) -> 
	  let typ = Ada_utils.check_typ expected_typ Float
	  in (FloatVal(f,s), typ)
	       
      | CChar(c) -> 	  
	  (EnumVal(c), Ada_utils.check_typ expected_typ Character)
      | CBool(b) -> 
	  (BoolVal(b), Ada_utils.check_typ expected_typ Boolean)


      | Var(v) -> 
	  eval_static_const (normalize_name v with_package
			     current_package extern)
	    expected_typ
	    
      | FunctionCall(_) -> raise NonStaticExpression
	  
      | NullExpr | CString (_) -> Npkcontext.error 
	  "Ada_normalize.eval_static_exp"
	    "not implemented"
	    
      | Unary(op,exp) -> 
	  eval_static_unop op exp expected_typ
	  
      | Binary(op,e1,e2) -> 
	  eval_static_binop op e1 e2 expected_typ
	    
      | Qualified(subtyp, exp) -> 
	  let typ = Ada_utils.check_typ expected_typ 
	    (Ada_utils.base_typ subtyp) in 
	  let (value,typ) = eval_static_exp exp (Some(typ)) in
	    Ada_utils.check_static_subtyp subtyp value;
	    (value, typ)
 
	
  (* expected_typ : type du résultat de l'opération, pas
     des opérandes *)
  and eval_static_binop op e1 e2 expected_typ =
    let expected_typ1 = Ada_utils.typ_operand op expected_typ in 
    let (val1, val2, typ) = 
      try
	let (val1, typ1) = eval_static_exp e1 expected_typ1 in 
	  match op with
	    | Puissance ->
		let (val2, _) = eval_static_exp e2 (Some(Integer))
		in (val1, val2, typ1)
	    | _ ->
		let (val2, typ2) = eval_static_exp e2 (Some(typ1))
		in (val1, val2, typ2)
      with
	  AmbiguousTypeException ->
	    try
	      let (val2, typ2) =
		eval_static_exp e2 expected_typ1
	      in let (val1, typ1) = 
		  eval_static_exp e1 (Some(typ2))
	      in (val1, val2, typ1)
	    with
		AmbiguousTypeException -> 
		  Npkcontext.error "Ada_normalize.eval_static_exp" 
		    "ambiguous operands"
    in 
      Ada_utils.check_operand_typ op typ;
      match (op,val1,val2) with	  
	  (* opérations sur entiers ou flottants *)
	| (Plus, IntVal(v1), IntVal(v2)) -> 
	    (IntVal(Nat.add v1 v2), typ)
	| (Plus, FloatVal(v1,_), FloatVal(v2,_)) -> 
	    (FloatVal(float_to_flottant (v1 +. v2)), typ)
	      
	| (Moins, IntVal(v1), IntVal(v2)) -> 
	    (IntVal(Nat.sub v1 v2), typ)
	| (Moins, FloatVal(v1,_), FloatVal(v2,_)) -> 
	    (FloatVal(float_to_flottant(v1 -. v2)), typ)
	      
	| (Fois, IntVal(v1), IntVal(v2)) -> 
	    (IntVal(Nat.mul v1 v2), typ)
	| (Fois, FloatVal(v1,_), FloatVal(v2,_)) -> 
	    (FloatVal(float_to_flottant(v1 *. v2)), typ)
	      
	| (Div, IntVal(v1), IntVal(v2)) -> 
	    (IntVal(Nat.div v1 v2), typ)
	| (Div, FloatVal(v1,_), FloatVal(v2,_)) -> 
	    (FloatVal(float_to_flottant (v1 /. v2)), typ)
	      
	| (Puissance, IntVal(v1), IntVal(v2)) -> 
	    (IntVal(Ada_utils.puiss v1 v2), typ)
	| (Puissance, FloatVal(v1,_), IntVal(v2)) -> 
	    (FloatVal(float_to_flottant (v1 ** (float_of_int (Nat.to_int v2)))), 
		      typ)
	      
	(*opérations sur les entiers*)
	| (Rem, IntVal(v1), IntVal(v2)) ->
	    (IntVal(Ada_utils.rem_ada v1 v2), typ)
	      	      
	| (Mod, IntVal(v1), IntVal(v2)) -> 
	    (IntVal(Ada_utils.mod_ada v1 v2), typ)
	    
	(* comparaisons *)
	| (Eq, v1, v2) ->
	    (BoolVal(Ada_utils.eq_val v1 v2), Boolean)

	| (Neq, v1, v2) ->
	    (BoolVal(not (Ada_utils.eq_val v1 v2)), Boolean)

	| (Lt, v1, v2) ->
	    (BoolVal(Ada_utils.inf_val v1 v2), Boolean)
	    
	| (Le, v1, v2) ->
	    (BoolVal(not (Ada_utils.inf_val v2 v1)), Boolean)


	| (Ge, v1, v2) ->
	    (BoolVal(not (Ada_utils.inf_val v1 v2)), Boolean)
	    
	| (Gt, v1, v2) ->
	    (BoolVal(Ada_utils.inf_val v2 v1), Boolean)

	(* opérations sur les booléens *)
	| ((AndThen|And), BoolVal(b1), BoolVal(b2)) ->
	    (BoolVal(b1 && b2), Boolean)

	| ((OrElse|Or), BoolVal(b1), BoolVal(b2)) ->
	    (BoolVal(b1 || b2), Boolean)

	| (Xor, BoolVal(b1), BoolVal(b2)) ->
	    (BoolVal(Ada_utils.xor b1 b2), Boolean)

	      
	(* opérations sur les string *)
	| (Concat,_,_) -> 
	    Npkcontext.error 
	      "Ada_normalize.eval_static_binop"
	      "string error : not implemented "
	    
	| _ -> Npkcontext.error "Ada_normalize.eval_static_binop"
	    "invalid operator and argument"
	      
  and eval_static_unop op exp expected_typ = 
    let oppose exp = 
      match (eval_static_exp exp expected_typ) with
	| (IntVal(i), t) 
	    when (Ada_utils.integer_class t) -> 
	    (IntVal(Nat.neg i), t)
	| (FloatVal(f,_), Float) -> 
	    (FloatVal(float_to_flottant (-.f)), Float)
	| _ -> Npkcontext.error 
	    "Ada_normalize.eval_static_exp"
	      "invalid operator and argument"
	      
    and abs exp =
      match (eval_static_exp exp expected_typ) with
	| (IntVal(i), t) 
	    when (Ada_utils.integer_class t) -> 
	    let abs = if (Nat.compare i Nat.zero)<0 then Nat.neg i
	    else i
	    in (IntVal(abs), t)
	| (FloatVal(f,_), Float) -> 
	    (FloatVal(float_to_flottant (abs_float f)), Float)
	| _ -> Npkcontext.error 
	    "Ada_normalize.eval_static_exp"
	      "invalid operator and argument"
    in
      match (op, expected_typ) with
	  
	| (UPlus, Some(Float)) -> 
	    eval_static_exp exp expected_typ
	| (UPlus, Some(t)) when (Ada_utils.integer_class t) -> 
	    eval_static_exp exp expected_typ
	| (UPlus, None) ->
	    let (tr_exp, typ) = 
	      eval_static_exp exp expected_typ in   
	      (match typ with 
		 | Float -> (tr_exp, typ)
		 | t when (Ada_utils.integer_class t) -> 
		     (tr_exp, typ)
		 | _ -> Npkcontext.error 
		     "Ada_normalize.eval_static_unop" 
		       "Unexpected unary operator and argument")
		
	| (UMoins, None) | (UMoins, Some(Float)) -> oppose exp
	    
	| (UMoins, Some(t)) when (Ada_utils.integer_class t) -> 
	    oppose exp
	      
	| (Abs, None) | (Abs, Some(Float)) -> abs exp
	    
	| (Abs, Some(t)) when (Ada_utils.integer_class t) -> 
	    abs exp
	      
	| (Not, Some(Boolean))| (Not, None) ->
	    (match (eval_static_exp exp expected_typ) with
	       | (EnumVal(b), Boolean) -> 
		   (match b with
		      | 0 -> (EnumVal(1), Boolean)
		      | 1 -> (EnumVal(0), Boolean)
		      | _ -> 
			  Npkcontext.error 
			    "Ada_normalize.eval_static_unop" 
			    "internal error : unvalid boolean value")
	       | _ -> Npkcontext.error 
		   "Ada_normalize.eval_static_unop" 
		     "Unexpected unary operator and argument")

	| _ ->  Npkcontext.error 
	    "Ada_normalize.eval_static_unop" 
	      "Unexpected unary operator and argument"
	      
  and eval_static_const name expected_typ = 
    let rec mem_other_cst list_cst filter use var_masque = 
      match list_cst with
	| [] -> (match use with
		   | None -> false
		   | Some(ident) ->
		       (* on regarde les imports *)
		       mem_other_cst (find_all_use ident) filter
			 None var_masque)
	| (Number(_)|StaticConst(_)|VarSymb(_))::r 
	    when var_masque -> 
	    mem_other_cst r filter use var_masque 
	| (Number(_)|StaticConst(_)|VarSymb(_))::_ -> 
	    Npkcontext.error 
	      "Firstpass.translate_var"
	      ((string_of_name name)^" is not visible : "
	       ^"multiple use clauses cause hiding") 
	      
	(* un autre symbole existe ayant le bon type *)
	| (EnumLitteral(typ,_,_)|FunSymb(Some(typ),_))::_ 
	    when (filter typ) -> true
	    
	(* symbole d'énumération ou de fonctions n'ayant
	   pas le bon type *)
	| (EnumLitteral(_)|FunSymb(_))::r ->  
	    mem_other_cst r filter use var_masque
    in 
    let sans_selecteur ident name =      
      (* les variables masquées le sont par un symbole de fonction
	 ou enum interne.
	 var_possible indique si on peut avoir une variable : 
	 si on a rencontré un symbole d'énumération ou de fonction,
	 (qui ne convenait pas, sinon, on a appelé mem_other symb)
	 on déclenche une erreur si on rencontre une variable *)
	 
      let rec find_use list_cst var_masque var_possible =
	match list_cst with

	  | (Number(_)|StaticConst(_)|VarSymb(_))::r 
	      when var_masque -> find_use r var_masque var_possible

	  | [Number(IntVal(i),_)] when var_possible ->
	      let typ = check_typ expected_typ IntegerConst
	      in (IntVal(i), typ)
	  | [Number(FloatVal(f),_)] when var_possible ->
	      let typ = check_typ expected_typ Float
	      in (FloatVal(f), typ)
		   
	  | [StaticConst(v, typ,_)] when var_possible ->
	      let typ = check_typ expected_typ typ
	      in (v, typ)

	  | [VarSymb(_)] when var_possible -> 
	      raise NonStaticExpression

	  | (Number(_)|StaticConst(_)|VarSymb(_))::_ ->
	      Npkcontext.error
		"Ada_normalize.eval_static_cst"
		(ident^" is not visible : "
		 ^"multiple use clauses cause hiding")
		
	  | EnumLitteral(typ,v,_)::r when 
	      known_compatible_typ expected_typ typ ->
	      if (mem_other_cst r 
		    (known_compatible_typ expected_typ)
		    None var_masque)
	      then (Npkcontext.error
		      "Ada_normalize.eval_static_cst"
		      (ident^" is not visible : "
		       ^"multiple use clauses cause hiding"))
	      else (EnumVal(v), typ)

	  | FunSymb(Some(typ),_)::r
	      when known_compatible_typ expected_typ typ ->
	      if (mem_other_cst r 
		    (known_compatible_typ expected_typ)
		    None var_masque)
	      then (Npkcontext.error
		      "Ada_normalize.eval_static_cst"
		      (ident^" is not visible : "
		       ^"multiple use clauses cause hiding"))
	      else raise NonStaticExpression

	  | EnumLitteral(typ,v,_)::r when expected_typ = None ->
	      if (mem_other_cst r (fun _ -> true) 
		    None var_masque)
	      then raise AmbiguousTypeException 
	      else (EnumVal(v), typ)

	  | FunSymb(Some(_),_)::r when expected_typ = None ->
	      if (mem_other_cst r (fun _ -> true) 
		    None var_masque)
	      then raise AmbiguousTypeException 
	      else raise NonStaticExpression

	  | (EnumLitteral(_)|FunSymb(_))::r -> 
	      find_use r var_masque false

	  | []  when var_masque -> (* variable masqué : au moins
				     un symbol mais mauvais type *)
	      Npkcontext.error
		"Ada_normalize.eval_static_cst" 
		"uncompatible types"

	  | [] -> Npkcontext.error 
	      "Ada_normalize.eval_static_cst" 
		("cannot find symbol "^ident)


      and find_interne list_cst var_masque = 
	match list_cst with
	  | Number(IntVal(i),_)::_ when not var_masque->
	      let typ = check_typ expected_typ IntegerConst
	      in (IntVal(i), typ)
	  | Number(FloatVal(f),_)::_ when not var_masque ->
	      let typ = check_typ expected_typ Float
	      in (FloatVal(f), typ)
		   
	  | StaticConst(v, typ, _)::_ when not var_masque ->
	      let typ = check_typ expected_typ typ
	      in (v, typ)

	  | VarSymb(_)::_ when not var_masque ->
	      raise NonStaticExpression
	      
	  | EnumLitteral(typ, v, _)::_  when 
	      Ada_utils.known_compatible_typ expected_typ typ ->
	      (EnumVal(v), typ)
		
	  | FunSymb(Some(typ),_)::_  when 
	      Ada_utils.known_compatible_typ expected_typ typ ->
	      raise NonStaticExpression

	  | EnumLitteral(typ, v, _)::r when expected_typ=None ->
	      if (mem_other_cst r (fun _ -> true) 
		    (Some(ident)) true)
	      then raise AmbiguousTypeException
	      else (EnumVal(v), typ)
		
	  | FunSymb(Some(_),_)::r when expected_typ=None ->
	      if (mem_other_cst r (fun _ -> true) 
		    (Some(ident)) true)
	      then raise AmbiguousTypeException
	      else raise NonStaticExpression		

	  | (EnumLitteral(_)|FunSymb(_))::r -> 
	      find_interne r true
	      
	  | (Number(_)|StaticConst(_)|VarSymb(_))::r -> 
	      find_interne r var_masque

	  | [] -> find_use (find_all_use ident) var_masque true
	    
      in
	find_interne (find_all_cst name) false


    and avec_selecteur name = 

      (* les variables sont masquées *)
      let rec find_enum list_cst = match list_cst with

	| (Number(_)|StaticConst(_)|VarSymb(_))::r -> find_enum r

	| EnumLitteral(typ,v,_)::_ when 
	    known_compatible_typ expected_typ typ ->
	    (EnumVal(v), typ)

	| FunSymb(Some(typ), _)::_ 
	    when known_compatible_typ expected_typ typ -> 
	    raise NonStaticExpression
	    
	| EnumLitteral(typ,v,_)::r when expected_typ=None ->
	    if mem_other_cst r (fun _ -> true) None true
	    then raise AmbiguousTypeException
	    else (EnumVal(v), typ)

	| FunSymb(Some(_), _)::r when expected_typ=None ->
	    if mem_other_cst r (fun _ -> true) None true
	    then raise AmbiguousTypeException
	    else raise NonStaticExpression

	| (EnumLitteral(_)|FunSymb(_))::r -> find_enum r

	| [] ->  
	    Npkcontext.error
	      "Ada_normalize.eval_static_cst" 
	      "uncompatible types" in

      let list_symb = find_all_cst name in
	match list_symb with

	  | Number(IntVal(i),_)::_ ->
	      let typ = check_typ expected_typ IntegerConst
	      in (IntVal(i), typ)
	  | Number(FloatVal(f),_)::_ ->
	      let typ = check_typ expected_typ Float
	      in (FloatVal(f), typ)
		   
	  | StaticConst(v, typ, _)::_ ->
	      let typ = check_typ expected_typ typ
	      in (v, typ)

	  | VarSymb _::_ -> raise NonStaticExpression
		   
	  | [] -> Npkcontext.error 
	      "Ada_normalize.eval_static_cst" 
		("cannot find symbol "^(string_of_name name))
	  | _ -> find_enum list_symb
	      
    and avec_selecteur_courant ident name = 
      let rec find_global list_symb = 
	match list_symb with
	  | [] -> Npkcontext.error 
	      "Ada_normalize.eval_static_cst" 
		("cannot find symbol "^(string_of_name name))
	  
	  | Number(IntVal(i),true)::_ ->
	      let typ = check_typ expected_typ IntegerConst
	      in (IntVal(i), typ)
	  | Number(FloatVal(f),true)::_ ->
	      let typ = check_typ expected_typ Float
	      in (FloatVal(f), typ)

	  | Number((EnumVal _|BoolVal _),_)::_ ->
	      Npkcontext.error
		"Ada_normalize.eval_static_cst"
		"internal error : number cannot have EnumVal"
	      

	  | StaticConst(v, typ, true)::_ ->
	      let typ = check_typ expected_typ typ
	      in (v, typ)	

	  | EnumLitteral(typ,v,true)::_
	      when known_compatible_typ expected_typ typ ->
	      (EnumVal(v), typ)

	  | FunSymb(Some(typ), false)::_ 
	      when known_compatible_typ expected_typ typ ->
	      raise NonStaticExpression
	      
	  | EnumLitteral(typ, v, true)::r when expected_typ=None ->
	      if mem_other_cst r (fun _ -> true) None false
	      then raise AmbiguousTypeException
	      else (EnumVal(v), typ)

	  | FunSymb(Some(_), false)::r when expected_typ=None ->
	      if mem_other_cst r (fun _ -> true) None false
	      then raise AmbiguousTypeException
	      else raise NonStaticExpression

	  | VarSymb(true)::_ -> raise NonStaticExpression

	  | (Number(_, false)|StaticConst(_,_,false)
	    |EnumLitteral(_)|VarSymb(false)|FunSymb(_))::r -> 
	      find_global r		
      in find_global (find_all_cst ident)
    in 
      match name with
	| ([], ident) -> sans_selecteur ident name
	| (pack, ident)
	    when extern||List.mem pack with_package -> 
	    avec_selecteur (pack,ident)
	| (pack, ident) when pack = current_package ->
	    avec_selecteur_courant ([],ident) name
	| (pack, _) -> Npkcontext.error 
	    "Ada_normalize.eval_static_cst" 
	      ("unknown package "
	       ^(Print_syntax_ada.ident_list_to_string pack))

  in 
      eval_static_exp exp expected_typ
    

 
let eval_static_integer_exp exp csttbl context with_package
    current_package extern = 
  try
    let (v,_) = 
      eval_static 
	exp (Some(IntegerConst)) csttbl
	context with_package current_package extern in
      match v with
	| FloatVal _ | EnumVal _ | BoolVal _->
	    Npkcontext.error 
	      "Ada_normalize.eval_static_integer_exp"
	      "expected static integer constant"
	| IntVal(i) -> i
  with
    | NonStaticExpression -> 
	 Npkcontext.error 
	   "Ada_normalize.eval_static_integer_exp"
	   "expected static expression"
    | AmbiguousTypeException ->
	Npkcontext.error 
	  "Ada_normalize.eval_static_integer_exp"
	  "uncaught ambiguous type exception"
    
let eval_static_number exp csttbl context with_package
    current_package extern = 
  try
    let (v,_) = 
      eval_static 
	exp None csttbl
	context with_package current_package extern in
      match v with
	| EnumVal _ | BoolVal _ ->
	    Npkcontext.error 
	      "Ada_normalize.eval_static_integer_exp"
	      "expected static float or integer constant"
	| FloatVal(f) -> FloatVal(f)
	| IntVal(i) -> IntVal(i)
  with
    | NonStaticExpression -> 
	 Npkcontext.error 
	   "Ada_normalize.eval_static_integer_exp"
	   "expected static expression"
    | AmbiguousTypeException ->
	Npkcontext.error 
	  "Ada_normalize.eval_static_integer_exp"
	  "uncaught ambiguous type exception"
    


let extract_subprog_spec ast = match ast with
  | (context, Body(SubProgramBody(spec,_,_)), loc) -> 
      (context, Spec(SubProgramSpec(spec)), loc)
      
  | (_, Spec(_), _) -> Npkcontext.error 
      "Ada_normalize.extract_subprog_spec" 
	"body expected, specification found"
  | (_, Body(PackageBody(_)), _) -> Npkcontext.error 
      "Ada_normalize.parse_specification" 
	"subprogram body expected, package body found"
	
(* renvoie la spécification correspondant à name,
   extrait éventuellement cette spécification d'un corps
   de sous-programme, dans le cas ou aucun fichier de spécification
   n'est fourni.*)
let rec parse_specification name =

  (* tricherie : problème avec sous-package *)
  let spec_name = (string_of_name name)^".ads" in 
  let spec_ast = 
    if Sys.file_exists spec_name
    then
      Ada_parse.parse spec_name
    else 
      let body_name = (string_of_name name)^".adb" in  
	extract_subprog_spec (Ada_parse.parse body_name)
  in
    match spec_ast with
      | (_, Spec(_), _) -> spec_ast
      | (_, Body(_), _) -> Npkcontext.error 
	  "Ada_utils.parse_specification" 
	    "specification expected, body found"
   
(* renvoie la spécification normalisée du package correspondant
   à name, les noms étant traités comme extern à la normalisation*)
and parse_extern_specification name =
  let spec_ast = parse_specification name 
  in
    match (normalization spec_ast true) with
      | (_, Spec(spec), loc) -> (spec, loc)
      | (_, Body(_), _) -> Npkcontext.error 
	  "Ada_utils.parse_extern_specification" 
	    "internal error : specification expected, body found"

(* renvoie la spécification du package correspondant à name.
   cette spécification est normalisée *)
and parse_package_specification name =
  match (parse_specification name) with
    | (_, Spec(PackageSpec(name, decls)),_) -> (name, decls)
    | (_, Spec(SubProgramSpec(_)),_) -> 
	Npkcontext.error 
	  "Ada_normalize.parse_package_specification" 
	  ("package specification expected, "
	   ^"subprogram specification found")
    | (_, Body _, _) -> Npkcontext.error 
	   "Ada_utils.parse_package_specification" 
	  "internal error : specification expected, body found"

(* associe tous les identifiants de type à leur déclaration, si 
   elle existe, sinon lance une erreur.
   Recherche les spécifications.
   Transforme également tous les noms de fonctions et de types
   sous la forme
   package.ident (au niveau des déclarations) 
*)
and normalization compil_unit extern =
  let typtbl = Hashtbl.create 100
  and csttbl = Hashtbl.create 100
  and current_package = ref []
  and with_package = ref []
  and context = ref [] in

  (* gestion du contexte *)
  let add_with_package (par,ident) = 
    with_package := (par@[ident])::(!with_package)
  and set_current_package (par,ident) = 
    current_package := par@[ident]
  and raz_current_package _ = 
    current_package := []

  and add_context (select,ident) = 
    (* inverse partiellement la liste, mais tail-rec ?*)
    let rec incr_occurence res l use = match l with
      | (a,n)::r when a=use -> (a, n+1)::res@r
      | c::r -> incr_occurence (c::res) r use
      | [] -> (use,1)::res 
    in 
    let name = select@[ident] in
      if name = !current_package
      then ()
      else 
	begin
	  if (List.mem name !with_package)
	  then 
	    context := incr_occurence [] !context name
	  else 
	    Npkcontext.error "Ada_normalize.add_context"
	      ((string_of_name (select,ident))^" is undefined")
	end
	      
  and remove_context (select,ident) = 
    
    let rec decr_occurence res l use = match l with
      | (a,1)::r when a=use -> res@r
      | (a,n)::r when a=use -> (a,n-1)::res@r
      | c::r -> decr_occurence (c::res) r use
      | [] -> res 
    in 
      context := decr_occurence [] !context (select@[ident])  

  and val_use _ = List.map fst !context in

  (* gestion de la table des constantes *)

  (* ajout d'un nombre ou d'une constante *)
  let add_cst (nom:name) cst global =
    (if Hashtbl.mem csttbl nom
     then
       match Hashtbl.find csttbl nom with
	 | Number(_, glob) | StaticConst(_, _, glob) 
	 | VarSymb(glob)
	 | EnumLitteral(_, _, glob) when global = glob -> 
	     Npkcontext.error
	       "Ada_normalize.add_cst"
	       ("conflict : "^(string_of_name nom)
		^" already declared")
	 | FunSymb(_,ext) when global && ext=extern ->
	      Npkcontext.error
	       "Ada_normalize.add_cst"
	       ("conflict : "^(string_of_name nom)
		^" already declared")
	 | _ -> ());
    Hashtbl.add csttbl nom cst 

  (* ajout d'un littéral d'énumération *)
  and add_enum (nom:name) typ global value =
    (if Hashtbl.mem csttbl nom 
     then
       begin
	 List.iter 
	   (fun x -> match x with
	      | Number(_, glob) | VarSymb(glob)
	      | StaticConst(_, _, glob) when global = glob -> 
		  Npkcontext.error
		    "Ada_normalize.add_enum"
		    ("conflict : "^(string_of_name nom)
		     ^" already declared")
	      | EnumLitteral(t, _, glob) 
		  when typ=t && global = glob ->
		  Npkcontext.error
		    "Ada_normalize.add_enum"
		    ("conflict : "^(string_of_name nom)
		     ^" already declared")
	      | FunSymb(Some(t),ext) when typ=t && global 
		  && ext=extern ->
		  Npkcontext.error
		    "Ada_normalize.add_enum"
		    ("conflict : "^(string_of_name nom)
		     ^" already declared")
	      | _ -> ())
	   (Hashtbl.find_all csttbl nom)
       end);
    Hashtbl.add csttbl nom (EnumLitteral(typ, value, global)) 

  (* ajout d'un symbole de fonction *)
  and add_function nom typ ext =
    (if Hashtbl.mem csttbl nom
     then
       begin
	 List.iter 
	   (fun x -> match x with
	      | Number(_, true) | VarSymb(true)
	      | StaticConst(_, _, true) -> 
		  Npkcontext.error
		    "Ada_normalize.add_function"
		    ("conflict : "^(string_of_name nom)
		     ^" already declared")
	      | EnumLitteral(t, _, true) 
		  when typ=Some(t) ->
		  Npkcontext.error
		    "Ada_normalize.add_function"
		    ("conflict : "^(string_of_name nom)
		     ^" already declared")

	      (* on ignore le cas où deux fonctions ont
		 le même type, pour accepter cas spec+body *)
	      | _ -> ())
	   (Hashtbl.find_all csttbl nom)
       end);
    Hashtbl.add csttbl nom (FunSymb(typ,ext)) 


  and remove_cst ident = Hashtbl.remove csttbl ident in

  (* gestion de la table des types *)
  let mem_typ nom = Hashtbl.mem typtbl nom 
  and find_all_typ nom = Hashtbl.find_all typtbl nom in

  let find_all_use ident = 
    List.flatten
      (List.map
	 (fun pack -> find_all_typ (pack, ident))
	 (val_use ()))
  in

  let add_subtyp (nom:Syntax_ada.name) subtyp location global =
    (if mem_typ nom
     then
       match Hashtbl.find typtbl nom with
	 | (_, _, glob) when global = glob -> 
	     Npkcontext.error
	       "Ada_utils.typ_normalization.add_subtyp"
	       ("conflict : "^(string_of_name nom)
		^" already declared")
	 | _ -> ());
    Hashtbl.add typtbl nom (subtyp,location,global)
      
  and remove_subtyp x = Hashtbl.remove typtbl x in
    
  let add_typ nom (typdecl:Syntax_ada.typ_declaration) location global =
    let subtyp = match typdecl with
      | Enum _ -> Unconstrained(Declared(typdecl, location))
      | IntegerRange(_, contrainte, _) -> 
	  Constrained(Declared(typdecl, location), contrainte,
		      true)
      | DerivedType(_, subtyp_ind) -> 
	  let subtyp = Ada_utils.extract_subtyp subtyp_ind
	  in match subtyp with
	    | Unconstrained(_) -> 
		Unconstrained(Declared(typdecl, location))
	    | Constrained(_, contrainte, static) ->
		Constrained(Declared(typdecl, location),
			    contrainte, static)
	    | SubtypName _ ->
		Npkcontext.error
		  "Ada_normalize.add_typ"
		  "internal error : unexpected subtyp name"
		
    in
      add_subtyp nom subtyp location global
      

  and find_subtyp x = 

    let sans_selecteur ident = 
      if mem_typ x
      then 
	let (decl, loc, _) = Hashtbl.find typtbl x in
	  (decl,loc)
      else 
	begin
	  match find_all_use ident with
	    | [(typ_decl, loc, _)] -> (typ_decl, loc)
	    | [] -> Npkcontext.error 
		"Ada_normalize.typ_normalization.find_subtyp" 
		  ("Unknown identifier "^ident)
	    | _::_ -> Npkcontext.error
		"Firstpass.find_subtyp"
		  (ident^" is not visible : "
		   ^"multiple use clauses cause hiding")
	end
	  
    and avec_selecteur _ =
      try 
	let (decl, loc, _) = Hashtbl.find typtbl x in
	  (decl,loc)
      with Not_found -> 
	Npkcontext.error 
	  "Ada_normalize.normalization.find_typ.avec_selecteur" 
	  ("Unknown identifier "^(string_of_name x))
    
    and selecteur_courant ident = 
      let rec find_global list_ident = match list_ident with
	| (typ_decl, loc, true)::_ -> (typ_decl,loc)
	| (_, _, false)::r -> find_global r 
	| [] -> Npkcontext.error 
	    "Ada_normalize.normalization.find_typ.selecteur_courant" 
	      ("Unknown identifier "^(string_of_name x)) in
	find_global (find_all_typ ident)
    in

	match x with
	  | ([], ident) -> sans_selecteur ident
	  | (pack, _) 
	      when extern||List.mem pack (!with_package) -> 
	      avec_selecteur x
	  | (pack, ident) when pack = !current_package ->
	      selecteur_courant ([],ident)
	  | (pack, _) -> Npkcontext.error 
	      "Ada_normalize.find_typ"
		("unknown package "
		 ^(Print_syntax_ada.ident_list_to_string 
		     pack))
  in

  let normalize_name name = 
    (*let suppr_package (parents, ident) pack = match parents with
      | [] -> ([], ident)
      | a when a=pack -> ([], ident)
      | a when (List.mem a (!with_package)) -> (a, ident)
      | _ -> Npkcontext.error 
	  "ada_normalize.normalize_name.suppr_package"
	    ("unknown package "
	     ^(Print_syntax_ada.ident_list_to_string parents)
	     ^" cur : "
	     ^(Print_syntax_ada.ident_list_to_string pack))*)
    let add_package (parents, ident) pack = match parents with
      | [] -> (pack, ident)
      | a when a=pack -> (pack, ident)
      | a when (List.mem a (!with_package)) -> (a, ident)
      | _ -> Npkcontext.error 
	  "ada_normalize.normalize_name.add_package"
	    ("unknown package "
	     ^(Print_syntax_ada.ident_list_to_string parents))
   
    in
      match extern with
	| false -> name (*suppr_package name (!current_package)*)
	| true -> add_package name (!current_package)
	    (* pas de gestion de with dans package inclus *)

  and normalize_extern_ident ident =
    normalize_extern_ident ident (!current_package) in
  let normalize_ident ident :name = 
    normalize_ident ident (!current_package) extern

  in  
  (*let normalize_typ typ = match typ with
    | Integer | IntegerConst | Float | Boolean | Character 
    | Declared(_) | String -> typ
    | TypName(name) -> 
	let (decl, loc ) = find_typ (normalize_name name) in
	  Declared(decl, loc)*)
  let normalize_subtyp subtyp = match subtyp with
      | Unconstrained(typ) -> Unconstrained(typ) 
      | Constrained(typ,const,static) -> Constrained(typ,const,
						     static)
      | SubtypName(name) -> fst (find_subtyp (normalize_name name))
	    
      
  in
  let rec normalize_exp exp = match exp with
    | Qualified(subtyp, exp) -> Qualified(normalize_subtyp subtyp, 
					  normalize_exp exp)
    | NullExpr | CInt _ | CFloat _ | CBool _ | CChar _
    | CString _ | Var _  -> exp
    | Unary (uop, exp) -> Unary(uop, normalize_exp exp)
    | Binary(bop, e1, e2) -> Binary(bop, normalize_exp e1,
				    normalize_exp e2)
    | FunctionCall(nom, params) ->
	FunctionCall(nom, List.map normalize_exp params)

  in

  (* normalize la contrainte contrainte
     le type des bornes est typ
     static indique si 
     on lance une erreur en cas de borne non-static
     et si on vérifie l'ordre des bornes 
     (autrement dit, on attend une contrainte statique non nulle
     en retour. uniquement utilisé dans le cas entier)
  *)
  let normalize_contrainte contrainte typ static = 
    let eval_range exp1 exp2 = 
      let norm_exp1 = normalize_exp exp1
      and norm_exp2 = normalize_exp exp2 in
	(* on essaye d'évaluer les bornes *)
	(try
	   let (val1,_) = eval_static 
	     norm_exp1 (Some(typ)) csttbl (val_use ()) 
	     !with_package !current_package extern 
	   and (val2,_) = eval_static 
	     norm_exp2 (Some(typ)) csttbl (val_use ()) 
	     !with_package !current_package extern in
	   let contrainte =  match (val1, val2) with
	     | (FloatVal(f1),FloatVal(f2)) -> 
		 if f1<=f2
		 then FloatRangeConstraint(f1, f2)
		 else 
		   Npkcontext.error 
		     "Ada_normalize.normalize_contrainte"
		     "null range not accepted"

	     | (IntVal(i1), IntVal(i2)) -> 
		 if (Nat.compare i1 i2)<=0
		 then IntegerRangeConstraint(i1, i2)
		 else 
		   Npkcontext.error 
		     "Ada_normalize.normalize_contrainte"
		     "null range not accepted"

	     | (EnumVal(i1), EnumVal(i2)) -> 
		 if i1<=i2
		 then 
		   let bounds = (Newspeak.Nat.of_int i1, 
				 Newspeak.Nat.of_int i2) 
		   in IntegerRangeConstraint(bounds)
		 else 
		   Npkcontext.error 
		     "Ada_normalize.normalize_contrainte"
		     "null range not accepted"
		   
	     | (BoolVal(b1), BoolVal(b2)) -> 
		 let i1 = Ada_utils.nat_of_bool b1
		 and i2 = Ada_utils.nat_of_bool b2
		 in
		   if b1 <= b2
		   then IntegerRangeConstraint(i1, i2)
		   else 
		     Npkcontext.error 
		       "Ada_normalize.normalize_contrainte"
		       "null range not accepted"
		     
	     | (_, _) -> 
		 (* ce cas n'est pas censé se produire :
		    on a vérifié que les deux bornes sont de même
		    type.*)
		 Npkcontext.error 
		   "Ada_normalize.normalize_contrainte"
		   ("internal error : range error : expected static "
		    ^"float or integer constant")
	   in contrainte
	 with
	   | NonStaticExpression -> 
	       if static
	       then raise NonStaticExpression
	       else 
		 RangeConstraint(norm_exp1,norm_exp2)

	   | AmbiguousTypeException ->
	       Npkcontext.error 
		 "Ada_normalize.normalize_contrainte"
		 "internal error : uncaught ambiguous type exception")
    in
      match contrainte with
	| RangeConstraint(exp1, exp2) -> 
	    eval_range exp1 exp2
	    
	| IntegerRangeConstraint _ 
	| FloatRangeConstraint _ ->
	    Npkcontext.error
	      "Ada_normalize.eval_contrainte"
	      "internal error : unexpected Numeric Range"
	    


  in
  let normalize_subtyp_indication (subtyp_ref, contrainte, subtyp) =
    (* on établit le sous-type tel qu'il sera utilisé dans 
       le reste du code, à partir du type de base du sous-type
       de référence, de la contrainte normalisée, et d'un
       booléen qui indique si le sous-type de référence est 
       statique*)
    let subtyp_of_constraint contrainte typ static_ref =
      let static_constraint = Ada_utils.constraint_is_static 
	contrainte in
	
      (* Dans le cas de contrainte statique, la contrainte
	 du sous-type résultat reste la même.
	 Dans le cas d'un RangeCosntraint contenant des expressions
	 on génère deux temporaires, qui permettront de se
	 référer aux valeurs des bornes à l'instant de la 
	 déclaration du sous-type.
	 Ces temporaires sont déclarés et initialisés dans
	 firstpass.
      *)
      let contrainte_subtyp_result = match contrainte with 
	| RangeConstraint(_, _) ->
	    let min = normalize_ident (gen_tmp ())
	    and max = normalize_ident (gen_tmp ()) in
	    RangeConstraint(Var(min), Var(max))
	| _ -> contrainte
      in
	(Constrained(typ, contrainte_subtyp_result,
		     static_ref && static_constraint))
    in (match subtyp with
	  | None -> ()
	  | Some(_) -> 
	      Npkcontext.error
		"Ada_normalize.normalize_subtyp_indication"
		"internal error : subtyp already provided");
      
      let norm_subtyp_ref = normalize_subtyp subtyp_ref in 
      let (norm_subtyp, norm_contrainte) = 
	match (contrainte, norm_subtyp_ref) with
	  | (None, Unconstrained(typ)) -> 
	      (Unconstrained(typ), None)
	  | (None, Constrained(typ, const, static)) -> 
	      (Constrained(typ, const, static), None)
	  | (Some(const), Unconstrained(typ)) ->
	      let norm_contrainte = 
		normalize_contrainte const typ false
	      in (subtyp_of_constraint norm_contrainte typ true, 
		  Some(norm_contrainte))
	  | (Some(const), Constrained(typ, const_ref, stat_ref)) ->
	      let norm_contrainte = 
		normalize_contrainte const typ false
	      in
		if not 
		  (Ada_utils.constraint_is_constraint_compatible
		     const_ref norm_contrainte)
		then 
		  Npkcontext.error
		    "Ada_normalize.normalize_subtyp_indication"
	            "constraint error : uncompatible constraint";
		(subtyp_of_constraint norm_contrainte typ stat_ref,
		 Some(norm_contrainte))
	| (_, SubtypName _ ) ->
	    Npkcontext.error
	      "Ada_normalize.normalize_subtyp_indication"
	      "internal error : unexpected subtyp name"
    in
      (norm_subtyp_ref, norm_contrainte, Some(norm_subtyp))

  in
  let rec normalize_instr (instr,loc) = 
    Npkcontext.set_loc loc;
    match instr with
      | NullInstr | ReturnSimple -> (instr, loc)
      | Affect(nom, exp) -> (Affect(nom, normalize_exp exp), loc)
      | Return(exp) -> (Return(normalize_exp exp), loc)
      | If(exp, instr_then, instr_else) -> 
	  (If(normalize_exp exp, normalize_instr_list instr_then,
	      normalize_instr_list instr_else), loc)
      | Loop(NoScheme,instr_list) -> (Loop(NoScheme, 
					   normalize_instr_list 
					     instr_list), loc)
      | Loop(While(exp), instrs) -> 
	  (Loop(While(normalize_exp exp),
		normalize_instr_list instrs), loc)
      | Exit(None) -> (Exit(None), loc)
      | Exit(Some(cond)) -> (Exit(Some(normalize_exp cond)), loc)
      | ProcedureCall(nom, params) -> 
	  (ProcedureCall(nom, List.map normalize_exp params), loc)

  and normalize_instr_list instr_list = 
    List.map normalize_instr instr_list
  in

  let normalize_integer_range ident taille contrainte = 
    match (taille, contrainte) with
      | (None, RangeConstraint(_)) ->
	  begin
	    try
	      let norm_contrainte = 
		normalize_contrainte contrainte IntegerConst true 
	      in match norm_contrainte with
		| IntegerRangeConstraint(min, max) ->
		    let ikind = Ada_utils.ikind_of_range min max 
		    in IntegerRange(ident, norm_contrainte, Some(ikind))
			
		| _ -> 
		    Npkcontext.error
		      "Ada_normalize.normalize_integer_range"
		      "internal error : uncompatible constraint type"
	    with
		NonStaticExpression ->
		  Npkcontext.error
		    "Ada_normalize.normalize_integer_range"
		    "expected static expression"
	  end
      | _ -> 
	  Npkcontext.error
	    "Ada_normalize.normalize_integer_range"
	    "internal error : size or constraint already provided"
	

  in
  let add_extern_typdecl typ_decl loc = match typ_decl with
    | Enum(ident, symbs, _) -> 
	add_typ (normalize_extern_ident ident) typ_decl loc true;
	List.iter
	  (fun (ident,v) -> add_enum (normalize_extern_ident ident)
	     (Declared(typ_decl,loc)) true v)
	  symbs
    | DerivedType(ident, _) -> 
	add_typ (normalize_extern_ident ident) typ_decl loc true
    | IntegerRange(ident,_,_) ->
	add_typ (normalize_extern_ident ident) typ_decl loc true

  and normalize_typ_decl typ_decl loc global = match typ_decl with
    | Enum(ident, symbs, _) -> 
	add_typ (normalize_ident ident) typ_decl loc global;
	List.iter
	  (fun (ident,v) -> add_enum (normalize_ident ident)
	     (Declared(typ_decl,loc)) global v)
	  symbs;
	typ_decl
    | DerivedType(ident, subtyp_ind) -> 
	let norm_subtyp_ind = 
	  normalize_subtyp_indication subtyp_ind in
	let normtyp = DerivedType(ident, norm_subtyp_ind)
	in 
	  add_typ (normalize_ident ident) normtyp loc global;
	  normtyp
    | IntegerRange(ident,contrainte,taille) ->
	let decl = normalize_integer_range ident taille contrainte
	in
	  add_typ (normalize_ident ident) decl loc global;
	  decl
  and remove_typ_decl typ_decl = match typ_decl with
    | Enum(nom, symbs, _) -> remove_subtyp (normalize_ident nom);
	List.iter
	  (fun (symb, _) -> remove_cst (normalize_ident symb))
	  symbs
    | DerivedType(nom,_) -> remove_subtyp (normalize_ident nom)
    | IntegerRange(nom,_,_) -> remove_subtyp (normalize_ident nom)
  in

  let normalize_sub_program_spec subprog_spec addparam =
    let normalize_params param_list func = 
      List.map 
	(fun param -> 
	   if func && (param.mode <> In)
	   then (Npkcontext.error 
	     "Ada_normalize.normalize_sub_program_spec"
	     ("unvalid parameter mode : functions can only have"
	      ^" \"in\" parameters"))
	   else
	     (if addparam 
	      then 
		List.iter 
		  (fun x -> add_cst (normalize_ident x) 
		     (VarSymb(false)) false)
		  param.pnom;
	      {param with ptype = normalize_subtyp param.ptype})) 
	param_list
    in
      match subprog_spec with

	| Function(name, [], return_type)  ->
	    let norm_name = normalize_name name
	    and norm_subtyp = normalize_subtyp return_type in
	      add_function 
		norm_name (Some(base_typ norm_subtyp)) false;
	      Function(norm_name, [], norm_subtyp)
		
	| Function(name,param_list,return_type) ->
	    let norm_name = normalize_name name in
	      add_function norm_name None false;
	      Function(norm_name, 
		       normalize_params param_list true, 
		       normalize_subtyp return_type)	
	      
	| Procedure(name,param_list) ->
	    let norm_name = normalize_name name in
	      add_function norm_name None false;
	      Procedure(norm_name, 
			normalize_params param_list false)
  in
  let rec normalize_basic_decl item loc global = match item with
    | UseDecl(use_clause) -> List.iter add_context use_clause;
	item
    | ObjectDecl(ident_list,subtyp_ind,def, Variable) ->
	let norm_subtyp_ind =
	  normalize_subtyp_indication subtyp_ind in
	  (List.iter
	     (fun x -> add_cst (normalize_ident x)
		(VarSymb(global)) global)
	     ident_list);
	  ObjectDecl(ident_list, norm_subtyp_ind, def, Variable)
	  
    | ObjectDecl(ident_list,subtyp_ind, Some(exp), Constant) -> 
	(* constantes *)
	let norm_subtyp_ind = 
	  normalize_subtyp_indication subtyp_ind in
	let subtyp = Ada_utils.extract_subtyp norm_subtyp_ind in
	let typ = base_typ subtyp in
	let add_ident v x = add_cst (normalize_ident x)
	  (StaticConst(v, typ, global)) global in
	let status = 
	  try
	    let (v,_) = eval_static exp (Some(typ)) csttbl 
	      (val_use ()) !with_package !current_package extern in
	      
	      (* on vérifie que la valeur obtenue est conforme
		 au sous-type *)
	      Ada_utils.check_static_subtyp subtyp v;	
	      List.iter (add_ident v) ident_list;
	      StaticVal(v)
	  with
	    | AmbiguousTypeException ->
		Npkcontext.error 
		  "Ada_normalize.normalize_basic_decl"
		  "uncaught ambiguous type exception"
	    | NonStaticExpression -> 
		(List.iter
		   (fun x -> add_cst (normalize_ident x)
		      (VarSymb(global)) global)
		   ident_list);
		Constant 
		  (*la constante n'est pas statique *)
		  
	in
	  ObjectDecl(ident_list, norm_subtyp_ind, Some(exp),status)
	    
    | ObjectDecl(_) ->
	Npkcontext.error 
	  "Ada_normalize.normalize_basic_decl"
	  ("internal error : constant without default value"
	   ^"or already evaluated")
	  
    | TypeDecl(typ_decl) -> 
	let norm_typ_decl = normalize_typ_decl typ_decl loc global
	in TypeDecl(norm_typ_decl)
	  
    | SpecDecl(spec) -> SpecDecl(normalize_spec spec)
	
    | NumberDecl(ident_list, exp, None) ->
	let norm_exp = normalize_exp exp in
	let v = eval_static_number norm_exp csttbl (val_use ()) 
	  !with_package !current_package extern in
	  (*ajouts dans la table*)
	  List.iter
	    (fun ident -> add_cst (normalize_ident ident) 
	       (Number(v, global)) global)
	    ident_list;
	  NumberDecl(ident_list, norm_exp, Some(v))
	    
    | NumberDecl(ident, exp, Some(v)) ->
	(* cas jamais emprunté *)
	NumberDecl(ident, normalize_exp exp, Some(v))
	  
    | SubtypDecl(ident, subtyp_ind) ->
	let norm_subtyp_ind = 
	  normalize_subtyp_indication subtyp_ind in 
	let subtyp = Ada_utils.extract_subtyp norm_subtyp_ind in
	  add_subtyp (normalize_ident ident) subtyp loc global;
	  SubtypDecl(ident, norm_subtyp_ind)
    | RepresentClause _ -> item	        
  
  and normalize_decl_part decl_part global = 
    let rec normalize_decl_items items = 
      match items with
	| (BasicDecl(basic),loc)::r -> 
	    Npkcontext.set_loc loc;
	    let decl = normalize_basic_decl basic loc global 
	    in (BasicDecl(decl),loc)::(normalize_decl_items r)
	    
	| (BodyDecl(body),loc)::r -> 
	    Npkcontext.set_loc loc;
	    let norm_item = (BodyDecl(normalize_body body), loc)
	    in norm_item::(normalize_decl_items r)

	| [] -> []
    in normalize_decl_items decl_part

  and remove_decl_part decl_part = 

    (* incomplet *)
    let remove_decl_item (item,_) = match item with
      | BasicDecl(TypeDecl(typ_decl)) -> 
	  remove_typ_decl typ_decl
      | BasicDecl (SubtypDecl (ident, _)) ->
	  remove_subtyp (normalize_ident ident)
      | BasicDecl(ObjectDecl(ident_list,_, _, _)) -> 
	  List.iter
	    (fun ident -> remove_cst (normalize_ident ident))
	    ident_list

      | BasicDecl(UseDecl(use_clause)) -> 
	  List.iter remove_context use_clause

      | BasicDecl(SpecDecl(_)) -> () (* pas de déclaration de type 
					dans spec package/fonc *)
      | BodyDecl(_) -> () (* rien à supprimer pour un corps,
			     les déclarations internes sont
			     supprimées lors du traitement
			     du corps *)
      | BasicDecl(NumberDecl(idents,_,_)) -> 
	  List.iter 
	    (fun x -> remove_cst (normalize_ident x)) 
	    idents

      | BasicDecl(RepresentClause _) -> ()

    in List.iter remove_decl_item decl_part

  and remove_params subprogram_decl = 
    let params = match subprogram_decl with
      | Function(_,param_list,_) -> param_list
      | Procedure(_,param_list) -> param_list in
      List.iter
	(fun param ->
	   List.iter
	     (fun x -> remove_cst (normalize_ident x))
	     param.pnom)
	params      
    
	 
  and normalize_package_spec (nom, list_decl) = 
    set_current_package nom;
    let rec normalize_decls decls = match decls with
      |	(decl, loc)::r -> 
	  Npkcontext.set_loc loc;
	  let decl = normalize_basic_decl decl loc true
	  in (decl,loc)::(normalize_decls r)
      | [] -> [] in
    let norm_spec = normalize_decls list_decl in
      raz_current_package ();
      (nom,norm_spec)
    

  and normalize_spec spec = match spec with
    | SubProgramSpec(subprogr_spec) -> 
	SubProgramSpec(
	  normalize_sub_program_spec subprogr_spec false)
    | PackageSpec(package_spec) ->
	PackageSpec(normalize_package_spec package_spec)
  

  and normalize_body body  = match body with
    | SubProgramBody(subprog_decl,decl_part,instr_list) ->
	
	let norm_subprog_decl = 
	  normalize_sub_program_spec subprog_decl true
	and norm_decl_part = normalize_decl_part decl_part false in
	let norm_instr_list = normalize_instr_list instr_list
	in 
	  remove_decl_part decl_part;
	  remove_params subprog_decl;
	  SubProgramBody(norm_subprog_decl,norm_decl_part,
			 norm_instr_list)
	    
    | PackageBody(name, package_spec, decl_part, instr_list) -> 
	let norm_decl = match package_spec with
	  | None -> 
	      let package_spec = parse_package_specification name
	      in 
		Some(normalize_package_spec package_spec)
	  | Some(spec) -> Some(normalize_package_spec spec)
	in
	  set_current_package name;
	  let norm_decl_part = normalize_decl_part decl_part true in
	  let norm_instr_list = normalize_instr_list instr_list
	  in
	    remove_decl_part decl_part;
	    raz_current_package ();
	    PackageBody(name, norm_decl, norm_decl_part, 
			norm_instr_list)
	    

  in
  let normalize_lib_item lib_item loc =
    Npkcontext.set_loc loc;
    match lib_item with
      | Spec(spec) -> Spec(normalize_spec spec)
      | Body(body) -> Body(normalize_body body)

  in 

  (* ajoute toutes les déclarations contenues dans la
     spec, sans normalisation (puisque déjà normalisé). 
     Ajoute également le nom du package
     à la liste de package accessible. *)
(* TODO *)
  let add_extern_spec spec =
    let add_extern_basic_decl (basic_decl, loc) =
      Npkcontext.set_loc loc;
      match basic_decl with
	| TypeDecl(typ_decl) ->
	    add_extern_typdecl typ_decl loc
	| ObjectDecl(ident_list, _, _, 
		     (Variable | Constant)) ->
	    (List.iter
	       (fun x -> add_cst (normalize_extern_ident x)
		  (VarSymb(true)) true)
	       ident_list)
	      
	| ObjectDecl(ident_list,subtyp_ind, _, StaticVal(v)) -> 
	    (* constante statique *)
	    let typ = base_typ 
	      (Ada_utils.extract_subtyp subtyp_ind) in
	      List.iter
		(fun x -> add_cst (normalize_extern_ident x)
		   (StaticConst(v, typ, true)) true)
		ident_list
		
	| NumberDecl(ident_list, _, Some(v)) ->
	    (*ajouts dans la table*)
	    List.iter
	      (fun ident -> add_cst 
		 (normalize_extern_ident ident) 
		 (Number(v, true)) true)
	      ident_list;
	    
	| NumberDecl(_, _, None) ->
	    Npkcontext.error
	      "Ada_normalize.add_extern_spec.add_extern_basic_decl"
	      "internal error : external number declaration without value"	
	| SpecDecl(SubProgramSpec
		     (Function(name, [], return_typ))) ->
	    add_function name (Some(base_typ return_typ)) true
	| SpecDecl(SubProgramSpec(Function(name, _, _) |
				      Procedure(name, _))) ->
	    add_function name None true
	| SubtypDecl(ident, subtyp_ind) -> 
	    add_subtyp (normalize_extern_ident ident) 
	      (Ada_utils.extract_subtyp subtyp_ind)
	      loc true	 
	| SpecDecl _ -> ()
	| UseDecl _ -> ()
	| RepresentClause _ -> ()

    in match spec with
      | SubProgramSpec(Function(name, [], return_typ)) ->
	  add_function name (Some(base_typ return_typ)) true
      | SubProgramSpec(Function(name, _, _)|Procedure(name, _)) ->
	  add_function name None true
	    
      | PackageSpec(nom, basic_decls) -> 
	  set_current_package nom;
	  List.iter add_extern_basic_decl basic_decls;
	  raz_current_package ();
	  add_with_package nom
	
  in

  (* normalise le context, en supprimant les doublons with *)
  let rec normalize_context context previous_with = 
    match context with
      | With(nom, _, _)::r when (List.mem nom previous_with) ->
	  (* doublon *)
	  normalize_context r previous_with
      | With(nom, _, spec)::r ->
	  
	  let (norm_spec, loc) = match spec with
	    | None -> parse_extern_specification nom
	    | Some(_) -> Npkcontext.error 
		"Ada_normalize.normalize_context" 
		  "internal error : spec provided"
	  in
	    add_extern_spec norm_spec;
	    (With(nom, loc, Some(norm_spec, loc)))
	    ::(normalize_context r (nom::previous_with))
      | UseContext(name_list)::r -> 
	  List.iter add_context name_list;
	  (UseContext(name_list))::(normalize_context r previous_with)
      | [] -> []
  in

  let (context,lib_item,loc) = compil_unit
  in
  let norm_context = normalize_context context []
  in
  let norm_lib_item = normalize_lib_item lib_item loc
  in
    Npkcontext.forget_loc ();
    (norm_context, norm_lib_item, loc)
