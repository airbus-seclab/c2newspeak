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
  email: jasmine . duchon AT free . fr

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org
*)

open Syntax_ada

module C = Cir
module K = Npkil
module Nat = Newspeak.Nat
module Npk = Newspeak

exception AmbiguousTypeException

(* types *)

(* un symbol peut représenter une variable ou un littéral
   d'énumération.
   le paramètre booléen indique :
   - si la variable est global ou
    local (true = global) dans le cas var et enum,
   - si la fonction/procedure est interne ou externe dans le cas 
     fun (extern=true)
   dans le cas des variables : le dernier param booléen
   indique si la variable est en lecture seule
*)
type symb =
  | VarSymb of C.lv*Syntax_ada.subtyp*bool*bool
  | EnumSymb of C.exp*Syntax_ada.typ*bool
  | FunSymb of C.funexp*Syntax_ada.sub_program_spec*bool*C.ftyp
  | NumberSymb of value*bool

(* Constants *)
let ret_ident = "!return"

let ret_lbl = 0
let cnt_lbl = 1
let brk_lbl = 2
let default_lbl = 3

let ident_to_name ident = ([], ident)
let string_of_name = Print_syntax_ada.name_to_string

let eq_base_typ = Ada_utils.eq_base_typ
let base_typ = Ada_utils.base_typ

let extract_scalar_typ cir_typ = match cir_typ with
  | C.Scalar(t) -> t
  | _ -> 
      Npkcontext.error
	"Firstpass.extract_scalar_typ"
	"type isn't a scalar type"




let make_check_constraint contrainte exp = 
  match contrainte with
    | IntegerRangeConstraint (v1,v2) ->
	C.Unop(K.Belongs_tmp(v1, K.Known (Nat.add v2 Nat.one)), exp)
    | FloatRangeConstraint(_, _) -> exp
	
    | RangeConstraint _ ->
	Npkcontext.error
	  "Firstpass.make_check_constraint"
	  "internal error : unexpected range constraint (non-static)"
	  
let make_check_subtyp subtyp exp = 
  match subtyp with
    | Unconstrained _ -> exp
    | Constrained(_, contrainte, _) ->
	make_check_constraint contrainte exp
    | SubtypName _ ->
	Npkcontext.error
	  "Firstpass.make_check_subtyp"
	  "internal error : unexpected subtyp name"
	  
let make_offset styp exp size = 
  match styp with 
      Constrained( _, x , _ ) ->
	begin match x with
	    IntegerRangeConstraint(nat1, _) -> 
	      let borne_inf =   C.Const(C.CInt(nat1)) in 
	      
	      let decal =  C.Binop (Npk.MinusI, exp, borne_inf) in
		
		C.Binop (Newspeak.MultI, decal,  size)
	 
	  |  _ -> Npkcontext.error "Firstpass.make_offset"
	       "contrainte (not IntegerRangeConstraint) not coded yet "
		
	end
	  
    | Unconstrained _ -> exp 
	
    | SubtypName _ -> Npkcontext.error "Firstpass.make_offset"
	"SubtypName not implemented yet (especially for Enum)"
	  
	  
let translate compil_unit =
  
  (* Références globales *)
  let symbtbl = Hashtbl.create 100
  and tmp_cnt = ref 0
  and fun_decls = Hashtbl.create 100
  and globals = Hashtbl.create 100 

  and context = ref []
  and current_package = ref []
  and extern = ref false
  and with_package = ref []
  in

  let set_current_package (par, ident) = 
    current_package := par@[ident]
  and raz_current_package _ = 
    current_package := []
  and add_with_package (par, ident) =
    with_package := (par@[ident])::(!with_package)

  and add_context (select,ident) = 

    (* inverse partiellement la liste, mais tail-rec ?*)
    let rec incr_occurence res l use = match l with
      | (a,n)::r when a=use -> (a, n+1)::res@r
      | c::r -> incr_occurence (c::res) r use
      | [] -> (use,1)::res 
    in 
      context := incr_occurence [] !context (select@[ident])  
	
  and remove_context (select,ident) = 
    
    let rec decr_occurence res l use = match l with
      | (a,1)::r when a=use -> res@r
      | (a,n)::r when a=use -> (a,n-1)::res@r
      | c::r -> decr_occurence (c::res) r use
      | [] -> res 
    in 
      context := decr_occurence [] !context (select@[ident])  

  and val_use _ = List.map fst !context
  in

   
  let find_symb x = Hashtbl.find symbtbl x

  and find_all_symb x = Hashtbl.find_all symbtbl x

  and mem_symb x = Hashtbl.mem symbtbl x in 

  let find_all_use ident = 
    List.flatten
      (List.map
	 (fun pack -> find_all_symb (pack, ident))
	 (val_use ()))

	 
  in
  let find_name name f_ident f_with f_current = 
    match name with
      | ([], ident) -> f_ident ident name
      | (pack, ident)
	  when !extern||List.mem pack (!with_package) -> 
	  f_with (pack,ident)
      | (pack, ident) when pack = !current_package ->
	  f_current ([],ident) name
      | (pack, _) -> Npkcontext.error 
	  "Firstpass.find_name"
	    ("unknown package "
	     ^(Print_syntax_ada.ident_list_to_string 
		 pack))
  in
    
  (* fonction de traduction des types *)  
  let rec translate_typ typ = match typ with
    | Integer -> C.Scalar(Npk.Int(Npk.Signed, Ada_config.size_of_int))
    | Float -> C.Scalar(Npk.Float(Ada_config.size_of_float))
    | Boolean -> C.Scalar(Npk.Int(Npk.Unsigned, Ada_config.size_of_boolean))
    | Character -> C.Scalar(Npk.Int(Npk.Unsigned, Ada_config.size_of_char))
    | Declared(typ_decl, _) -> translate_declared typ_decl
    | IntegerConst -> C.Scalar(Npk.Int(Npk.Signed, Ada_config.size_of_int))
    | String -> Npkcontext.error "Firstpass.translate_typ"
	"String not implemented"
  and translate_declared typ_decl = match typ_decl with
    | Enum(_, _, bits) -> C.Scalar(Npk.Int(bits))
    | DerivedType(_, subtyp_ind) -> translate_typ 
	(Ada_utils.extract_typ subtyp_ind)
    | IntegerRange(_,_,Some(bits)) -> C.Scalar(Npk.Int(bits))
    | Array(_, ConstrainedArray(_, subtyp_ind, taille)) ->
	C.Array(translate_typ (Ada_utils.extract_typ subtyp_ind), taille)    
    | IntegerRange(_,_,None) -> Npkcontext.error 
	"Firstpass.translate_declared"
	  "internal error : no bounds provided for IntegerRange"
  and translate_subtyp subtyp = translate_typ (base_typ subtyp)

  and translate_int i = C.Const(C.CInt(i))
  in

  (* fonctions de traduction des noms*)
  (* on ajoute le nom du package courant dans le cas où on
     étudit les déclarations internes *)
  (* fonction appelée dans add_fundecl, add_funbody, add_global *)
  let translate_name name = 
    let tr_name = match (!extern, name) with
      | (true,_) -> name
      | (false,(_,ident)) -> (!current_package,ident)
    in
      Print_syntax_ada.name_to_string tr_name in
    
  (* gestion de la table de symboles *)
    
  (* déclaration d'une variable locale *)
  let add_var loc (st:Syntax_ada.subtyp) ident deref ro =
    let x = ident_to_name ident in
      (if Hashtbl.mem symbtbl x
       then 
	 match Hashtbl.find symbtbl x with
	     
	   (* si la déclaration précédente est global, on peut
	      redéfinir x*)
	   | (VarSymb(_, _, global,_),_,_)   
	   | (NumberSymb(_, global),_,_) 
	   | (EnumSymb(_,_, global),_,_) when global -> ()
	   | (FunSymb _,_,_) -> ()
	       
	   (* sinon la déclaration est local, 
	      cela cause une erreur *)
	   | ((VarSymb(_)|EnumSymb(_)|NumberSymb (_)),_,_)-> 
	       Npkcontext.error "Firstpass.add_var"
		 ("conflict : "^(string_of_name x)
		  ^" already declared")
      );
      let id = C.fresh_id () in
      let tr_typ = translate_typ (base_typ st) in
      let (lv,typ_cir) = 
	if deref
	then (C.Deref(C.Lval(C.Var(id), tr_typ), tr_typ), C.Scalar(Npk.Ptr))
	else (C.Var(id), tr_typ)
      in
	Hashtbl.add symbtbl x (VarSymb (lv, st, false, ro), 
			       typ_cir, loc);
	id
	  
  (* déclaration d'un nombre local *)
  and add_number loc value lvl ident =
    let x =  Ada_normalize.normalize_ident 
      ident !current_package !extern in
      (if Hashtbl.mem symbtbl x
       then 
	 match Hashtbl.find symbtbl x with
	     
	   (* erreur : déclaration de même niveau déjà existante *)
	   | (VarSymb(_, _, global,_),_,_) 
	   | (EnumSymb(_,_, global),_,_)
	   | (NumberSymb(_,global),_,_) when global=lvl -> 
	       Npkcontext.error "Firstpass.add_number"
		 ("conflict : "^(string_of_name x)
		  ^" already declared")
	   | (FunSymb _,_,_) when lvl -> 
	       Npkcontext.error "Firstpass.add_number"
		 ("conflict : "^(string_of_name x)
		  ^" already declared")
		 
	   (* les déclarations ne sont pas de même niveau *)
	   | ((VarSymb(_) | EnumSymb(_) 
	      | NumberSymb(_) | FunSymb _),_,_) -> ()
      );
      let typ_cir = match value with
	| IntVal _ -> translate_typ IntegerConst
	| FloatVal _ -> translate_typ Float
	| BoolVal _ -> 
	    Npkcontext.error
	      "Firstpass.add_number"
	      "internal error : number cannot have Enum val"
      in
	Hashtbl.add symbtbl x 
	  (NumberSymb(value, lvl), typ_cir, loc)



  (* déclaration d'un symbole d'énumération *)
  and add_enum loc ident value typ global =
    let name = Ada_normalize.normalize_ident 
      ident !current_package !extern in
      (if mem_symb name
       then 
	 List.iter
	   (fun x -> match x with
		(* erreur : il y a une déclaration de variable
		   ou de nombre de même niveau que la déclaration
		   courante *)
	      | ((VarSymb(_, _, global',_) | NumberSymb(_,global')),
		 _,_) when global=global' -> 
		  Npkcontext.error "Firstpass.add_enum"
		    ("conflict : "^ident^" already declared")
		 
	      (* erreur : déclaration d'un symbol d'énumération
		 de même type et de même niveau *)
	      | (EnumSymb(_, t, global'),_,_) 
		  when t=typ && global=global' ->
		  Npkcontext.error "Firstpass.add_enum"
		    ("conflict : "^ident^" already declared")
		    
	      (* il existe une fonction interne sans argument 
		 ayant le même type de retour que le symbole 
		 d'énumération, qui est global *)
	      | (FunSymb (_, Function(_, [], st), false, _), _, _) 
		  when global && (base_typ st)=typ ->
		  Npkcontext.error "Firstpass.add_enum"
		    ("conflict : "^ident^" already declared")
		 
	      | ((EnumSymb(_)|VarSymb(_)
		 |FunSymb _ |NumberSymb(_)),_,_) -> ())
	   (find_all_symb name));
      Hashtbl.add symbtbl 
	name
	(EnumSymb(translate_int value, typ, 
		  global), translate_typ typ, loc)
      
  and add_global loc typ tr_typ tr_init ro x = 
    let name = Ada_normalize.normalize_ident 
      x !current_package !extern in
      
    let tr_name = translate_name name in
      (if mem_symb name
       then 
	 (List.iter
	    (fun symb -> match symb with
		 (* il y a une déclaration de variable
		    ou de nombre globale *)
	       | (NumberSymb(_, true),_,_) 
	       | (VarSymb(_, _, true,_),_,_) -> 
		   Npkcontext.error "Firstpass.add_global"
		     ("conflict : "^x^" already declared")
		    
	       (* déclaration d'un symbol d'énumération global *)
	       | (EnumSymb(_, _, true),_,_) -> 
		   Npkcontext.error "Firstpass.add_global"
		     ("conflict : "^x^" already declared")
		    
	       (* fonction interne *)
	       | (FunSymb (_, Function(_, _, _), false, _), _, _) ->
		   Npkcontext.error "Firstpass.add_enum"
		     ("conflict : "^x^" already declared")
		     
	       | ((EnumSymb _ |VarSymb(_)
		  | FunSymb _ |NumberSymb(_)),_,_) -> ())
	    (find_all_symb name)));
      
      Hashtbl.add globals tr_name (tr_typ, loc, tr_init);
      Hashtbl.add symbtbl name
	(VarSymb (C.Global(tr_name), typ, true, ro), 
	 tr_typ, loc)
	
  and remove_symb x = Hashtbl.remove symbtbl (ident_to_name x) in
   
  let remove_formals args =
    remove_symb ret_ident;
    List.iter remove_symb args
  in

  (* génération de temp *)
  let gen_tmp loc t =
    let x = "tmp"^(string_of_int !tmp_cnt) in
    let id = add_var loc (Unconstrained(t)) x false false in
    let t = translate_typ t in
    let decl = (C.Decl (t, x, id), loc) in
      incr tmp_cnt;
      (x, decl, C.Var id)
  in

  (* fonctions pour la gestion des types *)
    
  let integer_class = Ada_utils.integer_class
  and  check_typ = Ada_utils.check_typ  
  and known_compatible_typ = Ada_utils.known_compatible_typ in    

  (* recherche d'un symbol de fonction : à revoir *)
  let find_fun_symb name = 

    let rec mem_other_symb list_ident var_masque = 
      match list_ident with
	| ((VarSymb(_)|NumberSymb(_)),_,_)::r when var_masque -> 
	    mem_other_symb r var_masque
	| ((VarSymb(_)|NumberSymb(_)),_,_)::_ -> 
	    Npkcontext.error 
	      "Firstpass.find_fun_symb" 
	      ((Print_syntax_ada.name_to_string name)
	       ^" is not a funtion")
	| (EnumSymb(_),_,_)::r -> mem_other_symb r var_masque
	| (FunSymb _,_,_)::_ -> true
	| [] -> false
    in
    let sans_selecteur ident name =
      let list_symb = find_all_symb name in
      
      let rec find_use list_symb var_masque =
	match list_symb with
	  | ((VarSymb(_)|NumberSymb(_)),_,_)::r when var_masque -> 
	      find_use r var_masque
		
	  | ((VarSymb(_)|NumberSymb(_)),_,_)::_ -> (*WG TO DO *)
	      Npkcontext.error 
		"Firstpass.find_fun_symb" 
		(ident ^" is not a funtion 1 ")
		
	  | (EnumSymb(_),_,_)::r ->
	      find_use r var_masque

	  | (FunSymb (_(*fn*), _(*sp*), true, _), C.Fun, _)::r -> 
	      if (mem_other_symb r var_masque)
	      then (Npkcontext.error
		      "Firstpass.find_fun_symb"
		      (ident^" is not visible : "
		       ^"multiple use clauses cause hiding"))
	      else (*fn, sp, trt*)
	(*	print_endline ("found --------------"^(match fn with C.Fname f -> f | _ -> "unkno"));
	*)
		List.hd list_symb
		

	  | (FunSymb (_, _, false, _), C.Fun, _)::_ -> Npkcontext.error
	      "Firstpass.find_fun_symb" 
		("internal error : imported function not "
		 ^"tagged as extern")

	  | (FunSymb _, _, _)::_ -> Npkcontext.error 
	      "Firstpass.find_fun_symb" 
		("internal error : translate type isn't "
		 ^"a fun type")

	  | [] -> Npkcontext.error 
	      "Firstpass.find_fun_symb"
		("cannot find symbol "^ident)
      in
      let rec find_interne list_symb var_masque = 
	match list_symb with
	  | ((VarSymb(_,_,true,_)|NumberSymb(_,true)),
	     _,_)::r when var_masque -> 
	      find_interne r var_masque

	  | (VarSymb(_),_,_)::_ -> 
	      (*WG*)
	      List.hd list_symb
		
	  | (NumberSymb(_),_,_)::_ -> 
	      Npkcontext.error 
		"Firstpass.find_fun_symb" 
		((Print_syntax_ada.name_to_string name)
		 ^" is not a funtion (NumberSymb)")
	      
	  | (EnumSymb(_),_,_)::r ->
	      find_interne r true

	  | (FunSymb _, C.Fun, _)::_ -> 
	      List.hd list_symb

	  | (FunSymb _, _,_)::_ -> 
	      Npkcontext.error "Firstpass.find_fun_symb" 
		("internal error : translate type isn't a fun type")

	  | [] -> find_use (find_all_use ident) var_masque

      in find_interne list_symb false
	      
    and avec_selecteur name =
      let list_symb = find_all_symb name in
      let rec find_fun list_symb = 
	match list_symb with
	  | ((VarSymb(_)|NumberSymb(_)),_,_)::_ ->  (*WG TO DO *)
	      Npkcontext.error 
	       "Firstpass.find_fun_symb" 
		((Print_syntax_ada.name_to_string name)
		 ^" is not a funtion")

	  | (EnumSymb(_),_,_)::r ->
	      find_fun r

	  | (FunSymb (_, _, true, _), C.Fun, _)::_ -> 
	      List.hd list_symb

	  | (FunSymb _, _, _)::_ -> Npkcontext.error 
	      "Firstpass.find_fun_symb" 
		("internal error : translate type isn't "
		 ^"a fun type")

	  | [] -> Npkcontext.error 
	      "Firstpass.find_fun_symb"
		("cannot find symbol "^(string_of_name name))
      in find_fun list_symb

    and avec_selecteur_courant ident name = 
      let list_symb = find_all_symb ident in
	let rec find_global list_symb = 
	match list_symb with
	  | ((VarSymb(_,_,false,_)|NumberSymb(_,false)),_,_)::r ->
	      find_global r

	  | ((VarSymb(_,_,true,_)|NumberSymb( _,true)),_,_)::_ ->(*WG TO DO *)
	      Npkcontext.error 
	       "Firstpass.find_fun_symb" 
		((Print_syntax_ada.name_to_string name)
		 ^" is not a funtion")
 
	  | (EnumSymb(_),_,_)::r ->
	      find_global r

	  | (FunSymb (_, _, false, _), C.Fun, _)::_ -> 
	      List.hd list_symb

	  | (FunSymb (_,_, true, _), _, _)::_ -> (* fonction externe *)
	      Npkcontext.error 
		"Firstpass.find_fun_symb"
		("cannot find symbol "^(string_of_name name))

	  | (FunSymb _, _, _)::_ -> 
	      Npkcontext.error "Firstpass.find_fun_symb" 
		("internal error : translate type isn't a fun type")

	  | [] -> Npkcontext.error 
	      "Firstpass.find_fun_symb"
		("cannot find symbol "^(string_of_name name))
      in find_global list_symb

    in find_name name sans_selecteur 
	 avec_selecteur avec_selecteur_courant

  in

  (* fonctions de traductions *)
  let translate_subtyp_option subtyp = match subtyp with
    | None -> C.Void
    | Some(subtyp) -> translate_typ (base_typ subtyp)
  
  in
  let translate_number v expected_typ = match v with
      | IntVal(i) ->
	  let t = check_typ expected_typ IntegerConst
	  in (translate_int i, t)
      | FloatVal(f,s) -> 
          let t = check_typ expected_typ Float 
	  in (C.Const(C.CFloat(f,s)), t) 
      | BoolVal _ ->
	  Npkcontext.error
	    "Firstpass.translate_number"
	    "internal error : number cannot have enum val"
  in
    
  let rec translate_lv lv write trans_exp = 
    (*cas d'un symbol sans sélecteur *)
    let fun_sans_sel ident name = 
      if mem_symb name then find_symb name 
      else begin
	let all_symbs = find_all_use ident in 
	  match all_symbs with
	    | [a] -> a
	    | [] -> Npkcontext.error 
		"Firstpass.translate_lv"
		  ("cannot find symbol "^ident)  
	    | _ -> Npkcontext.error 
		"Firstpass.translate_lv"
		  ("multiple use clause :"
		   ^ident^"is not visible") 
            end
    in
  
    (* cas d'un symbol avec sélecteur connu *)
    let fun_sel_connu name =       
      if mem_symb name then find_symb name
      else Npkcontext.error "Firstpass.translate_lv"
	("cannot find symbol "^(string_of_name name))
    in
      
    (*cas d'un symbol avec sélecteur qui est le package courant*)
    let fun_ident_name ident name = 
      if mem_symb ident then 
	try 
	  List.find
	    (fun (symb, _, _) -> match symb with
	       | VarSymb(_,_,true,false) -> true
	       | _ -> false)
	    (find_all_symb ident)
	with
	  | Not_found -> Npkcontext.error "Firstpass.translate_lv"
	      ("cannot find symbol "^(string_of_name name))
      else  
	Npkcontext.error "Firstpass.translate_lv"
	  ("cannot find symbol "^(string_of_name name))
    in
      
      match lv with 
	  Lval lv -> 
	    let (symb, _, _) = find_name lv
	      fun_sans_sel   (*sans sélecteur *)
	      fun_sel_connu  (*avec sélecteur connu *)
	      fun_ident_name (*avec sélecteur =  pkg courant*)
	      
	    in begin
	      match symb with
		| VarSymb(_,_,_, true) when write ->
		    Npkcontext.error "Firstpass.translate_lv"
		      ("Invalid left value : "^(translate_name lv)
		       ^" is read only")
		      
		| VarSymb(v, typ, _, _) -> (v, typ)
		| NumberSymb(_) ->
		    Npkcontext.error "Firstpass.translate_lv"
		      "Invalid left value: unexpected number symbol"
		| FunSymb _ -> Npkcontext.error "Firstpass.translate_lv"
		    "Invalid left value: unexpected function symbol"
		| EnumSymb(_) -> Npkcontext.error "Firstpass.translate_lv"
		    "Invalid left value: unexpected enum" 
	      end

	(*Affectation dans un tableau*)
	| ArrayAccess (lval, expr) ->
	    let (v, subtyp_lv) = translate_lv lval write trans_exp in 
	      match  subtyp_lv 
	      with
		  Unconstrained(Declared( Array(_,
		    ConstrainedArray(( stypindex, contraint,_ ),
				     ( stypelt,_,_),  _)), _)) ->
		    let size_base =  C.exp_of_int (C.size_of (
				(translate_typ (base_typ stypelt)))
						             ) 
		    in
		    let (exp,_) = trans_exp expr
		      (Some(base_typ(stypindex))) 
		    in
		      
		      
		    let new_constr  = 
		      match contraint with 
			  None -> begin match stypindex with 
			    | Constrained(_, contr, _) -> contr 
			    | Unconstrained _ 
			    | SubtypName _ ->
				Npkcontext.error
				  "Firstpass Array Access"
				  "Unconstrained or SubtypName" 
			  end
			    
			| Some(RangeConstraint(CInt(a), CInt(b))) 
			    
			| Some(IntegerRangeConstraint(a, b)) ->
			    if (Nat.compare a b)<=0
			    then
			      IntegerRangeConstraint(a, b)
			    else	 Npkcontext.error 
			      "Firstpass: in Array access"
			      "null range not accepted "
			      
			| Some(RangeConstraint _) ->
			    Npkcontext.error 
			      "Firstpass: in Array access"
			      "constraint is RangeConstraint"
			      
			| _ ->  Npkcontext.error 
			    "Firstpass: in Array access"
			      "constraint is not IntegerRange"
		    in
		      (*
			let chk_exp = match new_constr with
			Some ctr -> make_check_constraint ctr exp
			| _ -> exp in
		      *)

		    let chk_exp = make_check_constraint new_constr exp in
   		      
		    let offset =  match new_constr  
		    with IntegerRangeConstraint(nat1, _) -> 
		      let borne_inf =   C.Const(C.CInt(nat1)) in 
		      let decal =  C.Binop (Npk.MinusI,chk_exp, borne_inf) in
			C.Binop (Newspeak.MultI, decal,  size_base)
		      |  _ -> Npkcontext.error "Firstpass.make_offset"
			   "contrainte (not IntegerRangeConstraint) not coded yet "
		    in
		      (C.Shift (v, offset), stypelt)
			
		  | _ ->        (*Constrained*)
		      Npkcontext.error "firstpass: ArrayAcces" 
			" subtyp_lv has not expected typ"  
  in
    
    
    
  let rec translate_if_exp cond exp_then exp_else expected_typ = 
    match expected_typ with
      | None | Some(Boolean) ->
	  let loc = Npkcontext.get_loc () in
	  let (tmp, decl, vid) = gen_tmp loc Boolean in
	  let name = ident_to_name tmp in
	  let instr_if = If (cond, 
			     [(Affect(Lval name, exp_then),loc)],
			     (*WG Lval (Array)*)
			     [(Affect(Lval name, exp_else),loc)])
	  in let tr_instr_if = 
	      translate_instr_list [(instr_if,loc)]
	  in
	    remove_symb tmp;
	    (C.Pref (decl::tr_instr_if, 
		     C.Lval (vid, translate_typ Boolean)), 
	     Boolean)
      | Some(_) -> Npkcontext.error 
	  "Firstpass.translate_if_exp" 
	    "invalid operator and argument"
	    
  and translate_binop op e1 e2 expected_typ = match op with
    | Neq ->  
	translate_unop Not (Binary(Eq,e1,e2)) expected_typ
    | Le ->  
	translate_unop Not (Binary(Gt,e1,e2)) expected_typ
    | Ge -> 
	translate_unop Not (Binary(Lt,e1,e2)) expected_typ
    | AndThen ->
	translate_if_exp e1 e2 (CBool(false)) expected_typ
    | OrElse ->
	translate_if_exp e1 (CBool(true)) e2 expected_typ
    | Xor ->
	translate_if_exp e1 (Unary(Not, e2)) e2 expected_typ
	  
    | _ -> 
	let expected_typ1 = Ada_utils.typ_operand op expected_typ		  
	in 
	let (tr_e1, tr_e2, typ) = 
	  try
	    let (tr_e1, typ1) = translate_exp e1 expected_typ1
	    in let (tr_e2, typ2) = translate_exp e2 (Some(typ1))
	    in (tr_e1, tr_e2, typ2)
	  with
	      AmbiguousTypeException ->
		try
		  let (tr_e2, typ2) = 
		    translate_exp e2 expected_typ1
		  in let (tr_e1, typ1) = translate_exp e1 
		      (Some(typ2))
		  in (tr_e1, tr_e2, typ1)
		with
		    AmbiguousTypeException -> 
		      Npkcontext.error "Firstpass.translate_binop" 
			"ambiguous operands"
	in 
	  Ada_utils.check_operand_typ op typ;
	  let tr_typ = translate_typ typ in
	  let coerce n exp =  C.Unop (Npkil.Coerce (Npk.domain_of_typ n), exp)
	  in   
	    match (op,tr_typ) with	  
		(* opérations sur entiers ou flottants *)
	      | (Plus,C.Scalar(Npk.Int n)) -> 
		  (coerce n (C.Binop (Npk.PlusI, tr_e1, tr_e2)), typ)
	      | (Plus,C.Scalar(Npk.Float(n))) -> 
		  (C.Binop (Npk.PlusF (n), tr_e1, tr_e2), typ) 
	      | (Moins,C.Scalar(Npk.Int n)) -> 
		  (coerce n (C.Binop (Npk.MinusI, tr_e1, tr_e2)), typ)
	      | (Moins,C.Scalar(Npk.Float(n))) -> 
		  (C.Binop (Npk.MinusF(n), tr_e1, tr_e2), typ)
	      | (Fois,C.Scalar(Npk.Int n)) -> 
		  (coerce n (C.Binop (Npk.MultI, tr_e1, tr_e2)), typ)
	      | (Fois,C.Scalar(Npk.Float(n))) -> 
		  (C.Binop (Npk.MultF (n), tr_e1, tr_e2), typ)
	      | (Div,C.Scalar(Npk.Int n)) -> 
		  (coerce n (C.Binop (Npk.DivI, tr_e1, tr_e2)), typ)
	      | (Div,C.Scalar(Npk.Float(n))) -> 
		  (C.Binop (Npk.DivF (n), tr_e1, tr_e2), typ)
	      | (Rem, C.Scalar(Npk.Int n)) -> 
		  (coerce n (C.Binop (Npk.Mod, tr_e1, tr_e2)), typ)
		    
	      (* comparaisons *)
	      | (Eq, C.Scalar(t)) -> 
		  (C.Binop (Npk.Eq t, tr_e1, tr_e2), Boolean)
	      | (Gt, C.Scalar(t)) -> 
		  (C.Binop (Npk.Gt t, tr_e1, tr_e2), Boolean)
	      | (Lt, C.Scalar(t)) -> 
		  (C.Binop (Npk.Gt t, tr_e2, tr_e1), Boolean)
		  
	      (*traités avec Not plus haut *)
	      | (Neq, _) | (Ge, _) | (Le, _) -> Npkcontext.error 
		  "Firstpass.translate_binop" 
		    "internal error : unexpected operator !"
		    
	      (* reste puissance, mod, concat, and, or.. *)
		    
	      (* opérations sur les booléens *)
	      | ((AndThen | Xor | OrElse), _) -> Npkcontext.error 
		  "Firstpass.translate_binop" 
		  "internal error : unexpected operator !"
		    
	      | ((Puissance | Mod | Concat | And | Or),_) -> 
		  Npkcontext.error "Firstpass.translate_binop" 
		    "not implemented"
		    
	      | _ -> Npkcontext.error "Firstpass.translate_binop" 
		  "invalid operator and argument"
		    
		    
		  
  and translate_unop op exp expected_typ = 
    match (op, expected_typ) with
	
      | (UPlus, Some(Float)) -> 
	  translate_exp exp expected_typ
      | (UPlus, Some(t)) when (integer_class t) -> 
	  translate_exp exp expected_typ
	   
      | (UPlus, None) ->
	  let (tr_exp, typ) = translate_exp exp expected_typ in   
	    (match typ with 
	      | Float -> (tr_exp, typ)
	      | t when (integer_class t) -> (tr_exp, typ)
	      | _ -> Npkcontext.error "Firstpass.translate_unop" 
		  "Unexpected unary operator and argument")
 
      | (UMoins, None) ->
	  (* on doit déterminer le type de l'opérande *)
	  let (_, typ) = translate_exp exp expected_typ in 
	    (match typ with 
	       | Float -> 
		   translate_binop Moins 
		     (CFloat(0.,"0")) exp expected_typ
	       | t when (integer_class t) -> 
		   translate_binop Moins (CInt(Nat.zero)) exp 
		     expected_typ
	       | _ -> Npkcontext.error "Firstpass.translate_unop" 
		   "Unexpected unary operator and argument")
	      
	      
      | (UMoins, Some(t)) when (integer_class t) -> 
	  translate_binop Moins (CInt(Nat.zero)) exp expected_typ
	    
      | (UMoins, Some(Float)) -> 
	  translate_binop Moins (CFloat(0.,"0")) exp expected_typ
	    
      | (Not, None) | (Not, Some(Boolean)) -> 
	  let (exp, _) = translate_exp exp (Some(Boolean))
	  in (C.Unop (K.Not, exp), Boolean)
	       
      | (Abs, _) -> Npkcontext.error "Firstpass.translate_unop" 
	  "abs not implemented" (* on ignore abs *) 
	    
      | _ -> 
	  Npkcontext.error "Firstpass.translate_unop" 
	    "Unexpected unary operator and argument"
	
  and translate_function_call fname tr_typ spec exp_list expected_typ =
    let (params, ret_t) = 
      match spec with
	| Function(_,params,subtyp) -> 
	    (params, check_typ expected_typ (base_typ subtyp))
	      
	| Procedure(name, _) -> Npkcontext.error 
	    "Firstpass.translate_exp" 
	      ((Print_syntax_ada.name_to_string name)
	       ^" is a procedure, function expected") 
    in
    let translate_paramater param exp = 
      let subtyp = param.ptype in
      let (tr_exp, _) = translate_exp exp (Some(base_typ subtyp)) in
	make_check_subtyp subtyp tr_exp in
    let tr_params = 
      if (List.length params <> List.length exp_list) 
      then Npkcontext.error "Firstpass.translate_function_call"
	"wrong number of arguments"
      else 
	List.map2 translate_paramater params exp_list
    in 
      (C.Call(tr_typ, fname, tr_params), ret_t)
       
  and translate_var name expected_typ = 
    let trans_fun_typ tr_typ fname spec = 
      translate_function_call fname tr_typ spec [] expected_typ
    in    

    (* recherche d'autres symboles respectant éventuellement
       une condition (filter).
       use : indique si on peut regarder les imports
        None : pas d'import regarder
        Some(ident) : on peut rechercher 
                     l'identifiant ident	       
       var_masque : indique si les variables sont masqués par un
                    autre symbole
       Si les variables sont visibles, alors on déclenche 
       une erreur.
    *)
    let rec mem_other_symb list_symb filter use 
	var_masque = 
      match list_symb with
	| [] -> (match use with
		   | None -> false
		   | Some(ident) ->
		       (* on regarde les imports *)
		       mem_other_symb 
			 (find_all_use ident) filter
			 None var_masque)

	(* les variables sont masquées *)
	| ((VarSymb(_)|NumberSymb(_)),_,_)::r when var_masque -> 
	    mem_other_symb r filter use var_masque
	      
	| ((VarSymb(_)|NumberSymb(_)),_,_)::_ -> 
	    Npkcontext.error 
	      "Firstpass.translate_var"
	      ((string_of_name name)^" is not visible : "
	       ^"multiple use clauses cause hiding")
	      
	(* un autre symbole existe ayant le bon type *)
	| (EnumSymb(_,typ,_),_,_)::_ when (filter typ) -> true
	| (FunSymb (_, (Function(_,[],typ)), _, _),_,_)::_ 
	    when (filter (base_typ typ)) -> true
	    
	(* symbole d'énumération ou de fonctions n'ayant
	   pas le bon type *)
	| (EnumSymb(_),_,_)::r | (FunSymb _,_,_)::r ->  
	    mem_other_symb r filter use var_masque
	      
    in
    let sans_selecteur ident name =  
	
      let rec find_use list_symb var_masque var_possible = 
	match list_symb with
	  | [] when var_masque -> 
	      (* variable masqué : au moins
		 un symbol mais mauvais type *)
	      Npkcontext.error
		"Firstpass.translate_var.find_enum_fun" 
		"uncompatible types"

	  | [] -> Npkcontext.error 
	      "Firstpass.translate_var"
		("cannot find symbol "^ident)
	      
	  (* cas des variables *)
	  | ((VarSymb(_)|NumberSymb(_)),_,_)::r when var_masque ->
	      find_use r var_masque var_possible
		
	  (* var visible, symbol unique*)
	  | [(VarSymb(v,typ,_,_),tr_typ,_)] when var_possible -> 
	      let t = check_typ expected_typ (base_typ typ) in 
		(C.Lval (v, tr_typ), t)

	  (* nombre visible, symbole unique*)
	  | [(NumberSymb(v,_),_,_)] when var_possible -> 
	      translate_number v expected_typ

	  (* d'autres symboles existent, conflit ! *)
	  | ((VarSymb(_)|NumberSymb(_)),_,_)::_ -> 
	      Npkcontext.error
		"Firstpass.translate_var"
		(ident^" is not visible : "
		 ^"multiple use clauses cause hiding")
		
	  (* cas ou le type attendu n'est pas précisé *)
	  | (EnumSymb(exp,typ,_),_,_)::r
	      when expected_typ = None -> 
	      if (mem_other_symb r (fun _ -> true)
		    None var_masque)
	      then raise AmbiguousTypeException 
	      else (exp, typ)
		
	  | (FunSymb (fname, (Function(_,[],_) as spec), _, tr_typ), 
	     _, _)::r when expected_typ=None ->
	      if (mem_other_symb r (fun _ -> true) 
		    None var_masque)
	      then raise AmbiguousTypeException
	      else trans_fun_typ tr_typ fname spec
		
	  (* cas ou le type attendu est connu et correct *)
	  | (EnumSymb(exp,typ,_),_,_)::r when 
	      known_compatible_typ expected_typ typ ->
	      if (mem_other_symb r 
		    (known_compatible_typ expected_typ)
		    None var_masque)
	      then (Npkcontext.error
		      "Firstpass.translate_var"
		      (ident^" is not visible : "
		       ^"multiple use clauses cause hiding"))
	      else (exp, typ)
		
	  | (FunSymb (fname, (Function(_,[],subtyp) as spec), _, tr_typ), 
	     _, _)::r 
	      when (known_compatible_typ expected_typ
		      (base_typ subtyp)) -> 
	      if (mem_other_symb r 
		    (known_compatible_typ expected_typ)
		    None var_masque)
	      then (Npkcontext.error
		      "Firstpass.translate_var"
		      (ident^" is not visible : "
		       ^"multiple use clauses cause hiding"))
	      else 
		trans_fun_typ tr_typ fname spec
		  
	  (* autres cas : une variable n'est plus valide *)
	  | (EnumSymb(_),_,_)::r | (FunSymb _,_,_)::r ->  
	      find_use r var_masque false 

		
      in 

      (* lorsqu'on a appelé cette fonction, on a au moins
	 un enum/fun en début de la liste.
	 Dans tous les cas, les variables sont masquées.
      *)
      let rec find_enum_fun list_symb = match list_symb with

	(* variable masqué par le 1er symbole enum/fun *)
	| ((VarSymb(_)|NumberSymb(_)), _, _)::r -> find_enum_fun r

	(* cas type attendu connu et compatible :
	   symbol unique à ce niveau*)
	| (EnumSymb(exp,typ,_),_,_)::_
	    when known_compatible_typ expected_typ typ ->
	    (exp, typ)
	      
	| (FunSymb (fname, (Function(_,[],subtyp) as spec), _, tr_typ), 
	    _, _)::_ 
	    when (known_compatible_typ expected_typ 
		    (base_typ subtyp)) -> 
	    trans_fun_typ tr_typ fname spec
	      
	(* cas type attendu non connu :
	   recherche d'autres symboles *)
	| (EnumSymb(exp,typ,_),_,_)::r
	    when expected_typ = None -> 
	    if (mem_other_symb r (fun _ -> true) 
		  (Some(ident)) true)
	    then raise AmbiguousTypeException
	    else (exp, typ)

	| (FunSymb (fname, (Function(_,[],_) as spec), _, tr_typ), 
	   _, _)::r when expected_typ=None ->
	    if (mem_other_symb r (fun _ -> true) 
		  (Some(ident)) true)
	    then raise AmbiguousTypeException
	    else trans_fun_typ tr_typ fname spec
		 
	(* autres cas : type incompatible, fonction
	   à plusieurs arguments, procedure *)
	| (EnumSymb(_), _,_)::r | (FunSymb _, _, _)::r -> 
	    find_enum_fun r
	       
	| [] -> find_use (find_all_use ident) true false
	     
	     
      in
      let list_symb = find_all_symb name in
	
	(*fonction principale : recherche d'un symbole
	  lorsque le package n'est pas précisé.*)
	match list_symb with
	  | (VarSymb(v,subtyp,_,_),tr_typ,_)::_ -> 
	      let t = check_typ expected_typ (base_typ subtyp) in 
		(C.Lval (v, tr_typ), t)
	  | (NumberSymb(v,_),_,_)::_ -> 
	      translate_number v expected_typ
	      
	  | [] -> find_use (find_all_use ident) false true
	  | _ -> find_enum_fun list_symb

    (* recherche d'un symbole externe, avec sélécteur *)
    and avec_selecteur name = 
      let rec find_enum_fun list_symb = match list_symb with
	| ((VarSymb(_)|NumberSymb(_)), _, _)::r -> 
	    find_enum_fun r
	      
	(* type attendu connu et compatible *)
	| (EnumSymb(exp,typ,_),_,_)::_
	    when known_compatible_typ expected_typ typ ->
	    (exp, typ)
	            
	| (FunSymb (fname, (Function(_,[],subtyp) as spec), _, tr_typ), 
	   _, _)::_ 
	    when (known_compatible_typ expected_typ 
		    (base_typ subtyp)) -> 
	    trans_fun_typ tr_typ fname spec
	      
	(* type attendu inconnu *)
	| (EnumSymb(exp,typ,_),_,_)::r
	    when expected_typ = None -> 
	    if mem_other_symb r (fun _ -> true) None true
	    then raise AmbiguousTypeException
	    else (exp, typ)
	      
	| (FunSymb (fname, (Function(_,[],_) as spec), _, tr_typ), 
	   _, _)::r when expected_typ=None ->
	    if mem_other_symb r (fun _ -> true) None true
	    then raise AmbiguousTypeException
	    else trans_fun_typ tr_typ fname spec
	      
	| (EnumSymb(_), _,_)::r | (FunSymb _, _, _)::r -> 
	    find_enum_fun r
	      
	| [] -> Npkcontext.error 
	    "Firstpass.translate_var"
	      "uncompatible types" in
	
      let list_symb = find_all_symb name in
	match list_symb with
	  | (VarSymb(v,subtyp,_,_),tr_typ,_)::_ -> 
	      let t = check_typ expected_typ (base_typ subtyp) in 
		(C.Lval (v, tr_typ), t)
	  | (NumberSymb(v,_),_,_)::_ -> 
	      translate_number v expected_typ
	  | [] -> Npkcontext.error 
	      "Firstpass.translate_var"
		("cannot find symbol "^(string_of_name name))
	  | _ -> find_enum_fun list_symb

    (* recherche interne uniquement *)
    and avec_selecteur_courant ident name = 
      let list_symb = find_all_symb ident in
      let rec find_global list_symb = 
	match list_symb with
	  | [] -> Npkcontext.error 
	      "Firstpass.translate_var"
		("cannot find symbol "^(string_of_name name))

	  (* variable globale *)
	  | (VarSymb(v,subtyp,true,_),tr_typ,_)::_ -> 
	      let t = check_typ expected_typ (base_typ subtyp) in 
		(C.Lval (v, tr_typ), t)
		  
	  (* nombre global *)
	  | (NumberSymb(v,true),_,_)::_ ->
	      translate_number v expected_typ

	  (* type connu et compatible *)
	  (*symbole d'énumération global *)
	  | (EnumSymb(exp,typ,true),_,_)::_ 
	      when known_compatible_typ expected_typ typ ->
	      (exp, typ)
	      	      
	  (* fonction interne *)
	  | (FunSymb (fname, 
		     (Function(_,[],subtyp) as spec), false, tr_typ), 
	     _, _)::_ 
	      when (known_compatible_typ expected_typ 
		      (base_typ subtyp)) -> 
	      trans_fun_typ tr_typ fname spec

	  (*type inconnu*)
	  (*symbole d'énumération global *)
	  | (EnumSymb(exp,typ,true),_,_)::r
	      when expected_typ = None -> 
	      if mem_other_symb r (fun _ -> true) None false
	      then raise AmbiguousTypeException
	      else (exp, typ)

	  (* fonction interne *)
	  | (FunSymb (fname, (Function(_,[],_) as spec), false, tr_typ), 
	      _, _)::r when expected_typ=None ->
	     if mem_other_symb r (fun _ -> true) None false
	     then raise AmbiguousTypeException
	     else trans_fun_typ tr_typ fname spec

	  | _::r -> find_global r
      in find_global list_symb
    in 
      find_name name sans_selecteur avec_selecteur 
	avec_selecteur_courant

  and translate_exp exp expected_typ = match exp with
    | NullExpr -> (C.Const(C.CInt(Nat.zero)), Integer)
	(* type access uniquement : faire une vérif sur expected
	   typ*)
	
    | CFloat(f,s) -> 
      let t = check_typ expected_typ Float 
      in (C.Const(C.CFloat(f,s)), t)
	   
    | CInt(i) ->
      let t = check_typ expected_typ IntegerConst 
      in (translate_int i, t)
	   
    | CChar(c) -> 
	let t = check_typ expected_typ Character
	in (translate_int (Nat.of_int c), t)

    | CBool(b) -> 
	let t = check_typ expected_typ Boolean
	in (translate_int (Ada_utils.nat_of_bool b), t)
	   
    | CString _ -> Npkcontext.error "Firstpass.translate_exp" 
	"string not implemented"
	  
    | Var(name) -> 
	translate_var name expected_typ
	  
    | Unary(unop,exp) -> 
	translate_unop unop exp expected_typ
	  
    | Binary(binop,exp1,exp2) -> 
	translate_binop binop exp1 exp2 expected_typ
	  
    | Qualified(subtyp, exp) ->
	let qtyp = check_typ expected_typ (base_typ subtyp) in
	let (tr_exp, typ) = translate_exp exp (Some(qtyp)) in
	  (make_check_subtyp subtyp tr_exp, typ)
	     
    | FunctionCall(name, exp_list) -> 
	(*fonction ou lecture d'un element de tableau/matrice*)
	(* let (fname, spec, tr_typ) = find_fun_symb name in *)
	let array_or_fun = find_fun_symb name in
	  begin
	    match array_or_fun with 
		(FunSymb (fname, spec, _, tr_typ), C.Fun,  _) -> 
		  translate_function_call 
		    fname tr_typ spec exp_list expected_typ
	      | (VarSymb(lv, subtyp, _, _),_,_) ->  begin  
		  
		  let rec destroy subt = (*du plus gros vers plus petit*)
		    let styp_fom_ind ind = let (a,_,_) = ind in a in	      
		    match subt with 
			Unconstrained(Declared (Array( _, ConstrainedArray(
			sbtyp_ind, sbtypelt_ind, _)),_)) ->
			  let sbtyp = styp_fom_ind sbtyp_ind in 
			  let sbtypelt = styp_fom_ind sbtypelt_ind in 
			  let deb = (sbtyp, sbtypelt) in
			  let fin = destroy sbtypelt in
			    [deb]@fin
		      | _ -> []
		  in

		  (*TO DO base_typ already exist use it if possible*)
		  let subtyp_to_typ sub = match sub with 
		      Constrained (z, _, _) ->  z
		    | _ -> Npkcontext.error "firstpass.ml:Function Call" 
					  " for array range TO DO "
		  in

		  (* TODO WG  ! ajouter le belongs ! *)		
		  let rec rebuild lv subt exp_list  = 
		    let lgth  = List.length exp_list in 
		    let  last_exp = List.hd exp_list in 
		    let (subt_range, tpelt) = List.nth subt (lgth - 1) in
		      (* ! ajouter le belongs ! *)	
		    
		    let chk_exp = make_check_subtyp subt_range last_exp 
		    in
		    let sz =  C.exp_of_int (
		      C.size_of ((translate_typ (base_typ tpelt)))) in 
		    
		    let offset = make_offset subt_range chk_exp sz in
		      
		      if (compare lgth 1 = 0) then
			
			let adatyp = subtyp_to_typ tpelt in 
			 
			  ( C.Lval (C.Shift (lv, offset),
				    (  translate_typ adatyp )
				   ), 
			    adatyp
			  )
		      else 
			let sh_lv = C.Shift (lv, chk_exp) in 
			  rebuild sh_lv subt (List.tl exp_list)
		  in	    
		    
		  let bk_typ = destroy subtyp in
		    
		  let dim = List.length exp_list in 		    
		    
		    if (compare (List.length bk_typ) dim < 0) 
		    then Npkcontext.error "firstpass.ml:Function Call"
		      "more elts than dimensions";
		    
		    if (compare 0 dim = 0) 
		    then Npkcontext.error "firstpass.ml:Function Call" 
		      "no element for shifting";
		    
		    let types = List.map (
		      fun x -> Some (subtyp_to_typ (fst x))
		    ) bk_typ in
		      
		    let dim_types = Array.to_list (
		      Array.sub  (Array.of_list types) 0 dim) in
		      
		    let tr_exp_list = 
		      List.map2 (fun x y ->
				   fst (translate_exp x y)) exp_list dim_types 
		    in
			
		      (*		     translate_exp exp expected_typ*)
		      match (rebuild lv bk_typ tr_exp_list) with
			  (C.Lval (a, tpelt), adatyp) ->
			    (C.Lval (a, tpelt),  adatyp)
			| _ ->  Npkcontext.error "firstpass.ml:Function Call"
			    ("firstpass unexepted form in translate_exp")
		end
	      | _ -> Npkcontext.error "Firstpass.translate_exp" 
		  "FunctionCall case but unexpected result"
	  end
	    
	    
    (*WG  TO DO *)
    | First _ | Last _  | Length _-> 
	Npkcontext.error
	  "Firstpass.translate_exp"
	  "Last, first or Length remaining in firstpass, non static "
	  
	  
(*   and make_check_constraint contrainte exp =  *)
(*     match contrainte with *)
(*       | IntegerRangeConstraint (v1,v2) ->  *)
(* 	  (\* vérification d'une contrainte entière *\) *)
(* 	  C.Unop(K.Belongs_tmp(v1,K.Known (Nat.add v2 Nat.one)), exp) *)
(*       | FloatRangeConstraint(_, _) -> exp *)
(* 	  (\* TODO : vérification d'une contrainte flottante *\) *)
(* 	  (\*C.Unop( *)
(* 	    C.Belongs( *)
(* 	    C.FloatRange((inf, string_of_float inf), *)
(* 	    (sup, string_of_float sup))), *)
(* 	    exp)*\) *)
      
(*       | RangeConstraint _ -> *)
(* 	  Npkcontext.error *)
(* 	    "Firstpass.make_check_constraint" *)
(* 	    "internal error : unexpected range constraint (non-static)" *)

(*       (\* TODO newspeak needs to be extended to support this case *\) *)
(*       (\* an exception should have been thrown before *)
(* 	 when a constraint is none static *\) *)
	  
(*       (\* if the solution of a belongs who uses temporary is choosed,  *)
(* 	 here is what to do :*\) *)
(*       (\* | RangeConstraint(Var(v1), Var(v2)) -> *)
(* 	 let (tr_v1,_) = translate_lv v1 false *)
(* 	 and (tr_v2,_) = translate_lv v2 false in *)
(* 	 C.Unop( *)
(* 	 C.Belongs( *)
(* 	 C.LvalRange(tr_v1,tr_v2)),exp) *\) *)


(*   and make_check_subtyp subtyp exp =  *)
(*     match subtyp with *)
(*       | Unconstrained _ -> exp *)
(*       | Constrained(_, contrainte, _) -> *)
(* 	  make_check_constraint contrainte exp *)
(*       | SubtypName _ -> *)
(* 	  Npkcontext.error *)
(* 	    "Firstpass.make_check_subtyp" *)
(* 	    "internal error : unexpected subtyp name" *)

	   
  (* test de correction des indications de sous-types, pour les cas
     non static *)

 (* and make_check_subtyp_indication (subtyp_ref, contrainte, subtyp_res) 
    loc =

    let add_bounds tmp1 tmp2 typ =
      let subtyp = Unconstrained(typ) in
      let v1 = add_var loc subtyp tmp1 false false
      and v2 = add_var loc subtyp tmp2 false false in
      let trans_typ = translate_subtyp subtyp in
      let decl1 = C.Decl(trans_typ, tmp1, v1)
      and decl2 = C.Decl(trans_typ, tmp2, v2)
      in (v1, v2, decl1, decl2) in 
    

    let decl_unconstrained_init typ exp1 exp2 tmp1 tmp2 =
      let name_to_ident (_,ident) = ident in
      let (v1, v2, decl1, decl2) = add_bounds 
	      (name_to_ident tmp1) (name_to_ident tmp2) typ in
      let (tr_exp1,_) = translate_exp exp1 (Some(typ))
      and (tr_exp2,_) = translate_exp exp2 (Some(typ)) in
      let aff1 = make_affect (C.Var v1) tr_exp1 
	(Unconstrained(typ)) loc
      and aff2 = make_affect (C.Var v2) tr_exp2 
	(Unconstrained(typ)) loc
      in ((v1,v2),(tr_exp1,tr_exp2),[(decl1,loc);(decl2,loc)],[aff1;aff2])

    let tested_decl_init typ exp1 exp2 tmp1 tmp2 =
      let ((v1, v2), (tr_exp1, tr_exp2),decl, unconstrained_init) = 
	decl_unconstrained_init typ exp1 exp2 tmp1 tmp2 
      and tr_typ = translate_typ typ in
      let test = C.Binop(Npk.Gt(tr_typ), tr_exp1, tr_exp2)
      and aff1_constrained = make_affect (C.Var v1) tr_exp1 
	subtyp_ref loc
      and aff2_constrained = make_affect (C.Var v2) tr_exp2 
	subtyp_ref loc
      in (decl,[(C.If(test,unconstrained_init,
		      [aff1_constrained;aff2_constrained]), loc)])
		
    in match (subtyp_ref, contrainte,subtyp_res) with
      | (_, _, None) ->
	  Npkcontext.error
	    "Firstpass.make_check_subtyp_indication"
	    "internal error : no subtyp provided"
      | (SubtypName _, _, _) | (_, _, Some(SubtypName _)) ->
	  Npkcontext.error
	    "Firstpass.make_check_subtyp_indication"
	    "internal error : unexpected subtyp name"

      (* rien à vérifier, mais on déclare les temps nécessaire au sous-type *)
      | (Unconstrained _, Some(RangeConstraint(exp1, exp2)), 
	 Some(Constrained(typ, RangeConstraint(Var tmp1, Var tmp2), _))) -> 
	  let (_,_,decl, aff) = decl_unconstrained_init 
	    typ exp1 exp2 tmp1 tmp2
	  in (decl,aff)

      (* rien à vérifier pour les cas suivants *)
      | (Unconstrained _, _, _) -> ([],[])
      | (_, None, _) -> ([],[])
      | (_, Some(NullRange),_) -> ([],[])

	  
      (* correction d'une contrainte non statique *)
      | (Constrained _, Some(RangeConstraint(exp1,exp2)),
	 Some(Constrained(typ, RangeConstraint(Var tmp1, Var tmp2), _))) ->
	  tested_decl_init typ exp1 exp2 tmp1 tmp2
	  

      (* autres cas : on ne fait rien pour l'instant *)
      | (Constrained _, Some _, Some _) -> ([],[])*)


  and make_affect id exp subtyp_lv loc =   
    let typ_lv = base_typ subtyp_lv in
    let typ = translate_typ typ_lv in
    let checked_exp = make_check_subtyp subtyp_lv exp
    in (C.Set(id,typ,checked_exp),loc)
	 
	 
  and translate_affect lv exp loc = 
    let (tr_lv,subtyp_lv) = translate_lv lv true translate_exp
      (*WG*) in
    let (tr_exp,_) = translate_exp exp (Some(base_typ subtyp_lv))
    in make_affect tr_lv tr_exp subtyp_lv loc
	 
  and translate_instr_list instr_list = match instr_list with
    | (instr,loc)::r ->	
	(Npkcontext.set_loc loc;
	 match instr with
	   | NullInstr -> (translate_instr_list r)
	   | Affect(lv,exp) -> 
	       let tr_affect = translate_affect lv exp loc
	       in  tr_affect::(translate_instr_list r)
	   | Return(exp) -> 
	       translate_instr_list (* WG Lval for Array diff*)
		 ((Affect(Lval (ident_to_name ret_ident),exp),loc)
		  ::(ReturnSimple,loc)::r)
	   | ReturnSimple ->
	       let tr_reste = 
		 match r with
		   | [] -> []
		   | (_,next_loc)::_ -> 
		       Npkcontext.set_loc next_loc;
		       Npkcontext.print_warning 
			 "Firstpass.translate_instr_list" 
 			 "Unreachable code";
		       Npkcontext.set_loc loc;
		       translate_instr_list r
	     in
		 (C.Goto ret_lbl, loc)::tr_reste
	   | If(condition,instr_then,instr_else) -> 
	       let (tr_exp,typ) = 
		 translate_exp condition (Some(Boolean))
	       in 
		 (match typ with
		    | Boolean -> 
			let tr_then = 
			  translate_instr_list instr_then
			and tr_else = 
			  translate_instr_list instr_else
			in (C.If(tr_exp,tr_then,tr_else),loc)
			   ::(translate_instr_list r)
		    | _ -> Npkcontext.error 
			"Firstpass.translate_instr_list" 
			  "expected a boolean type for condition")
		   
	   | Exit(Some(cond)) ->
	       translate_instr_list
		 ((If(cond, [(Exit(None), loc)], []), loc)::r)
		 
	   | Exit(None) -> 
	       (C.Goto brk_lbl, loc)
	       ::(translate_instr_list r)
		 
	   | Loop(While(cond), body) ->
	       translate_instr_list 
		 ((Loop(NoScheme,(Exit(Some(Unary(Not,cond))),loc)::body),
		   loc)::r)
		 
	   | Loop(NoScheme, body) ->
	       let tr_body = translate_instr_list body in
		 (C.Block([C.Loop(tr_body), loc], Some (brk_lbl,[])),loc)
		 ::(translate_instr_list r)
		   
	   | ProcedureCall (name, args) -> begin
	       let array_or_fun  = find_fun_symb name in 
		 match array_or_fun with 
		     (FunSymb (fname, spec, _, tr_typ), C.Fun,  _) ->   
		       let params = 
			 match spec with 
			   | Function(_) -> Npkcontext.error 
			       "Firstpass.translate_instr" 
				 ((Print_syntax_ada.name_to_string name)
				  ^" is a function, procedure expected")
			   | Procedure(_, params) -> params 
		       in
		       let tr_param param exp = 
			 match param.mode with
			   | In -> fst (translate_exp exp 
					  (Some(base_typ param.ptype)))
			   | Out | InOut -> 
			       match exp with
				 | Var(v) -> 
				     let (vid, typ) = translate_lv (Lval v) true translate_exp in
				     let t = check_typ 
				       (Some(base_typ param.ptype)) 
				       (base_typ typ) in
				       C.AddrOf(vid, translate_typ t) 
				 | _ ->  Npkcontext.error 
				     "Firstpass.translate_instr"
				       ("actual for out parameter"
					^"must be a variable")
		       in
		       let tr_params = 
			 if (List.length params <> List.length args) 
			 then Npkcontext.error "Firstpass.translate_instr"
			   "wrong number of arguments"
			 else 
			   List.map2
			     tr_param
			     params args
		       in
			 (C.Exp(C.Call(tr_typ, fname, tr_params)), loc)
			  ::(translate_instr_list r)
		   | _ -> 
		       Npkcontext.error "Firstpass.translate_instr"
			 "find_fun_symb did not expect this (maybe array) as an instr! " 
		  
		
	     end	  
      )
	  
    | [] -> []
	

  in
  let translate_param param =     
    let typ_cir = match param.mode with
      | In -> translate_typ (base_typ param.ptype)
      | Out -> C.Scalar(Npk.Ptr)
      | InOut -> C.Scalar(Npk.Ptr)
    in 
      List.map 
	(fun _ -> typ_cir) param.pnom

  and add_param loc param = 
    let (deref,ro) = match param.mode with
      | In -> (false, true)
      | Out | InOut -> (true, false)
    in
      List.map 
	(fun name -> (name, add_var loc param.ptype name deref ro))
      param.pnom
  in

    (* prend une liste de paramètres en argument
       et renvoie liste de typ *)
  let translate_param_list param_list = 
    (List.flatten
       (List.map translate_param param_list))
  and add_params subprog_spec loc = 
    let (param_list, ret_id) =
      match subprog_spec with
	| Function(_, param_list, return_type) ->
	    (param_list, add_var loc return_type ret_ident false false)
	| Procedure(_,param_list) -> 
	    (param_list, C.fresh_id ())
    in
    let (param_names, vids) =
      (List.split (List.flatten
		     (List.map (add_param loc) param_list)))
    in
      (param_names, (ret_id,vids))
	
      
  in
  let translate_sub_program_spec subprog_spec = 
    let (name, param_list, return_type) =
      match subprog_spec with
	| Function(name,param_list,return_type) ->
	    (name, param_list, Some(return_type))
	| Procedure(name,param_list) -> 
	    (name, param_list, None)
    in let params_typ : C.typ list = 
	translate_param_list param_list
    in 
      (name, (params_typ, translate_subtyp_option return_type))
	
  in

  let add_fundecl subprogspec loc = 
    let check_ident name =
     if Hashtbl.mem symbtbl name
     then 
       let list_ident = Hashtbl.find_all symbtbl name in
	 List.iter
	   (fun symb -> match symb with
	      | (FunSymb (_,_,extern', _),_,_) 
		  when extern'= !extern -> 
		  Npkcontext.error "Firstpass.add_fundecl"
		    ("conflict : "^(string_of_name name)
		     ^" already declared")
	      | (FunSymb _, _, _) -> ()
	      | ((VarSymb(_)|NumberSymb(_)),_,_) -> 
		  Npkcontext.error "Firstpass.add_fundecl"
		    ("conflict : "^(string_of_name name)
		     ^" already declared")
	      | (EnumSymb(_,etyp,_),_,_) ->
		  begin
		    match subprogspec with
		      | Function(_, [], rtyp)
			  when etyp = (base_typ rtyp) ->
			  Npkcontext.error "Firstpass.add_fundecl"
			    ("conflict : "^(string_of_name name)
 			     ^" already declared")
		      | _ -> ()
		  end)
	   list_ident
    in 
    let (name, ftyp) = translate_sub_program_spec subprogspec
    in
      check_ident name;
      Hashtbl.add symbtbl name 
	(FunSymb (C.Fname(translate_name name), subprogspec, 
		  !extern, ftyp), C.Fun, loc);
      ftyp
  in

  let translate_enum_declaration typ_decl list_val_id loc global = 
    List.iter 
      (fun (x,id) -> add_enum loc x id 
	 (Declared(typ_decl, loc)) global) 
      list_val_id in 

  let translate_derived_typ_decl subtyp_ind loc global = 
    match Ada_utils.extract_typ subtyp_ind with
      | Declared(Enum(_, list_val_id, _) as typ_decl,_) -> 
	  translate_enum_declaration typ_decl list_val_id loc global
      | _ -> () 
  in   
  let translate_typ_declaration typ_decl loc global = 
    match typ_decl with
      | Enum(_, list_val_id, _) -> 
	  translate_enum_declaration typ_decl list_val_id loc global
      | DerivedType(_, ref_subtyp_ind) -> 
	  translate_derived_typ_decl ref_subtyp_ind loc global 
      | IntegerRange _ -> ()
      | Array _ -> ()
  in
 
  (* déclarations basiques locales *)
  let translate_basic_declaration basic loc = match basic with
    | ObjectDecl(idents, subtyp_ind, def, const) -> 
(*	let subtyp = Ada_utils.extract_subtyp subtyp_ind in
*)
	let subtyp =  Ada_utils.extract_subtyp subtyp_ind in
	
	let read_only = match const with
	  | Variable -> false
	  | Constant | StaticVal(_) -> true in
	  List.fold_right
	    (fun ident (list_decl, list_aff) -> 
	       let id = add_var loc subtyp ident false read_only in
	       let decl = (C.Decl(translate_subtyp subtyp, 
				  ident, id),loc)::list_decl
	       and aff =  
		 match def with
		   | None -> list_aff
		   | Some(exp) -> 
		       let (tr_exp,_) = translate_exp exp 
			 (Some(base_typ subtyp)) in 
			 (make_affect (C.Var(id)) 
			    tr_exp subtyp loc)::list_aff
	       in (decl,aff))
	    idents
	    ([],[])
	  
    | TypeDecl(typ_decl) -> 
	translate_typ_declaration typ_decl loc false;
	([],[])

    | SubtypDecl _ ->
	([],[])
	  
    | SpecDecl(_) -> Npkcontext.error 
	"Firstpass.translate_basic_declaration" 
	  ("déclaration de sous-fonction, sous-procedure ou "
	   ^"sous package non implémenté")
	  
    | UseDecl(use_clause) -> List.iter add_context use_clause;
	([],[])
	  
    | NumberDecl(idents, _, Some(v)) ->
	List.iter 
	  (add_number loc v false)
	  idents;
	([],[])
    | NumberDecl _ -> 
	Npkcontext.error 
	  "Firstpass.translate_basic_declaration" 
	  "internal error : number declaration whitout value"
    | RepresentClause _ ->
	Npkcontext.error 
	  "Firstpass.translate_basic_declaration" 
	  "internal error : unexpected representation clause"


  in
  let translate_declarative_item (item,loc) = 
    Npkcontext.set_loc loc;
    match item with
      | BasicDecl(basic) -> translate_basic_declaration basic loc
	  
      | BodyDecl(_) -> Npkcontext.error 
	  "Firstpass.translate_instr_list" 
	    "sous-fonction, sous-procedure ou sous package non implémenté"
	    
  in
    
  (* renvoie liste d'instructions (affectations par défaut)
     fonction appelée pour la partie déclarative d'une fonction
     ou procédure *)
  let translate_declarative_part decl_part = 
    let (decl, aff) =
      List.split (List.map translate_declarative_item decl_part)
    in 
    let list_decl = List.flatten decl
    and list_aff = List.flatten aff
    in list_decl@list_aff 
  in
  
 
  
  let remove_basic_declaration basic = match basic with
    | ObjectDecl(idents, _, _, _) ->
	List.iter 
	  (fun x -> ignore (remove_symb x)) 
	  idents
    | TypeDecl(Enum(_,idents,_)) -> 
	List.iter 
	  (fun (x,_) -> ignore (remove_symb x)) 
	  idents
    | TypeDecl(_) -> ()
    | SubtypDecl(_) -> ()
    | SpecDecl(_) -> Npkcontext.error 
	"Firstpass.remove_basic_declaration" 
	  ("déclaration de sous-fonction, sous-procedure ou "
	   ^"sous package non implémenté")
    | UseDecl(use_clause) -> List.iter remove_context use_clause
    | NumberDecl(idents, _, _) -> 
	List.iter 
	  (fun x -> ignore (remove_symb x)) 
	  idents
    | RepresentClause _ -> 
	Npkcontext.error 
	  "Firstpass.remove_basic_declaration" 
	  "internal error : unexpected representation clause"

  in
	  
  let remove_declarative_item (item,_) = match item with
    | BasicDecl(basic) -> remove_basic_declaration basic
    | BodyDecl(_) -> () in
	
    
  let remove_declarative_part decl_part = 
    List.iter remove_declarative_item decl_part in
      

  let add_funbody subprogspec decl_part instr_list loc =
    let search_spec name = 
      let list_ident = Hashtbl.find_all symbtbl name in
	try
	  let symb = 
	    (List.find
	       (fun symb -> 
		  match symb with
		    | (FunSymb (_, spec, false, _),_,_) -> 
			spec = subprogspec
		    | ((FunSymb _ | VarSymb _
		    | EnumSymb _ | NumberSymb _),_,_) -> false)
	       list_ident)
	  in 
	    match symb with
	      | (FunSymb (_, _, _, ftyp), C.Fun, _) -> ftyp
	      | _ -> 
		  Npkcontext.error "Firstpass.add_funbody.seach_spec" 
		    "internal error : typ is not a fun typ"
	with Not_found -> add_fundecl subprogspec loc
    in
    let name = match subprogspec with
      | Function(name, _, _) -> name
      | Procedure(name, _) -> name in
      
    let ftyp = search_spec name
    and (params, vids) = add_params subprogspec loc
    and body_decl = translate_declarative_part decl_part
    and body = translate_instr_list instr_list in 
    let body_lbl = (vids, (C.Block (body_decl@body, 
				    Some (ret_lbl,[])), 
			   loc)::[]) 
    in
      remove_formals params;
      remove_declarative_part decl_part;
      
      Hashtbl.replace fun_decls (translate_name name)
	(ftyp, loc, body_lbl)
	   

	
  in
      
  let rec translate_global_basic_declaration (basic, loc) = 
    match basic with
      | ObjectDecl(idents, subtyp_ind, init, const) ->
	  (*let subtyp = Ada_utils.extract_subtyp subtyp_ind in*)
	let subtyp = Ada_utils.extract_subtyp subtyp_ind in


	  let read_only = match const with
	    | Variable -> false
	    | Constant | StaticVal(_) -> true in
	  let tr_typ = translate_subtyp subtyp in
	  let tr_init : C.init option = 
	    match (init,!extern) with
	      | (_,true) -> Some None
	      | (None,_) -> Some(None)
	      | (Some(exp),false) -> 
		  let (tr_exp,_) = translate_exp exp (Some(base_typ subtyp)) 
		  in Some((Some [(0, extract_scalar_typ tr_typ, tr_exp)]))
	  in
	    List.iter
	      (add_global loc subtyp tr_typ tr_init read_only)
	      idents
	    
      | TypeDecl(typ_decl) -> 
	  translate_typ_declaration typ_decl loc true
	    
      | SubtypDecl _ -> ()

      | UseDecl(use_clause) -> List.iter add_context use_clause
	    
      | SpecDecl(spec) -> translate_spec spec loc false
      | NumberDecl(idents, _, Some(v)) -> 
	  List.iter 
	    (add_number loc v true)
	    idents
      | NumberDecl(_) -> 
	  Npkcontext.error 
	    "Firstpass.translate_global_basic_declaration" 
	    "internal error : number declaration whitout value"
      | RepresentClause _ ->
	  Npkcontext.error 
	    "Firstpass.translate_global_basic_declaration" 
	    "internal error : unexpected representation clause"


  (* quand cette fonction est appelée, on est dans le corps d'un
     package *)
  and translate_global_decl_item (item,loc) = 
    Npkcontext.set_loc loc;
    match item with
      | BasicDecl(basic) -> 
	  translate_global_basic_declaration (basic, loc)
	    
      | BodyDecl(body) -> translate_body body false loc
	  
  and translate_spec spec loc glob = match spec with
      
    | SubProgramSpec(subprog_spec) -> 
	ignore (add_fundecl subprog_spec loc)
	
    | PackageSpec(nom, basic_decl_list) -> 
	match glob with
	  | false -> Npkcontext.error 
	    "Firstpass.translate_spec" 
		"déclaration de sous package non implémenté"
	  | true -> 
	      set_current_package nom;
	      let _ = List.map 
		(* problème : variables *)
		(translate_global_basic_declaration)
		basic_decl_list
	      in
		raz_current_package ();
		if !extern then add_with_package nom
	      
	      
	      
  and translate_body body glob loc :unit =   
    
    Npkcontext.set_loc loc;
    match (body, glob) with
      | (SubProgramBody(subprog_decl,decl_part, instr_list), _) ->
	  add_funbody subprog_decl decl_part instr_list loc
	  
	      
	      
      (* si globals est fournie (Some) alors on est au niveau 
	 supérieur,
	 donc on accepte la déclaration d'un package. Sinon, il
	 s'agit d'un sous-package, ce qui n'est pas géré *)
      | (PackageBody(name, package_spec, decl_part, _), 
	 true) ->
	  set_current_package name;
	  (match package_spec with
	     | None -> ()
	     | Some(_, basic_decls) -> 
		 ignore
		 (List.map 
		   translate_global_basic_declaration
		   basic_decls));
	  ignore (List.map translate_global_decl_item
		    decl_part)
	    
      | (PackageBody(_), false) -> Npkcontext.error 
	  "Firstpass.translate_body" 
	    "déclaration de sous package non implémenté"
	    
  in	    
    
  let translate_library_item lib_item loc =
    Npkcontext.set_loc loc;
    match lib_item with
      | Body(body) -> translate_body body true loc
	  
      | Spec(_) -> Npkcontext.error 
	  "Firstpass.translate_library_item" 
	    "Rien à faire pour les spécifications"
	    
  in

  let rec translate_context context = 
    match context with
      | With(nom, loc, spec)::r ->
	  Npkcontext.set_loc loc;
	  (match spec with
	    | Some(spec, loc) -> 
		translate_spec spec loc true;
		add_with_package nom;
		translate_context r
	    | None -> Npkcontext.error 
		"Firstpass.translate_context" 
		  "internal error : no specification provided")
      | UseContext(use_clause)::r -> 
	  List.iter add_context use_clause;
	  translate_context r
      | [] -> ()
	    
  
  in
    (* corps de la fonction translate *)
    
  let normalized_compil_unit =  Ada_normalize.normalization compil_unit false
  in
    Npkcontext.print_debug
      (Print_syntax_ada.ast_to_string [normalized_compil_unit]);
  let (context,lib_item,loc) = normalized_compil_unit
  in
    try 
      Npkcontext.set_loc loc;
      extern := true;
      translate_context context;
      extern := false;
      translate_library_item lib_item loc;
      Npkcontext.forget_loc ();
      (globals, fun_decls, [])
    with
	AmbiguousTypeException -> Npkcontext.error 
	  "Firstpass.translate" 
	  "uncaught ambiguous type exception"
	  
