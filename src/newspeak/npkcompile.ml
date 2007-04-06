open Cil
open Cilutils

open Npkcontext
open Npkutils
open Npkenv
open Npkil
open Npkfirstpass

module K = Newspeak




(*===================================*)
(* Misc. functions used by translate *)
(*===================================*)

(* extract case and default labels *)
let extract_labels status l = 
  let rec extract_aux l = match l with 
    | [] -> []
    | (Case (_, loc))::r | (Default loc)::r -> begin
	try
	  (K.Label (retrieve_switch_label status loc), !cur_loc)::(extract_aux r)
	with Not_found -> error "Npkcompile.translate_labels" "unexpected label"
      end
    | (Label (_, _, false))::r -> extract_aux r
    | _ -> error "Npkcompile.translate_labels" "invalid label"
  in
    extract_aux l



(* Generates a Newspeak statement by wrapping a block body with
   declarations decls. The order of the declaration must be carefully
   checked because in Newspeak, the variables are identified by their
   positions in the declaration stacks, not by their names *)
let generate_body body decls =
  let rec generate_aux body decls =
    match decls with
      | [] -> body
      | (name, t, loc)::r ->
	  generate_aux [K.Decl (name, t, body), loc] r
  in generate_aux body decls





(*================================*)
(* The central translate function *)
(*================================*)

let rec translate_const c =
  match c with
      CInt64 (i, _, _) -> K.Const (K.CInt64 i)
    | CStr s -> get_cstr s
    | CChr c -> K.Const (K.CInt64 (Int64.of_int (Char.code c))) 
	
    | CReal (f, _, Some s) -> K.Const (K.CFloat (f, s))
    | CReal (f, _, None) ->
	let s = string_of_float f in
	  print_warning "Npkcompile.translate_const"
	    ("No string representation available for const "^s);
	  K.Const (K.CFloat (f, s))
	    
    | CWStr _ | CEnum _
	-> error "Npkcompile.translate_const"
	("const '"^(string_of_exp (Const c))^"' not handled")

	
and translate_cast t e = 
  match t, e with
    | TNamed (info, _), e -> translate_cast info.ttype e
	
    | TPtr _, Const (CInt64 (c, _, _)) when c = Int64.zero -> K.Const K.Nil
	
    | t, e -> begin
	match translate_typ t, translate_typ (typeOf e) with
	    (K.Scalar (K.Int k1), K.Scalar (K.Int k2)) when k1 = k2 ->
	      translate_exp e

	  | K.Scalar (K.Int int_t), K.Scalar (K.Int _) ->
	      K.make_int_coerce int_t (translate_exp e)
		
	  | (K.Scalar (K.Float sz as t'), K.Scalar (K.Float _ as t)) -> 
	      K.UnOp (K.Cast (t, t'), translate_exp e)
		
	  | (K.Scalar (K.Float sz' as t'), K.Scalar (K.Int (_, sz) as t)) ->
	      K.UnOp (K.Cast (t, t'), translate_exp e)
		
	  | (K.Scalar (K.Int (sign, sz') as kt'), 
	     K.Scalar (K.Float sz as kt)) ->
	      if sign = K.Unsigned then
		print_warning "Npkcompile.translate_cast"
		  ("cast from float to unsigned integer: "
		   ^"sign may be lost: "
		   ^(string_of_type (typeOf e))^"' -> '"
		   ^(string_of_type t)^"' in '"
		   ^(string_of_exp (CastE (t, e)))^"'");
	      K.UnOp (K.Cast (kt, kt'), translate_exp e)
		
	  | K.Scalar K.Ptr, K.Scalar K.Ptr -> translate_exp e
	  | K.Scalar K.FunPtr, K.Scalar K.FunPtr ->
	      print_warning "Npkcompile.translate_cast"
		("probable dangerous cast: '"^(string_of_type (typeOf e))^"' -> '"
		 ^(string_of_type t)^"' in '"^(string_of_exp (CastE (t,e)))^"'");
	      translate_exp e
		
	  | (K.Scalar (K.Int (sign, sz)), K.Scalar K.Ptr)
	      when sz = pointer_size && !castor_allowed ->
	      print_warning "Npkcompile.translate_cast"
		("probable invalid cast '"^(string_of_type (typeOf e))^"' -> '"
		 ^(string_of_type t)^"' in '"^(string_of_exp (CastE (t,e)))^"'");
		K.UnOp (K.PtrToInt (sign, sz), translate_exp e)
		  
	  | (K.Scalar K.Ptr, K.Scalar (K.Int (_, sz) as int_t))
	      when sz = pointer_size && !castor_allowed ->
	      print_warning "Npkcompile.translate_cast"
		("probable invalid cast '"^(string_of_type (typeOf e))^"' -> '"
		 ^(string_of_type t)^"' in '"^(string_of_exp (CastE (t,e)))^"'");
		K.UnOp (K.Cast (int_t, K.Ptr), translate_exp e)
		  
	  | K.Scalar (K.Ptr as kt'), K.Scalar (K.FunPtr as kt) 
	      when !castor_allowed ->
	      print_warning "Npkcompile.translate_cast"
		("probable invalid cast '"
		 ^(string_of_type (typeOf e))^"' -> '"
		 ^(string_of_type t)^"' in '"
		 ^(string_of_exp (CastE (t,e)))^"'");
		K.UnOp (K.Cast (kt, kt'), translate_exp e)
		  
	  | _ ->
	      error "Npkcompile.translate_cast"
		("translate cast: Invalid cast '"^(string_of_type (typeOf e))^"' -> '"
		 ^(string_of_type t)^"' in '"^(string_of_exp (CastE (t,e)))^"'")
      end
	
	
and translate_lval lv =
  match lv with
    | Var v, NoOffset -> get_var v
	
    | Mem e, NoOffset ->
	let sz = size_of (typeOfLval lv) in
	  K.Deref (translate_exp e, sz)
	    
    | _ ->
	let (lv', offs) = removeOffsetLval lv in
	let t = typeOfLval lv' in
	  match offs with
	      Field (info, NoOffset) ->
		let o = offset_of t offs in
		  K.Shift (translate_lval lv', K.exp_of_int o)
		    
	    | Index (e, NoOffset) -> begin
		try
		  match translate_typ t with
		    | K.Array (t_elt, len) ->
			let index_exp =
			  K.BinOp (K.MultI, K.make_belongs len (translate_exp e),
				  K.exp_of_int (K.size_of t_elt))
			in
			  K.Shift (translate_lval lv', index_exp)
			    
		    | _ -> error "Npkcompile.translate_lval" "array expected"
		with LenOfArray -> begin
		  match lv', offs with
		    | (Var v, NoOffset), Index (e, NoOffset) ->
			K.Shift_tmp (v.vname, translate_exp e)
		    | _ ->
			error "Npkcompile.translate_lval"
			  ("type of lval "^(string_of_lval lv')
			    ^" is not defined enough")
		end
	      end

	    | _ -> error "Npkcompile.translate_lval" "offset not handled"
		


(* TODO: See if there cannot be any factorisation here *)
and translate_exp e =
  match e with
      Const c -> translate_const c
    | SizeOf t -> K.exp_of_int (size_of t)
    | SizeOfE e -> K.exp_of_int (size_of (typeOf e))
    | SizeOfStr s -> K.exp_of_int (String.length s + 1)
	
    | CastE (t, e) -> translate_cast t e
	
    (* Unary operators  *)
    (*------------------*)
	
    | UnOp (Neg, e, t) -> begin
	match translate_typ t with
	  | K.Scalar (K.Int int_t) ->
	      K.make_int_coerce int_t (K.BinOp (K.MinusI, K.exp_of_int 0, translate_exp e))
	  | K.Scalar (K.Float sz) ->
	      (* TODO: check this transformation is really correct 
		 i.e. source and destination expression have the same 
		 semantics *)
	      K.BinOp (K.MinusF sz, K.zero_f, translate_exp e)
	  | _ -> error "Npkcompile.translate_exp" "integer or float type expected"
      end
	
    | UnOp (BNot, e, t) -> begin
	match translate_typ t with
	    K.Scalar (K.Int int_t) -> 
	      let b = K.domain_of_typ int_t in
		K.UnOp (K.BNot b, translate_exp e)
	  | _ -> error "Npkcompile.translate_exp" "integer type expected"
      end
	
    | UnOp (LNot, e, t) -> begin
	match translate_typ t with
	    K.Scalar (K.Int int_t) -> K.UnOp (K.Not, translate_exp e)
	  | _ -> error "Npkcompile.translate_exp" "integer type expected"
      end
	
    (* Binary operators *)
    (*------------------*)
	
    (* Arithmetic and floating point operations *)
    | BinOp (PlusA as o, e1, e2, t)   | BinOp (MinusA as o, e1, e2, t)
    | BinOp (Mult as o, e1, e2, t)    | BinOp (Div as o, e1, e2, t) 
    | BinOp (Mod as o, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar (K.Int int_t) ->
	      K.make_int_coerce int_t (K.BinOp (translate_arith_binop o,
						translate_exp e1, translate_exp e2))
		
	  | K.Scalar (K.Float sz) ->
	      K.BinOp (translate_float_binop sz o, 
		       translate_exp e1, translate_exp e2)
	  | _ -> error "Npkcompile.translate_exp" "integer or float type expected"
      end
	
    (* Bitwise operations *)
    | BinOp (BAnd as o, e1, e2, t)    | BinOp (BOr as o, e1, e2, t)
    | BinOp (BXor as o, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar (K.Int int_t) ->
	      K.BinOp (translate_logical_binop int_t o,
		       translate_exp e1, translate_exp e2)
	  | _ -> error "Npkcompile.translate_exp" "integer type expected"
      end
	
    (* Logical operations *)
    | BinOp (Shiftlt as o, e1, e2, t) | BinOp (Shiftrt as o, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar (K.Int int_t) ->
	      K.make_int_coerce int_t (K.BinOp (translate_logical_binop int_t o,
						translate_exp e1, translate_exp e2))
	  | _ -> error "Npkcompile.translate_exp" "integer type expected"
      end
	
    (* Equality and inequality, comparisons : between numbers or pointers *)
    | BinOp ((Eq|Gt) as o, e1, e2, TInt _) -> 
	let op = translate_rel_binop (typeOf e1) (typeOf e2) o in
	  K.BinOp (op, translate_exp e1, translate_exp e2)
	    
    | BinOp (Le, e1, e2, (TInt _ as t)) -> translate_exp (BinOp (Ge, e2, e1, t))
    | BinOp (Lt, e1, e2, (TInt _ as t)) -> translate_exp (BinOp (Gt, e2, e1, t))
    | BinOp (Ge, e1, e2, (TInt _ as t)) -> K.UnOp (K.Not, translate_exp (BinOp (Gt, e2, e1, t)))
    | BinOp (Ne, e1, e2, (TInt _ as t)) -> K.UnOp (K.Not, translate_exp (BinOp (Eq, e2, e1, t)))
	
    (* Pointer / Integer addition *)
    | BinOp (IndexPI, e1, e2, t) | BinOp (PlusPI, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar K.Ptr -> 
	      let sz = size_of_subtyp t in
		K.BinOp (K.PlusPI, translate_exp e1,
			 K.BinOp (K.MultI, translate_exp e2, K.exp_of_int sz))
	  | K.Scalar K.FunPtr ->
	      error "Npkcompile.translate_exp"
		"pointer arithmetics forbidden on function pointers"
	  | _ -> error "Npkcompile.translate_exp" "data pointer type expected"
      end
	
    (* Pointer / Integer subtraction *) 
    | BinOp (MinusPI, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar K.Ptr -> 
	      let sz = size_of_subtyp t in
	      let v1 = translate_exp e1 in
	      let v2 = K.BinOp (K.MultI, translate_exp e2, K.exp_of_int sz) in
		K.BinOp (K.PlusPI, v1,
			 K.BinOp (K.MinusI, K.exp_of_int 0, v2))
	  | K.Scalar K.FunPtr ->
	      error "Npkcompile.translate_exp"
		"pointer arithmetics forbidden on function pointers"
	  | _ ->
	      error "Npkcompile.translate_exp" "data pointer type expected"
      end
	
    (* Pointer difference *) 
    | BinOp (MinusPP, e1, e2, t) -> begin
	match translate_typ (typeOf e1), translate_typ (typeOf e2), translate_typ t with
	  | K.Scalar K.Ptr, K.Scalar K.Ptr, K.Scalar K.Int int_t -> 
	      let v1 = translate_exp e1 in
	      let v2 = translate_exp e2 in
		K.make_int_coerce int_t (K.BinOp (K.MinusPP, v1, v2))
	  | _ -> error "Npkcompile.translate_exp" "data pointer type expected"
      end
	
    | Lval lv -> begin
	match (translate_typ (typeOfLval lv)) with
	    K.Scalar s -> K.Lval (translate_lval lv, s)
	  | _ -> error "Npkcompile.translate_exp"
	      ("scalar type expected for left value "^(string_of_lval lv))
      end
	
    | AddrOf lv -> begin
	match lv, translate_typ (typeOf e) with
	  | (Var f, NoOffset), K.Scalar K.FunPtr ->
	      K.AddrOfFun f.vname
	      
	  | _, K.Scalar K.Ptr ->
	      let (lv', offs) = removeOffsetLval lv in begin
		  match offs with 
		    | Index (e, NoOffset) -> begin
			match translate_typ (typeOfLval lv') with
			  | K.Array (t_elt, len) ->
			      let sz = K.size_of t_elt in
				K.BinOp (K.PlusPI, K.AddrOf (translate_lval lv', len * sz),
					 K.BinOp (K.MultI, K.make_belongs len (translate_exp e), K.exp_of_int sz))
			  | _ -> error "Npkcompile.translate_exp" "unexpected error"
		      end
			
		    | _ -> let sz = size_of (typeOfLval lv) in
			K.AddrOf (translate_lval lv, sz)
		end
							 
	  | _ -> error "Npkcompile.translate_exp"
	      ("unexpected left value in AddrOf "^(string_of_lval lv))
      end
	
    (* These patterns are deleted during the 1st pass *)
    | StartOf _ 
	
    (* All the patterns remaining are not completely handled *)
    | BinOp (Eq, _, _, _) | BinOp (Ne, _, _, _)
    | BinOp (Gt, _, _, _) | BinOp (Ge, _, _, _)
    | BinOp (Lt, _, _, _) | BinOp (Le, _, _, _)
    | BinOp (LAnd, _, _, _) | BinOp (LOr, _, _, _)
    | AlignOf _ | AlignOfE _ ->
	error "Npkcompile.translate_exp"
	  ("expression '"^(string_of_exp e)^"' not handled")
	
	
	
and translate_stmtkind status kind =
  update_loc (get_stmtLoc kind);
  let loc = !cur_loc in
    match kind with
      | Instr il -> translate_instrlist il
	  
      | Return (None, _) ->
	  [K.Goto status.return_lbl, loc]
	    
      | Return (Some e, _) ->
	  let typ = translate_typ (typeOf e) in
	  let lval = get_ret_var status in
	    [translate_set lval typ e, loc;
	     K.Goto status.return_lbl, loc]

      | If (e, blk1, blk2, _) ->
	  translate_if status e blk1.bstmts blk2.bstmts
	    
      | Block b -> translate_stmts status b.bstmts
	  
      | Loop (body, _, _, _) ->
	  let status = new_brk_status status in
	  let loop = K.InfLoop (translate_stmts status body.bstmts), loc in
	  let lbl = (K.Label status.brk_lbl, loc) in
	    [loop;lbl]
	      
	      
      | Break _ -> [K.Goto status.brk_lbl, loc]
	  
      | Switch (e, body, stmt_list, _) ->
	  translate_switch status e stmt_list body
	    
      | Goto (x, _) ->
	  let rec explore_labels l =
	    match l with
		[] -> error "Npkcompile.translate_stmtkind" "unexpected goto"
	      | (Label (s, loc, false) as l)::r -> begin
		  try
		    translate_stmts status (Hashtbl.find code_to_duplicate l)
		  with Not_found -> explore_labels r
		end
	      | lbl::r -> explore_labels r
	  in
	    explore_labels (!x.labels)
	      
      | Continue _ | TryFinally _ | TryExcept _ ->
	  error "Npkcompile.translate_stmtkind" "stmtkind not handled"
	  
	  

	  
and translate_stmts status stmts =
  match stmts with
      [] -> []
    | stmt::r ->
	let labels = extract_labels status stmt.labels in
	let first = translate_stmtkind status stmt.skind in
	  labels@first@(translate_stmts status r)
	    

and translate_instrlist il =
  match il with
      [] -> []
    | i::r ->
	let i = translate_instr i in
	  i@(translate_instrlist r)
	    
	    
and translate_instr i =
  update_loc (get_instrLoc i);
  match i with
    |	Set (lv, e, _) ->
	  let lval = translate_lval lv in
	  let typ = translate_typ (typeOfLval lv) in
	    [translate_set lval typ e, !cur_loc]

    | Call (x, Lval lv, args, _) ->
	translate_call x lv args
	  
    | Call (_, _, _, _) ->
	error "Npkcompile.translate_instr"
	  ("call '"^(string_of_instr i)^"' not handled")
    | Asm (_, _, _, _, _, _) ->
	error "Npkcompile.translate_instr" "Asm block not supported"
	  

	  
	  
and translate_set lval typ e =
  match typ with
    | K.Scalar sca ->
	let exp = translate_exp e in
	  K.Set (lval,exp,sca)
	    
    | K.Region (_, sz) -> begin
	match e with
	  | Lval lv_src ->
	      let src = translate_lval lv_src in
		K.Copy (lval, src, sz)
	  | _ ->
	      error "Npkcompile.translate_set"
		("left value expected instead of '"^(string_of_exp e)^"'")
      end
	
    | _ -> error "Npkcompile.translate_set" "invalid type"
	

and translate_if status e stmts1 stmts2 =
  let if_loc = !cur_loc in
  let (e, t) = 
    match translate_typ (typeOf e) with
	K.Scalar (K.Int _ as t) -> (e, t)
      | K.Scalar (K.Ptr|K.FunPtr as t) -> (BinOp (Ne, e, null, intType), t)
      | _ ->
	  error "Npkcompile.translate_if"
	    ("bad expression '"^(string_of_exp e)^"'")
  in
  let rec normalize e =
    match e with
	(* TODO: beware is the type t here really the same ? *)
      | K.UnOp (K.Not, K.UnOp (K.Not, e)) -> normalize e

      | K.UnOp (K.Not, K.BinOp ((K.Gt _|K.Eq _), _, _))
      | K.BinOp ((K.Gt _|K.Eq _), _, _) -> e

      | K.UnOp (K.Not, e) -> K.BinOp (K.Eq t, e, K.zero)
      | _ -> K.UnOp (K.Not, K.BinOp (K.Eq t, K.zero, e))
  in
    let cond1 = normalize (translate_exp e) in
    let cond2 = K.negate cond1 in
    let body1 = translate_stmts status stmts1 in
    let body2 = translate_stmts status stmts2 in
      [K.ChooseAssert [([cond1], body1); ([cond2], body2)], if_loc]


(** Returns a set of blocks, each representing a choice of the case 
    statement. Each case is translated by appending a guard.
    It also builds the guard of the default statement. *)
and translate_switch status e stmt_list body =
  let loc = !cur_loc in
  let switch_exp = translate_exp e in

  let cases = ref [] in

  let def_asserts = ref [] in
  let status = ref (new_brk_status status) in
  let def_goto = ref (K.Goto !status.brk_lbl, loc) in

  let collect_label label =
    match label with
	Case (_, loc) | Default loc
	    when mem_switch_label !status loc -> ()
      | Case (v, loc) ->
	  let t = 
	    match translate_typ (typeOf v) with
		K.Scalar i -> i
	      | _ -> error "Npkcompile.tranlate_switch" "expression not scalar"
	  in
	  let case_exp = K.BinOp (K.Eq t, switch_exp, translate_exp v) in
	  let def_exp = K.negate case_exp in
	  let new_lbl = new_label () in
	    status := add_switch_label !status loc new_lbl;
	    def_asserts := def_exp::!def_asserts;
	    cases := ([case_exp], [K.Goto new_lbl, translate_loc loc]):: (!cases)
      | Default loc ->
	  let new_lbl = new_label () in
	    status := add_switch_label !status loc new_lbl;
	    def_goto := (K.Goto new_lbl, translate_loc loc);
	    ()
      | _ -> error "Npkcompile.tranlate_switch" "invalid label"
  in

  let collect_labels s = List.iter collect_label s.labels in

    List.iter collect_labels stmt_list;
    let choices = List.rev (!cases) in
    let default_choice = (List.rev (!def_asserts), [!def_goto]) in
    let body = translate_stmts !status body.bstmts in
      ((K.ChooseAssert (default_choice::choices), loc)::body)@[K.Label !status.brk_lbl, loc]



and translate_call x lv args_exps =
  let loc = !cur_loc in

  let handle_retval fname ret_type =
    match ret_type, x with
	(* No return value *)
	None, None -> [], []

      (* Return value ignored *)
      | Some t, None ->
	  push_local ();
	  [("value_of_"^fname, t, loc)], []
	    
      (* Return value put into Lval cil_lv *)
      | Some t_given, Some cil_lv ->
	  let t_expected = translate_typ (typeOfLval cil_lv) in
	    push_local ();
	    let lval = translate_lval cil_lv in
	    let ret_decl = ["value_of_"^fname, t_given, loc] in
	    let ret_epilog = match t_given, t_expected with
	      | K.Scalar s_giv, K.Scalar s_exp when s_giv = s_exp ->
		  [K.Set (lval, K.Lval (K.Local 0, s_giv), s_exp), loc]
		    
	      | K.Scalar (K.Int _ as s_giv), K.Scalar (K.Int int_t as s_exp) ->
		  let exp = K.make_int_coerce int_t (K.Lval (K.Local 0, s_giv)) in
		    [K.Set (lval, exp, s_exp), loc]
		      
	      | K.Region (desc1, sz1), K.Region (desc2, sz2)
		  when sz1 = sz2 && desc1 = desc2 ->
		  [K.Copy (lval, K.Local 0, sz1), loc]
		    
	      | _ -> error "Npkcompile.translate_call" "invalid implicit cast"
	    in
	      (ret_decl, ret_epilog)
		
      | _ ->
	  error "Npkcompile.translate_call"
	    ("function "^fname^" has return type void")
  in

  let rec handle_args_decls fname formals exps =
    let rec handle_args_decls_aux accu i formals exps =
      match formals, exps with
	| None, []  -> accu
	| None, e::r_e ->
	    push_local ();
	    let t = translate_typ (typeOf e) in
	      handle_args_decls_aux 
		(("arg"^(string_of_int i), t, loc)::accu)
		(i+1) None r_e
		
	| Some [], [] -> accu
	| Some ((n, t_expected)::r_t), e::r_e ->
	    let t_given = translate_typ (typeOf e) in
	      if t_expected <> t_given then begin
		error "Npkcompile.translate_call"
		  ("type mismatch on call ('"^(K.string_of_typ t_given)^"' <> '"
		   ^(K.string_of_typ t_expected)^"') in "^fname^" arguments")
	      end;
	      push_local ();
	      handle_args_decls_aux ((n, t_expected, loc)::accu) (i+1) (Some r_t) r_e
		
	| _,_ ->
	    error "Npkcompile.translate_call"
	      ("function "^fname^" called with incorrect number of args")
    in
      handle_args_decls_aux [] 0 formals exps
  in

  let rec handle_args_prolog accu n i args_exps =
    match args_exps with
	[] -> accu
      | e::r_e ->
	  let t = translate_typ (typeOf e) in
	  let lval = K.Local ((n-i) - 1) in
	    handle_args_prolog ((translate_set lval t e, !cur_loc)::accu) n (i+1) r_e
  in

    save_loc_cnt ();
    let name, ret_type, formals = match lv with
      | Var f, NoOffset ->
	  let name = f.vname in
	  let _ = match f.vtype with
	    | TFun (ret, _, _, _) ->
		let arg_from_exp e = ("", typeOf e, []) in begin
		    try update_fun_proto name ret (Some (List.map arg_from_exp args_exps))
		    with invalid_argument ->
		      (* TODO: See if we can be as specific as before (int4 <> ptr) *)
		      error "Npkcompile.translate_call" 
			("function "^name^" called with args not matching prototype")
		  end
		  
	    | _ ->
		error "Npkcompile.translate_call"
		  ("invalid type '"^(string_of_type f.vtype)^"'")
	  in 
	  let x = Hashtbl.find fun_specs name in
	  let fs = match x.pargs with
	    | None -> None
	    | Some ld -> Some (List.map extract_ldecl ld)
	  in

	    (name, x.prett, fs)

      | Mem (Lval fptr), NoOffset ->
	  let typ = translate_typ (typeOfLval fptr) in
	    if typ <> K.Scalar K.FunPtr
	    then error "Npkcompile.translate_call" "FunPtr expected";

	    let ret = match x with
	      | None -> None
	      | Some cil_lv -> Some (translate_typ (typeOfLval cil_lv))
	    in
	    let (_, line, _) = loc in
	      ("fptr_called_line_" ^ (string_of_int line), ret, None)

      | _ -> error "Npkcompile.translate_call" "Left value not supported"
    in

    let ret_decl, ret_epilog = handle_retval name ret_type in
    let args_decls = handle_args_decls name formals args_exps in
    let args_prolog = handle_args_prolog [] (List.length args_exps) 0 args_exps in

    let res = match lv with
      | Var f, NoOffset ->
	  let call_w_prolog = (K.Call (K.FunId f.vname), loc)::args_prolog in
	  let call_wo_ret = generate_body (List.rev call_w_prolog) args_decls in
	    generate_body (call_wo_ret@(ret_epilog)) ret_decl
      | Mem (Lval fptr), NoOffset ->
	  let args_t = List.rev (List.map (fun (_, t, _) -> t) args_decls) in
	  let fptr_exp = K.Lval (translate_lval fptr, K.FunPtr) in
	    
	  let call_w_prolog = (K.Call (K.FunDeref (fptr_exp, (args_t, ret_type))), loc)::args_prolog in
	  let call_wo_ret = generate_body (List.rev call_w_prolog) args_decls in
	    generate_body (call_wo_ret@(ret_epilog)) ret_decl
      | _ -> error "Npkcompile.translate_call" "Left value not supported"
	  
    in
      restore_loc_cnt ();
      res





let translate_fun name spec =
  match spec.pargs, spec.plocs, spec.pcil_body with
      Some formals, Some locals, Some cil_body ->
	cur_loc := spec.ploc;
	let floc = !cur_loc in
	let status = 
	  match spec.prett with
	    | None -> empty_status ()
	    | Some _ ->
		push_local ();
		new_ret_status ()
	in
	  List.iter (loc_declare false) formals;
	  List.iter (loc_declare true) locals;
	  
	  let blk = K.simplify((translate_stmts status cil_body.bstmts)@
				 [K.Label status.return_lbl, floc]) in
	  let body = generate_body blk (get_loc_decls ()) in

	    (* TODO ?: Check only one body exists *)
	    spec.pcil_body <- None;
	    spec.pbody <- Some body(* pbody should only be Newspeak.blk *)



    (* TODO: Same question here *)
    | _ -> ()








(*=========================================*)
(* compile function, which wraps translate *)
(*=========================================*)

let compile in_name out_name  =
  if not (Filename.check_suffix in_name K.c_suffix)
  then error "Npkcompile.compile" (in_name^" is not a .c file");

  initCIL ();
  useLogicalOperators := false;

  print_debug ("Parsing "^in_name^"...");
  let cil_file = Frontc.parse in_name () in
    print_debug ("Parsing done.");
    if !verb_cil then begin
      print_newline ();
      print_endline ("Cil output for "^in_name);
      for i = 1 to String.length in_name do
	print_string "-"
      done;
      print_endline "---------------";
      dump stdout cil_file;
      print_newline ();
      print_newline ()
    end;

    print_debug "Running first pass...";
    update_loc locUnknown;
    first_pass cil_file;
    update_loc locUnknown;
    print_debug "First pass done.";
  
    print_debug ("Translating "^in_name^"...");
    Hashtbl.iter translate_fun fun_specs;

    let npko = {ifilename = in_name;
		iglobs = Hashtbl.copy glb_decls;
		ifuns = Hashtbl.copy fun_specs;
		iusedglbs = !glb_used; iusedcstr = !glb_cstr;
		iusedfuns = !fun_called;} in

      init_env ();
      
      if (!verb_npko) then begin
	
	print_endline "Newspeak Object output";
	print_endline "----------------------";
	dump_npko npko;
	print_newline ();
      end;
      
      update_loc locUnknown;
      if (out_name <> "") then begin
	print_debug ("Writing "^(out_name)^"...");
	let ch_out = open_out_bin out_name in
	  Marshal.to_channel ch_out "NPKO" [];
	  Marshal.to_channel ch_out npko [];
	  close_out ch_out;
	  print_debug ("Writing done.");
      end;
      
      init_env();
      npko











(*


(* TODO: add these as #pragma assume in the code, a lot easier *)
(*let create_assumption str =
  let lexer = Genlex.make_lexer [">="] (Stream.of_string str) in
    try
      let x = 
	match Stream.next lexer with
	    Genlex.Ident id -> id
	  | _ -> raise Exit
      in
      let _ = 
	match Stream.next lexer with
	    Genlex.Kwd ">=" -> ()
	  | _ -> raise Exit
      in
      let n =  
	match Stream.next lexer with
	    Genlex.Int n -> n
	  | _ -> raise Exit
      in
      let v = Env.get_glb_var x in
      let t = Env.get_glb_typ x in
	(K.BinOp (K.Ge t, K.Lval (v, t), K.Const (K.CInt64 (Int64.of_int n))))
    with _ -> error ("Unknown assumption: "^str)*)




let translate () =
    (* Finally, our work with the globals is finished *)
    print_debug "Generating global declarations...";
    let glb_decls = get_glb_decls_inits translate_exp in
      update_loc locUnknown;
      print_debug "Declarations generated.";
      
      (* Here takes place the translation of the functions, from a name ->
	 specs hash table to a K.fid -> K.fundec hash_table *)
      let add_fun name spec =
	print_debug ("Translating function "^name^"...");
	let fun_body = translate_fun name spec in
	let extract_args_typs decl_list = match decl_list with
	    None -> []
	  | Some l -> List.map extract_type l
	in
	let res = name, ((extract_args_typs spec.formals, spec.ret_type), fun_body) in
	  print_debug ("Function "^name^" translated.");
	  res
      in
      let fun_defs = map_fun_specs add_fun in

(*      let assumptions = List.map create_assumption !Npkcontext.assumptions in

	(assumptions, glb_decls, fun_defs)*)

	([], glb_decls, fun_defs)









(*=====================================*)
(* cil2newspeak, which wraps translate *)
(*=====================================*)

(*
let cil2newspeak fnames = 

  let get_cilfile name =
    let (is_c, cil_output) =
      try 
	if (String.sub name ((String.length name) - 2) 2) = ".c"
	then (true, name^"il")
	else if (String.sub name ((String.length name) - 4) 4) = ".cil"
	then (false, name)
	else error ("File '"^name^"' is not a .c neither a .cil file");
      with Invalid_argument _ -> error ("File '"^name^"' is not a .c neither a .cil file");
    in
      if not is_c && !compile_only
      then print_warning ("Nothing to do for '"^name^"'")
      else begin
	let cilfile = 
	  if is_c then begin
	    print_debug ("Parsing "^name^"...");
	    let tmp = Frontc.parse name () in
	      print_debug ("Parsing done.");
	      if !verb_cil then begin
		print_endline ("Cil output for "^name);
		for i = 1 to String.length name do
		  print_string "-"
		done;
		print_endline "---------------";
		dump stdout tmp;
		print_endline "";
	      end;
	      tmp
	  end else
	    let ch_in = open_in_bin name in
	      print_debug ("Importing "^name^"...");
	      let tmp = Marshal.from_channel ch_in in
		print_debug ("Importing done.");
		tmp
	in
	  if !compile_only then  begin
	    let ch_out = open_out_bin cil_output in
	      print_debug ("Exporting "^cil_output^"...");
	      Marshal.to_channel ch_out cilfile [];
	      print_debug ("Exporting done.")
	  end else cilfiles := cilfile::(!cilfiles)
      end
  in
    List.iter get_cilfile fnames;
    if !cilfiles = [] then exit 0;

    let kernel = 
      if !mergecil then begin
	(*  MergeCil  *)
	let file = Mergecil.merge !cilfiles "File" in    
	  if !Errormsg.hadErrors then begin 
	    if not !ignores_cil_merge_errors
	    then error "fnames: Errors during merge phase";
	    prerr_newline ();
	  end;
	  Rmtmps.removeUnusedTemps file;
	  
	  if !verb_cil then begin
	    print_endline "Cil output";
	    print_endline "----------";
	    dump stdout file;
	    print_endline "";
	  end;
	  translate [file]
      end else begin
	List.iter my_remove_tmp !cilfiles;
	translate !cilfiles
      end
    in

      kernel
*)
*)
 




(*
let declare fnames =
  let total = List.length fnames in
  let processed = ref 0 in
  let declare name =
    let cin = open_in_bin name in
    let glb_decls = Marshal.from_channel cin in
    let (fun_decls, proto_decls) = Marshal.from_channel cin in
      close_in cin;
      (* TODO: this is a hack write again in a clean way *)
      List.iter (fun (g, loc) -> update_loc loc; glb_declare g) glb_decls;
      List.iter fun_declare fun_decls;
      (* TODO: this is a hack write again in a clean way *)
      List.iter (fun (g, loc) -> update_loc loc; fun_declare_prototype g) 
	proto_decls;
      if !verb_debug then begin
	processed := !processed + 1;
	let progress = !processed*100/total in
	  prerr_string ("Progress: "^(string_of_int progress)^"%\n")
      end
  in
    update_loc locUnknown;
    print_debug "Collecting globals and function declarations...";
    List.iter declare fnames;
    print_debug "Collection of globals and function declarations done."


let gen_ir name =
  if not (Filename.check_suffix name ".c") 
  then error ("File '"^name^"' is not a .c file.");
  print_debug ("Parsing "^name^"...");
  
  let cilfile = Frontc.parse name () in
    print_debug ("Parsing done.");
    if !verb_cil then begin
      print_endline ("Cil output for "^name);
      print_string (String.make (String.length name) '-');
      print_endline "---------------";
      dump stdout cilfile;
      print_newline ();
    end;

    my_remove_tmp cilfile;
    let (glbs, funs) = Local_pass.translate cilfile.globals in

    let name = (Filename.chop_extension name)^".ir" in
    let cout = open_out_bin name in
      print_debug ("Exporting "^name^"...");
      Marshal.to_channel cout glbs [];
      Marshal.to_channel cout funs [];
      close_out cout;
      print_debug ("Exporting done.");
      
      name
*)
