open Cil
open Cilutils

open Npkcontext
(* open Npkutils *)
(* open Env *)
(* open Local_pass *)

module K = Newspeak



(*
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
	with Not_found -> error "Cil2newspeak.translate_labels: unexpected label"
      end
    | (Label (_, _, false))::r -> extract_aux r
    | _ -> error "Cil2newspeak.translate_labels: invalid label"
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
      | decl::r ->
	  generate_aux [K.Decl (decl.var_decl, body), decl.var_loc] r
  in generate_aux body decls






(* TODO: factor out print_warnings by putting together the warnings and 
   selecting which should be verb_warnings or not *)

(* Exploration of Cil's "globals" *)
let explore g = 
  update_loc (get_globalLoc g);
  match g with
    | GType (t, _) ->
	if !verb_warnings then print_warning ("skip typedef "^t.tname)
    | GEnumTag (info, _) -> 
	if !verb_warnings then print_warning ("skip enum "^info.ename)
    | GCompTag (c, _) -> 
	if !verb_warnings then print_warning ("skip composite typedef "^c.cname)
    | GCompTagDecl (c, _) -> 
	if !verb_warnings then print_warning ("skip composite declaration "^c.cname)
    | GPragma (a, _) when !ignores_pragmas -> 
	print_warning ("Directive ignored: "
		       ^"unknown #pragma "^(string_of_attribute a))
	  
    | GVarDecl ({vname = name; vtype = TFun (ret,args,_,_)}, _) ->
	fun_declare_prototype (name, ret, args)
	  
    | GFun (f, _) -> 
	if (f.svar.vname = "main") then begin
	  let (ret, args) = 
	    match unrollType f.svar.vtype with
		TFun (ret, Some args, _, []) -> (ret, args)
	      | _ -> error ("Cil2newspeak.translate.explore: "
			    ^"main, should have a function type")
	  in
	    if (!verb_warnings) 
	      && (unrollType ret <> TInt (IInt, [])) then
		print_warning "return type of 'main' is not 'int'";
	    match args with
		[] -> ()
	      | (_, arg1, [])::(_, arg2, [])::[] 
		  when (unrollType arg1 = TInt (IInt, []))
		    && (unrollTypeDeep arg2 = 
			TPtr (TPtr (TInt (IChar, []), []), []))
		    -> ()
	      | _ -> error ("Invalid argument types for main: "
			    ^"authorized forms are main() and"
			    ^" main(int, char**)")
	end;
	fun_declare f
	  
    | GVarDecl (v, _) -> glb_declare (v, false, None)
	
    | GVar (v, {init = i}, _) -> glb_declare (v, true, i)
	
    | _ -> error ("Cil2newspeak.translate.explore: global "
		  ^(string_of_global g)^" not supported")

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
	  print_warning ("No string representation available for const "^s);
	  K.Const (K.CFloat (f, s))
	
    | CWStr _ | CEnum _
	-> error ("Cil2newspeak.translate.translate_const")

	
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
		print_warning ("cast from float to unsigned integer: "
			       ^"sign may be lost: "
			       ^(string_of_type (typeOf e))^"' -> '"
			       ^(string_of_type t)^"' in '"
			       ^(string_of_exp (CastE (t, e)))^"'");
	      K.UnOp (K.Cast (kt, kt'), translate_exp e)
		
	  | K.Scalar K.Ptr, K.Scalar K.Ptr -> translate_exp e
	  | K.Scalar K.FunPtr, K.Scalar K.FunPtr ->
	      print_warning ("probable dangerous cast: '"^(string_of_type (typeOf e))^"' -> '"
			     ^(string_of_type t)^"' in '"^(string_of_exp (CastE (t,e)))^"'");
	      translate_exp e
		
	  | (K.Scalar (K.Int (sign, sz)), K.Scalar K.Ptr)
	      when sz = pointer_size && !castor_allowed ->
	      print_warning ("probable invalid cast '"^(string_of_type (typeOf e))^"' -> '"
			     ^(string_of_type t)^"' in '"^(string_of_exp (CastE (t,e)))^"'");
		K.UnOp (K.PtrToInt (sign, sz), translate_exp e)
		  
	  | (K.Scalar K.Ptr, K.Scalar (K.Int (_, sz) as int_t))
	      when sz = pointer_size && !castor_allowed ->
	      print_warning ("probable invalid cast '"^(string_of_type (typeOf e))^"' -> '"
			     ^(string_of_type t)^"' in '"^(string_of_exp (CastE (t,e)))^"'");
		K.UnOp (K.Cast (int_t, K.Ptr), translate_exp e)
		  
	  | K.Scalar (K.Ptr as kt'), K.Scalar (K.FunPtr as kt) 
	      when !castor_allowed ->
	      print_warning ("probable invalid cast '"
			     ^(string_of_type (typeOf e))^"' -> '"
			     ^(string_of_type t)^"' in '"
			     ^(string_of_exp (CastE (t,e)))^"'");
		K.UnOp (K.Cast (kt, kt'), translate_exp e)
		  
	  | _ -> error ("translate cast: Invalid cast '"^(string_of_type (typeOf e))^"' -> '"
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
		match translate_typ t with
		  | K.Array (t_elt, len) ->
		      let index_exp =
			K.BinOp (K.MultI, K.make_belongs len (translate_exp e),
				 K.exp_of_int (K.size_of t_elt))
		      in
			K.Shift (translate_lval lv', index_exp)
			  
		  | _ -> error ("Cil2newspeak.translate.translate_lval: array expected")
	      end
	    | _ -> error ("Cil2newspeak.translate.translate_lval: offset not handled")
		
		
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
	  | _ -> error ("Cil2newspeak.translate.translate_exp: integer or float type expected")
      end
	
    | UnOp (BNot, e, t) -> begin
	match translate_typ t with
	    K.Scalar (K.Int int_t) -> 
	      let b = K.domain_of_typ int_t in
		K.UnOp (K.BNot b, translate_exp e)
	  | _ -> error "Cil2newspeak.translate.translate_exp: integer type expected"
      end
	
    | UnOp (LNot, e, t) -> begin
	match translate_typ t with
	    K.Scalar (K.Int int_t) -> K.UnOp (K.Not, translate_exp e)
	  | _ -> error "Cil2newspeak.translate.translate_exp: integer type expected"
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
	  | _ -> error ("Cil2newspeak.translate.translate_exp: integer or float type expected")
      end
	
    (* Bitwise operations *)
    | BinOp (BAnd as o, e1, e2, t)    | BinOp (BOr as o, e1, e2, t)
    | BinOp (BXor as o, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar (K.Int int_t) ->
	      K.BinOp (translate_logical_binop int_t o,
		       translate_exp e1, translate_exp e2)
	  | _ -> error ("Cil2newspeak.translate.translate_exp: integer type expected")
      end
	
    (* Logical operations *)
    | BinOp (Shiftlt as o, e1, e2, t) | BinOp (Shiftrt as o, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar (K.Int int_t) ->
	      K.make_int_coerce int_t (K.BinOp (translate_logical_binop int_t o,
						translate_exp e1, translate_exp e2))
	  | _ -> error ("Cil2newspeak.translate.translate_exp: integer type expected")
      end
	
    (* Equality and inequality, comparisons : between numbers or pointers *)
    | BinOp ((Eq|Ne|Ge|Gt) as o, e1, e2, TInt _) -> 
	let op = translate_rel_binop (typeOf e1) (typeOf e2) o in
	  K.BinOp (op, translate_exp e1, translate_exp e2)
	
    | BinOp (Le, e1, e2, t) -> translate_exp (BinOp (Ge, e2, e1, t))
						
    | BinOp (Lt, e1, e2, t) -> translate_exp (BinOp (Gt, e2, e1, t))
	
    (* Pointer / Integer addition *)
    | BinOp (IndexPI, e1, e2, t) | BinOp (PlusPI, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar K.Ptr -> 
	      let sz = size_of_subtyp t in
		K.BinOp (K.PlusPI, translate_exp e1,
			 K.BinOp (K.MultI, translate_exp e2, K.exp_of_int sz))
	  | K.Scalar K.FunPtr ->
	      error ("Cil2newspeak.translate.translate_exp: pointer "^
		       "arithmetics forbidden on function pointers")
	  | _ ->
	      error ("Cil2newspeak.translate.translate_exp: data pointer type expected")
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
	      error ("Cil2newspeak.translate.translate_exp: pointer "^
		       "arithmetics forbidden on function pointers")
	  | _ ->
	      error ("Cil2newspeak.translate.translate_exp: data pointer type expected")
      end
	
    (* Pointer difference *) 
    | BinOp (MinusPP, e1, e2, t) -> begin
	match translate_typ (typeOf e1), translate_typ (typeOf e2), translate_typ t with
	  | K.Scalar K.Ptr, K.Scalar K.Ptr, K.Scalar K.Int int_t -> 
	      let v1 = translate_exp e1 in
	      let v2 = translate_exp e2 in
		K.make_int_coerce int_t (K.BinOp (K.MinusPP, v1, v2))
	  | _ ->
	      error ("Cil2newspeak.translate.translate_exp: data pointer type expected")
      end
	
    | Lval lv -> begin
	match (translate_typ (typeOfLval lv)) with
	    K.Scalar s -> K.Lval (translate_lval lv, s)
	  | _ -> error ("Cil2newspeak.translate.translate_exp: "
			^"scalar type expected for left value "
			^(string_of_lval lv))
      end
	
    | AddrOf lv -> begin
	match lv, translate_typ (typeOf e) with
	  | (Var f, NoOffset), K.Scalar K.FunPtr -> begin
	      match get_fun_spec f.vname with
		  None -> error ("Cil2newspeak.translate.translate_exp: "
				 ^"unknown function "^f.vname)
		| Some _ -> K.AddrOfFun f.vname
	    end
	      
	  | _, K.Scalar K.Ptr ->
	      let (lv', offs) = removeOffsetLval lv in begin
		  match offs with 
		    | Index (e, NoOffset) -> begin
			match translate_typ (typeOfLval lv') with
			  | K.Array (t_elt, len) ->
			      let sz = K.size_of t_elt in
				K.BinOp (K.PlusPI, K.AddrOf (translate_lval lv', len * sz),
					 K.BinOp (K.MultI, K.make_belongs len (translate_exp e), K.exp_of_int sz))
			  | _ -> error "Cil2newspeak.translate.translate_exp"
		      end
			
		    | _ -> let sz = size_of (typeOfLval lv) in
			K.AddrOf (translate_lval lv, sz)
		end
							 
	  | _ -> error("Cil2newspeak.translate.translate_exp: "
		       ^"unexpected left value in AddrOf "
		       ^(string_of_lval lv))
      end
	
    (* These patterns are deleted during the 1st pass *)
    | StartOf _ 
	
    (* All the patterns remaining are not completely handled *)
    | BinOp (Eq, _, _, _) | BinOp (Ne, _, _, _)
    | BinOp (Gt, _, _, _) | BinOp (Ge, _, _, _)
    | BinOp (LAnd, _, _, _) | BinOp (LOr, _, _, _)
    | AlignOf _ | AlignOfE _
	-> error ("Cil2newspeak.translate.translate_exp: \""^(string_of_exp e)^"\"")
	
	
	
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
		[] -> error ("Cil2newspeak.translate.translate_stmt.translate_stmtkind: "
			     ^"unexpected goto");
	      | (Label (s, loc, false) as l)::r -> begin
		  try
		    translate_stmts status (Hashtbl.find code_to_duplicate l)
		  with Not_found -> explore_labels r
		end
	      | lbl::r -> explore_labels r
	  in
	    explore_labels (!x.labels)
	      
      | Continue _ | TryFinally _ | TryExcept _ ->
	  error "Cil2newspeak.translate.translate_stmt.translate_stmtkind";
	  
	  

	  
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
	error ("Cil2newspeak.translate.translate_instr: bad call \""
	       ^(string_of_instr i)^"\"");
    | Asm (_, _, _, _, _, _) ->
	  error ("Cil2newspeak.translate.translate_instr: Asm block not supported")
	    

	    
	    
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
	    | _ -> error ("Cil2newspeak.translate.translate_set: left value expected \""
			  ^(string_of_exp e)^"\"")
      end
	  
    | _ -> error "Cil2newspeak.translate.translate_set: invalid type"
	

(* TODO: either remove Not or remove Ne. Need to choose. *)
and translate_if status e stmts1 stmts2 =
  let if_loc = !cur_loc in
  let (e, t) = 
    match translate_typ (typeOf e) with
	K.Scalar (K.Int _ as t) -> (e, t)
      | K.Scalar (K.Ptr|K.FunPtr as t) -> (BinOp (Ne, e, null, intType), t)
      | _ -> error ("Cil2newspeak.translate.translate_if: bad expression \""
		    ^(string_of_exp e)^"\"")
  in
  let rec normalize e =
    match e with
	(* TODO: beware is the type t here really the same ? *)
      | K.UnOp (K.Not, e) -> K.negate (normalize e)
      | K.BinOp ((K.Ge _|K.Gt _|K.Eq _|K.Ne _), _, _) -> e
      | _ -> K.BinOp (K.Ne t, e, K.zero)
  in
  let cond1 = normalize (translate_exp e) in
  let cond2 = K.negate cond1 in
  let body1 = translate_stmts status stmts1 in
  let body2 = translate_stmts status stmts2 in
    [K.ChooseAssert [([cond1], body1); ([cond2], body2)], if_loc]

(*
  let if_loc = !cur_loc in
  let cond1 = translate_bool_exp e in
  let cond2 = K.negate cond1


  match e with
    | UnOp (LNot, e', _) -> translate_if status e' stmts2 stmts1
	
    | BinOp (Ge, _, _, _) | BinOp (Gt, _, _, _) | BinOp (Le, _, _, _) 
    | BinOp (Lt, _, _, _) | BinOp (Ne, _, _, _) | BinOp (Eq, _, _, _) ->
	  let if_loc = !cur_loc in
	    print_endline (Cilutils.string_of_exp e);
	  let cond1 = translate_exp e in
	    print_endline (K.string_of_exp cond1);
	  let cond2 = K.negate cond1 in
	    print_endline (K.string_of_exp cond2);
	  let body1 = translate_stmts status stmts1 in
	  let body2 = translate_stmts status stmts2 in
	    [K.ChooseAssert [([cond1],body1); ([cond2], body2)], if_loc]
	      
    | _ -> begin
	match translate_typ (typeOf e) with
	    | K.Scalar (K.Int _) ->
		let e = BinOp (Ne, e, CastE (typeOf e, zero), intType) in 
		  translate_if status e stmts1 stmts2
	    | K.Scalar K.Ptr | K.Scalar K.FunPtr ->
  translate_if status (BinOp (Ne, e, null, intType)) stmts1 stmts2
 	    | _ -> error ("Cil2newspeak.translate.translate_if: bad expression \""
			  ^(string_of_exp e)^"\"");
      end
*)

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
		| _ -> error "Env.add_cases: expression not scalar"
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
	| _ -> error ("Env.add_cases: invalid label")
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
	    [new_decl (t, ("value_of_" ^ fname), (K.Init [])) 0 loc true], []
	      
	(* Return value put into Lval cil_lv *)
	| Some t_given, Some cil_lv ->
	    let t_expected = translate_typ (typeOfLval cil_lv) in
	      push_local ();
	      let lval = translate_lval cil_lv in
	      let ret_decl = [new_decl (t_given, ("value_of_" ^ fname), (K.Init [])) 0 loc true] in
	      let ret_epilog = match t_given, t_expected with
		| K.Scalar s_giv, K.Scalar s_exp when s_giv = s_exp ->
		    [K.Set (lval, K.Lval (K.Local 0, s_giv), s_exp), loc]
		      
		| K.Scalar (K.Int _ as s_giv), K.Scalar (K.Int int_t as s_exp) ->
		    let exp = K.make_int_coerce int_t (K.Lval (K.Local 0, s_giv)) in
		      [K.Set (lval, exp, s_exp), loc]
			
		| K.Region (desc1, sz1), K.Region (desc2, sz2)
		    when sz1 = sz2 && desc1 = desc2 ->
		    [K.Copy (lval, K.Local 0, sz1), loc]
		      
		| _ -> error ("Cil2newspeak.translate.translate_call.handle_retval: invalid implicit cast")
	      in
		ret_decl, ret_epilog
		  
	| _ -> error ("Cil2newspeak.translate.translate_call.handle_retval: "
		      ^"function "^fname^" has return type void")
    in

    let rec handle_args_decls fname formals exps =
      let rec handle_args_decls_aux accu i formals exps =
	match formals, exps with
	  | None, []  -> accu
	  | None, e::r_e ->
	      push_local ();
	      let t = translate_typ (typeOf e) in
		handle_args_decls_aux 
		  ((new_decl (t, ("arg" ^ (string_of_int i)), (K.Init [])) 0 loc true)::accu)
		  (i+1) None r_e
		  
	  | Some [], [] -> accu
	  | Some (decl::r_t), e::r_e ->
	      let t_given = translate_typ (typeOf e) in
	      let t_expected = extract_type decl in
		if t_expected <> t_given then begin
		  error ("Cil2newspeak.translate.translate_call.handle_args_decls: "
			 ^"type mismatch ("^(K.string_of_typ t_given)^" <> "^(K.string_of_typ t_expected)^
			 ") in "^fname^" call are different");
		end;
		push_local ();
		handle_args_decls_aux ({decl with var_loc = loc}::accu) (i+1) (Some r_t) r_e
		  
	  | _,_ -> error ("Cil2newspeak.translate.translate_call.handle_args_decls: "
			  ^"function "^fname^" called with incorrect number of args")
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
      let res = match lv with
	| Var f, NoOffset ->
	    let spec = match get_fun_spec f.vname with
	      | None -> error ("Cil2newspeak.translate.translate_call: "
			       ^"Function not declared: "^f.vname)
	      | Some s -> s
	    in

	    let name = f.vname in
	    let ret_decl, ret_epilog = handle_retval name spec.ret_type in
	    let args_decls = handle_args_decls name spec.formals args_exps in
	    let args_prolog = handle_args_prolog [] (List.length args_exps) 0 args_exps in
	      
	    let call_w_prolog = (K.Call (K.FunId f.vname), loc)::args_prolog in
	    let call_wo_ret = generate_body (List.rev call_w_prolog) args_decls in

	      if (spec.formals = None)
	      then update_fun_spec f.vname None (Some args_decls) [] [] None;
	      generate_body (call_wo_ret@(ret_epilog)) ret_decl
		
	| Mem (Lval fptr), NoOffset ->
	    let typ = translate_typ (typeOfLval fptr) in
	      if typ <> K.Scalar K.FunPtr
	      then error "Cil2newspeak.translate.translate_call: FunPtr expected";
	      
	      let (_, line, _) = loc in
	      let name = "fptr_called_line_" ^ (string_of_int line) in
	      let ret_type = match x with
		| None -> None
		| Some cil_lv -> Some (translate_typ (typeOfLval cil_lv))
	      in
		
	      let ret_decl, ret_epilog = handle_retval name ret_type in
	      let args_decls = handle_args_decls name None args_exps in
	      let args_prolog = handle_args_prolog [] (List.length args_exps) 0 args_exps in
		
	      let args_t = List.rev (List.map extract_type args_decls) in
	      let fptr_exp = K.Lval (translate_lval fptr, K.FunPtr) in

	      let call_w_prolog = (K.Call (K.FunDeref (fptr_exp, (args_t, ret_type))), loc)::args_prolog in
	      let call_wo_ret = generate_body (List.rev call_w_prolog) args_decls in
		generate_body (call_wo_ret@(ret_epilog)) ret_decl
		  
	| _ -> error "Cil2newspeak.translate.translate_call: Left value not supported"
	    
      in
	restore_loc_cnt ();
	res



  let translate_fun name spec =
    let loc = match spec.fun_loc with
	None -> K.locUnknown
      | Some l -> l
    in
      cur_loc := loc;
      match (spec.formals, spec.body) with
	  _, [] -> None
	| None, _ -> error "Cil2newspeak.translate.translate_fun"
	| Some args, stmts -> begin
	    let init_frame () =
	      let status = 
		match spec.ret_type with
		    None -> empty_status ()
		  | Some t ->
		      loc_declare false (new_decl (t, "", (K.Init [])) 0 K.locUnknown true);
		      new_ret_status ()
	      in
		List.iter (loc_declare false) args;
		List.iter (loc_declare true) spec.locals;
		status
	    in
	      
	    let status = init_frame () in
	    let blk = K.simplify((translate_stmts status stmts)@[K.Label status.return_lbl, !cur_loc]) in
	    let body = generate_body blk (get_loc_decls ()) in
	      
	      Some body
	  end

(* TODO: add these as #pragma assume in the code, a lot easier *)
let create_assumption str =
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
    with _ -> error ("Unknown assumption: "^str)


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

      let assumptions = List.map create_assumption !Npkcontext.assumptions in

	(assumptions, glb_decls, fun_defs)

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


let prepare fnames =
  (* First we gather global declarations and functions *)
  declare fnames;

  update_loc locUnknown;
  (* Then we do our simplifications and collections on the entire file *)
  print_debug "Beginning first pass...";
  first_pass ();
  update_loc locUnknown;
  print_debug "First pass done."
    
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


let cil2newspeak fnames = 
  initCIL ();
  useLogicalOperators := false;
  (* TODO: remove List.rev here *)
  let fnames = List.map gen_ir (List.rev fnames) in

    prepare fnames;

  let kernel = translate () in
    print_debug "Translation complete.";
    if !verb_newspeak then begin
      print_endline "Newspeak output";
      print_endline "---------------";
      K.dump kernel;
      print_newline ()
    end;
    
    if (!newspeak_output <> "") then begin
      let ch_out = open_out_bin !newspeak_output in
	print_debug ("Writing "^(!newspeak_output)^"...");
	Marshal.to_channel ch_out (fnames, kernel) [];
	print_debug ("Writing done.");
    end;
      
    kernel


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

      if (!newspeak_output <> "") then begin
	let ch_out = open_out_bin !newspeak_output in
	  print_debug ("Writing "^(!newspeak_output)^"...");
	  Marshal.to_channel ch_out kernel [];
	  print_debug ("Writing done.");
      end;
      
      if !verb_newspeak then begin
	print_endline "Newspeak output";
	print_endline "---------------";
	K.dump kernel;
	print_newline ()
      end;
      kernel
*)
*)
 

(*=========================================*)
(* compile function, which wraps translate *)
(*=========================================*)

let compile name =
  initCIL ();
  useLogicalOperators := false;

  print_debug ("Parsing "^name^"...");
  let cil_file = Frontc.parse name () in
    print_debug ("Parsing done.");
    if !verb_cil then begin
      print_newline ();
      print_endline ("Cil output for "^name);
      for i = 1 to String.length name do
	print_string "-"
      done;
      print_endline "---------------";
      dump stdout cil_file;
      print_newline ();
      print_newline ()
    end;

    ignore (Local_pass.translate cil_file);
    
(*    remove_local_tmp cil_file;


    let (glbs, funs) = Local_pass.translate cilfile.globals in

    let name = (Filename.chop_extension name)^".ir" in
    let cout = open_out_bin name in
      print_debug ("Exporting "^name^"...");
      Marshal.to_channel cout glbs [];
      Marshal.to_channel cout funs [];
      close_out cout;
      print_debug ("Exporting done.");
      

  (* First we gather global declarations and functions *)
  declare fnames;

  update_loc locUnknown;
  (* Then we do our simplifications and collections on the entire file *)
  print_debug "Beginning first pass...";
  first_pass ();
  update_loc locUnknown;
  print_debug "First pass done."

  let kernel = translate () in
    print_debug "Translation complete.";
    if !verb_newspeak then begin
      print_endline "Newspeak output";
      print_endline "---------------";
      K.dump kernel;
      print_newline ()
    end;
    
    if (!newspeak_output <> "") then begin
      let ch_out = open_out_bin !newspeak_output in
	print_debug ("Writing "^(!newspeak_output)^"...");
	Marshal.to_channel ch_out (fnames, kernel) [];
	print_debug ("Writing done.");
    end;
      
    kernel

*)

failwith "Toto"
