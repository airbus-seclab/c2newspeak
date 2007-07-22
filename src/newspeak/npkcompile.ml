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

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org

  Olivier Levillain
  email: olivier.levillain@penjili.org
*)


open Params

open Cil
open Cilutils

open Npkcontext
open Npkutils

module F = Firstpass
module K = Npkil


let build_stmt stmtkind = (stmtkind, Npkcontext.get_loc ())

(*===================================*)
(* Misc. functions used by translate *)
(*===================================*)

let rec append_labels loc lbls blk =
  match lbls with
      [] -> blk
    | l::tl -> append_labels loc tl [K.DoWith (blk, l, []), loc]

(* TODO: remove *)
let hoist_labels blk = blk
(*  let rec remove_labels blk =
    match blk with
	[] -> ([], [])
      | (K.Label l, loc)::tl ->
	  let (lbls, tl) = remove_labels tl in
	    ((l, loc)::lbls, tl)
      | hd::tl -> 
	  let (lbls, tl) = remove_labels tl in
	    (lbls, hd::tl)
  in
  let (lbls, blk) = remove_labels blk in
    append_labels lbls blk
*)

let is_cil_label x =
  match x with
      Label (_, _, false) -> true
    | _ -> false

(* Generates a Newspeak statement by wrapping a block body with
   declarations decls. The order of the declaration must be carefully
   checked because in Newspeak, the variables are identified by their
   positions in the declaration stacks, not by their names *)
let rec append_decls decls body =
  match decls with
      [] -> body
    | (name, t, loc)::r -> append_decls r [K.Decl (name, t, body), loc]


(*================================*)
(* The central translate function *)
(*================================*)

let rec translate_const c =
  match c with
      CInt64 (i, k, _) -> 
	let (s, n) = translate_ikind k in
	  if n > 8 
	    || (n = 8 && s = Newspeak.Unsigned 
		&& Int64.compare i Int64.zero < 0) 
	  then error "Npkcompile.translate_const"
	    "integer too large: not representable";
	  K.Const (Newspeak.CInt64 i)
	  
    | CStr s -> Env.get_cstr s
    | CChr c -> K.Const (Newspeak.CInt64 (Int64.of_int (Char.code c))) 
	
    | CReal (f, _, Some s) -> K.Const (Newspeak.CFloat (f, s))
    | CReal (f, _, None) ->
	let s = string_of_float f in
	  if !verb_debug then begin
	    print_warning "Npkcompile.translate_const"
	      ("No string representation available for const "^s)
	  end;
	  K.Const (Newspeak.CFloat (f, s))
	    
    | CWStr _ | CEnum _
	-> error "Npkcompile.translate_const"
	("const '"^(string_of_exp (Const c))^"' not handled")

and translate_cast t e = 
  match t, e with
    | TNamed (info, _), e -> translate_cast info.ttype e
	
    | TPtr _, Const (CInt64 (c, _, _)) when c = Int64.zero -> 
	K.Const Newspeak.Nil
	
    | t, e -> begin
	let t_e = typeOf e in
	let cast = (t_e, t) in
	  match translate_typ t, translate_typ t_e with
	      (K.Scalar s, K.Scalar s_e) -> translate_scalar_cast cast e s s_e
		  
	    | _ ->
		error "Npkcompile.translate_cast"
		  ("translate cast: Invalid cast "^(string_of_cast cast e))
      end
	
and translate_scalar_cast cast e t1 t2 = 
  let e' = translate_exp e in
  match (t1, t2) with
      (Newspeak.Int k1, Newspeak.Int k2) when k1 = k2 -> e'
	
    | (Newspeak.Int int_t, Newspeak.Int _) -> K.make_int_coerce int_t e'
	
    | (Newspeak.Float _ as t', (Newspeak.Float _ as t))
    | (Newspeak.Float _ as t', (Newspeak.Int _ as t)) ->
	K.UnOp (K.Cast (t, t'), e')
	  
    | (Newspeak.Int (sign, sz') as kt', (Newspeak.Float sz as kt)) ->
	if sign = Newspeak.Unsigned then begin 
	  print_warning "Npkcompile.translate_scalar_cast"
	    ("cast from float to unsigned integer: "
	      ^"sign may be lost: "^(string_of_cast cast e))
	end;
	(K.UnOp (K.Cast (kt, kt'), e'))
	  
    | (Newspeak.Ptr, Newspeak.Ptr) -> e'

    | (Newspeak.FunPtr, Newspeak.FunPtr) ->
	print_warning "Npkcompile.translate_scalar_cast"
	  ("probable dangerous cast: "^(string_of_cast cast e));
	e'
		      
    | (Newspeak.Int (sign, sz), Newspeak.Ptr)
	when sz = pointer_size && !castor_allowed ->
	print_warning "Npkcompile.translate_scalar_cast"
	  ("probable invalid cast "^(string_of_cast cast e));
	  K.UnOp (K.PtrToInt (sign, sz), e')
	    
    | (Newspeak.Ptr, Newspeak.Int (sign, sz))
	when sz = pointer_size && !castor_allowed ->
	print_warning "Npkcompile.translate_scalar_cast"
	  ("probable invalid cast "^(string_of_cast cast e));
	  K.UnOp (K.IntToPtr (sign, sz), e')
	    
    | (Newspeak.Ptr as kt'), (Newspeak.FunPtr as kt) when !castor_allowed ->
	print_warning "Npkcompile.translate_scalar_cast"
	  ("probable invalid cast "^(string_of_cast cast e));
	K.UnOp (K.Cast (kt, kt'), e')
    
    | _ -> 
	error "Npkcompile.translate_scalar_cast"
	  ("translate cast: Invalid cast "^(string_of_cast cast e))

and translate_access lv e =
  let t = typeOfLval lv in
    match translate_typ t with
	K.Array (elt_t, len) ->
	  let elt_sz = size_of_subtyp t in
	  let (len, sz) =
	    match (len, lv) with
	      | (Some len, _) -> (K.Known len, K.Known (len * elt_sz))
	      | (None, (Var v, NoOffset)) -> 
		  (K.Length v.vname, K.SizeOf v.vname)
	      | _ -> 
		  error "Npkcompile.translate_access"
		    ("type of lval "^(string_of_lval lv)
		      ^" is not defined enough")
	  in
	  let checked_index = 
	    K.UnOp (K.Belongs_tmp (Int64.zero, len), translate_exp e) 
	  in
	  let offs = 
	    K.BinOp (Newspeak.MultI, checked_index, K.exp_of_int elt_sz) 
	  in
	    (offs, sz)
	      
      | _ -> error "Npkcompile.translate_access" "array expected"
      
     
and translate_lval lv =
  match lv with
    | Var v, NoOffset -> Env.get_var v

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
		    
	    | Index (e, NoOffset) ->
		let (offs, _) = translate_access lv' e in
		  K.Shift (translate_lval lv', offs)
		    
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
	  | K.Scalar (Newspeak.Int int_t) ->
	      K.make_int_coerce int_t (K.BinOp (Newspeak.MinusI, K.exp_of_int 0, translate_exp e))
	  | K.Scalar (Newspeak.Float sz) ->
	      (* TODO: check this transformation is really correct 
		 i.e. source and destination expression have the same 
		 semantics *)
	      K.BinOp (Newspeak.MinusF sz, K.zero_f, translate_exp e)
	  | _ -> error "Npkcompile.translate_exp" "integer or float type expected"
      end
	
    | UnOp (BNot, e, t) -> begin
	match translate_typ t with
	    K.Scalar (Newspeak.Int int_t) -> 
	      let b = Newspeak.domain_of_typ int_t in
		K.UnOp (K.BNot b, translate_exp e)
	  | _ -> error "Npkcompile.translate_exp" "integer type expected"
      end
	
    | UnOp (LNot, e, t) -> begin
	match translate_typ t with
	    K.Scalar (Newspeak.Int int_t) -> K.UnOp (K.Not, translate_exp e)
	  | _ -> error "Npkcompile.translate_exp" "integer type expected"
      end
	
    (* Binary operators *)
    (*------------------*)
	
    (* Arithmetic and floating point operations *)
    | BinOp (PlusA as o, e1, e2, t)   | BinOp (MinusA as o, e1, e2, t)
    | BinOp (Mult as o, e1, e2, t)    | BinOp (Div as o, e1, e2, t) 
    | BinOp (Mod as o, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar (Newspeak.Int int_t) ->
	      K.make_int_coerce int_t (K.BinOp (translate_arith_binop o,
						translate_exp e1, translate_exp e2))
		
	  | K.Scalar (Newspeak.Float sz) ->
	      K.BinOp (translate_float_binop sz o, 
		       translate_exp e1, translate_exp e2)
	  | _ -> error "Npkcompile.translate_exp" "integer or float type expected"
      end
	
    (* Bitwise operations *)
    | BinOp (BAnd as o, e1, e2, t)    | BinOp (BOr as o, e1, e2, t)
    | BinOp (BXor as o, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar (Newspeak.Int int_t) ->
	      K.BinOp (translate_logical_binop int_t o,
		       translate_exp e1, translate_exp e2)
	  | _ -> error "Npkcompile.translate_exp" "integer type expected"
      end
	
    (* Logical operations *)
    | BinOp (Shiftlt as o, e1, e2, t) | BinOp (Shiftrt as o, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar (Newspeak.Int int_t) ->
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
    | BinOp (Ge, e1, e2, (TInt _ as t)) -> 
	K.UnOp (K.Not, translate_exp (BinOp (Gt, e2, e1, t)))
    | BinOp (Ne, e1, e2, (TInt _ as t)) -> 
	K.UnOp (K.Not, translate_exp (BinOp (Eq, e2, e1, t)))
	
    (* Pointer / Integer addition *)
    | BinOp (IndexPI, e1, e2, t) | BinOp (PlusPI, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar Newspeak.Ptr -> 
	      let sz = size_of_subtyp t in
		K.BinOp (Newspeak.PlusPI, translate_exp e1,
			 K.BinOp (Newspeak.MultI, translate_exp e2, K.exp_of_int sz))
	  | K.Scalar Newspeak.FunPtr ->
	      error "Npkcompile.translate_exp"
		"pointer arithmetics forbidden on function pointers"
	  | _ -> error "Npkcompile.translate_exp" "data pointer type expected"
      end
	
    (* Pointer / Integer subtraction *) 
    | BinOp (MinusPI, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar Newspeak.Ptr -> 
	      let sz = size_of_subtyp t in
	      let v1 = translate_exp e1 in
	      let v2 = K.BinOp (Newspeak.MultI, translate_exp e2, K.exp_of_int sz) in
		K.BinOp (Newspeak.PlusPI, v1,
			 K.BinOp (Newspeak.MinusI, K.exp_of_int 0, v2))
	  | K.Scalar Newspeak.FunPtr ->
	      error "Npkcompile.translate_exp"
		"pointer arithmetics forbidden on function pointers"
	  | _ ->
	      error "Npkcompile.translate_exp" "data pointer type expected"
      end
	
    (* Pointer difference *) 
    | BinOp (MinusPP, e1, e2, t) -> begin
	match translate_typ (typeOf e1), translate_typ (typeOf e2), translate_typ t with
	  | K.Scalar Newspeak.Ptr, K.Scalar Newspeak.Ptr, 
	    K.Scalar Newspeak.Int int_t -> 
	      let v1 = translate_exp e1 in
	      let v2 = translate_exp e2 in
		K.make_int_coerce int_t (K.BinOp (Newspeak.MinusPP, v1, v2))
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
	  | (Var f, NoOffset), K.Scalar Newspeak.FunPtr ->
	      K.AddrOfFun f.vname
	      
	  | _, K.Scalar Newspeak.Ptr ->
	      let (lv', offs) = removeOffsetLval lv in begin
		  match offs with 
		    | Index (e, NoOffset) -> 
			let (offs, sz) = translate_access lv' e in
			let lv' = translate_lval lv' in
			  K.BinOp (Newspeak.PlusPI, K.AddrOf (lv', sz), offs)

		    | _ -> 
			let sz = size_of (typeOfLval lv) in
			  K.AddrOf (translate_lval lv, K.Known sz)
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
  Npkcontext.set_loc (get_stmtLoc kind);
  let loc = Npkcontext.get_loc () in
    match kind with
      | Instr il -> translate_instrlist il
	  
      | Return (None, _) -> [K.Goto (Env.get_ret_lbl ()), loc]
	    
      | Return (Some e, _) ->
	  let typ = translate_typ (typeOf e) in
	  let lval = Env.get_ret_var () in
	  let lbl = Env.get_ret_lbl () in
	    [translate_set lval typ e, loc; K.Goto lbl, loc]

      | If (e, blk1, blk2, _) -> translate_if status e blk1.bstmts blk2.bstmts
	    
      | Block b -> translate_stmts status b.bstmts
	  
      | Loop (body, _, _, _)  ->
	  let status = Env.new_brk_status status in
	  let loop = (K.InfLoop (translate_stmts status body.bstmts), loc) in
	  let lbl = Env.get_brk_lbl () in
	    [K.DoWith ([loop], lbl, []), loc]
	      
      | Break _ -> [K.Goto (Env.get_brk_lbl ()), loc]
	  
      | Switch (e, body, stmt_list, _) ->
	  translate_switch status e stmt_list body
	    
      | Goto (x, _) ->
	  let rec explore_labels l =
	    match l with
		[] -> error "Npkcompile.translate_stmtkind" "unexpected goto"
	      | (Label (s, loc, false) as l)::r -> begin
		  try
		    translate_stmts status 
		      (Hashtbl.find F.code_to_duplicate l)
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
    | stmt::r when List.for_all is_cil_label stmt.labels ->
	let first = translate_stmtkind status stmt.skind in
	  first@(translate_stmts status r)
    | stmt::r ->
	match stmt.labels with
	    (Case _ | Default _)::_ ->
	      error "Npkcompile.translate_stmts" 
		"unstructed use of switch statement"
	  | _ -> error "Npkcompile.translate_stmts" "unexpected label"

and translate_instrlist il =
  match il with
      [] -> []
    | i::r ->
	let i = translate_instr i in
	  i@(translate_instrlist r)
	    
	    
and translate_instr i =
  Npkcontext.set_loc (get_instrLoc i);
  match i with
      Set (lv, e, _) ->
	  let lval = translate_lval lv in
	  let typ = translate_typ (typeOfLval lv) in
	  let assignment = translate_set lval typ e in
	    (build_stmt assignment)::[]

    | Call (x, Lval lv, args, _) -> translate_call x lv args

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
  let loc = Npkcontext.get_loc () in
  let (e, t) = 
    match translate_typ (typeOf e) with
	K.Scalar (Newspeak.Int _ as t) -> (e, t)
      | K.Scalar (Newspeak.Ptr|Newspeak.FunPtr as t) -> 
	  (BinOp (Ne, e, null, intType), t)
      | _ ->
	  error "Npkcompile.translate_if"
	    ("bad expression '"^(string_of_exp e)^"'")
  in
  let rec normalize e =
    match e with
	(* TODO: beware is the type t here really the same ? *)
      | K.UnOp (K.Not, K.UnOp (K.Not, e)) -> normalize e

      | K.UnOp (K.Not, K.BinOp ((Newspeak.Gt _|Newspeak.Eq _), _, _))
      | K.BinOp ((Newspeak.Gt _|Newspeak.Eq _), _, _) -> e

      | K.UnOp (K.Not, e) -> K.BinOp (Newspeak.Eq t, e, K.zero)
      | _ -> K.UnOp (K.Not, K.BinOp (Newspeak.Eq t, K.zero, e))
  in
    let cond1 = normalize (translate_exp e) in
    let cond2 = K.negate cond1 in
    let body1 = translate_stmts status stmts1 in
    let body2 = translate_stmts status stmts2 in
      [(K.ChooseAssert [([cond1], body1); ([cond2], body2)], loc)]


(** Returns a set of blocks, each representing a choice of the case 
    statement. Each case is translated by appending a guard.
    It also builds the guard of the default statement. *)
and translate_switch status e stmt_list body =
  let (status, choices) = translate_switch_cases status e stmt_list in
  let body = translate_switch_body status choices body.bstmts in
    (* TODO: add the DoWith of the break statement ?? *)
    body
    
and translate_switch_cases status e labels =
  let status = ref status in
  let cases = ref [] in
  let default_cond = ref [] in
  let default_goto = ref [build_stmt (K.Goto (Env.get_brk_lbl ()))] in

  let switch_exp = translate_exp e in

  let translate_case x =
    let lbl = Env.new_lbl () in

    let rec translate_aux labels =
      match labels with
	  (* When two cases have the same body, then CIL creates only one 
	     instruction with several labels.
	     However this instruction is present several time in the list 
	     of labels.
	  *)
	  (Case (_, loc) | Default loc)::tl 
	    when Env.mem_switch_lbl !status loc -> translate_aux tl
      | (Case (v, loc))::tl ->
(*	TODO: remove ??  Npkcontext.set_loc loc;*)
	  let t = 
	    match translate_typ (typeOf v) with
		K.Scalar i -> i
	      | _ -> error "Npkcompile.tranlate_switch" "expression not scalar"
	  in
	  let cond = K.BinOp (Newspeak.Eq t, switch_exp, translate_exp v) in
	    status := Env.add_switch_lbl !status loc lbl;
	    cases := (cond::[], build_stmt (K.Goto lbl)::[])::!cases;
	    default_cond := (K.negate cond)::!default_cond;
	    translate_aux tl

      | (Default loc)::tl -> 
(* TODO: remove ?? *)
(*	  Npkcontext.set_loc loc;*)
	  (* TODO: have a get_default_lbl instead, with a default number 
	     for it *)
	    status := Env.add_switch_lbl !status loc lbl;
	    default_goto := [build_stmt (K.Goto lbl)];
	    translate_aux tl

      | [] -> ()

      | _ -> error "Npkcompile.translate_switch_cases" "invalid label"
    in
      translate_aux x.labels
  in
    List.iter translate_case (List.rev labels);
    let default_choice = (!default_cond, !default_goto) in
      (!status, [build_stmt (K.ChooseAssert (default_choice::!cases))])

(* TODO: should err on labels that one doesn't know *)
and translate_switch_body status body stmts =
  match stmts with
      [] -> [build_stmt (K.DoWith (List.rev body, Env.get_brk_lbl (), []))]

    | hd::tl when hd.labels = [] ->
	let hd = translate_stmtkind status hd.skind in
	  translate_switch_body status (hd@body) tl

    | hd::tl -> 
	let lbl = 
	  match hd.labels with
	      (Case (_, loc))::_ | (Default loc)::_ ->
		Env.get_switch_lbl status loc		
	    | _ -> error "Npkcompile.translate_switch_body" "invalid label"
	in

	let body = [build_stmt (K.DoWith (List.rev body, lbl, []))] in
	  
	let hd = translate_stmtkind status hd.skind in
	  translate_switch_body status (hd@body) tl

	  
and translate_call x lv args_exps =
  let loc = Npkcontext.get_loc () in

  let handle_retval fname ret_type =
    match ret_type, x  with
	(* No return value *)
	None, None -> [], []

      (* Return value ignored *)
      | Some t, None ->
	  Env.push_local ();
	  [("value_of_"^fname, t, loc)], []
	    
      (* Return value put into Lval cil_lv *)
      | Some t_given, Some cil_lv ->
	  let t_expected = translate_typ (typeOfLval cil_lv) in
	    Env.push_local ();
	    let lval = translate_lval cil_lv in
	    let ret_decl = ["value_of_"^fname, t_given, loc] in
	    let ret_epilog = match t_given, t_expected with
	      | K.Scalar s_giv, K.Scalar s_exp when s_giv = s_exp ->
		  let set = K.Set (lval, K.Lval (K.Local 0, s_giv), s_exp) in
		  [build_stmt set]
		    
	      | K.Scalar (Newspeak.Int _ as s_giv), 
		  K.Scalar (Newspeak.Int int_t as s_exp) ->
		  let exp = K.make_int_coerce int_t (K.Lval (K.Local 0, s_giv)) in
		    [build_stmt (K.Set (lval, exp, s_exp))]
		      
	      | K.Region (desc1, sz1), K.Region (desc2, sz2)
		  when sz1 = sz2 && desc1 = desc2 ->
		  [build_stmt (K.Copy (lval, K.Local 0, sz1))]
		    
	      | _ -> error "Npkcompile.translate_call" "invalid implicit cast"
	    in
	      (ret_decl, ret_epilog)
		
      | _ ->
	  error "Npkcompile.translate_call"
	    ("function "^fname^" has return type void")
  in

  let handle_args_decls fname exps =
    let res = ref [] in
    let build_unknown_args exps =
      let rec build_args exps i = 
	match exps with
	    [] -> []
	  | e::tl ->
	      let t = translate_typ (typeOf e) in
	      let tl = build_args tl (i+1) in
		(-1, "arg"^(string_of_int i), t)::tl
      in
	build_args exps 0
    in
    let rec handle_args exps args =
      match (exps, args) with
	  ([], []) -> ()
	| (e::tl, (_, str, _)::args) ->
	    Env.push_local ();
	    let t = translate_typ (typeOf e) in
	      res := (str, t, loc)::!res;
	      handle_args tl args
	| _ -> 
	    error "Npkcompile.translate_call.handle_args_decls.handle_args" 
	      "This code should be unreachable"
    in
    let args = 
      match Env.get_args fname with
	  None -> build_unknown_args exps 
	| Some args -> args
    in
      handle_args exps args;
      !res
  in

  let rec handle_args_prolog accu n i args_exps =
    match args_exps with
	[] -> accu
      | e::r_e ->
	  let t = translate_typ (typeOf e) in
	  let lval = K.Local ((n-i) - 1) in
	  let set = translate_set lval t e in
	    handle_args_prolog ((build_stmt set)::accu) n (i+1) r_e
  in

    Env.save_loc_cnt ();
    let (name, ret_type) = 
      match lv with
	| Var f, NoOffset ->
	    let name = f.vname in
	    let ret = 
	      match f.vtype with
		| TFun (ret, _, _, _) -> 
		    let ret = Npkutils.translate_ret_typ ret in
		    let arg_from_exp e = ("", typeOf e, []) in 
		    let args = Some (List.map arg_from_exp args_exps) in 
		    let args = Env.translate_formals name args in begin
		      try Env.update_fun_proto name ret args 
		      with Invalid_argument _ ->
			(* TODO: See if we can be as specific as before (int4 <> ptr) *)
			error "Npkcompile.translate_call" 
			  ("function "^name^" called with args not matching prototype")
		    end;
		    ret
			
		| _ ->
		    error "Npkcompile.translate_call"
		      ("invalid type '"^(string_of_type f.vtype)^"'")
	    in
	      (name, ret)

	| Mem (Lval fptr), NoOffset ->
	    let typ = translate_typ (typeOfLval fptr) in
	      if typ <> K.Scalar Newspeak.FunPtr
	      then error "Npkcompile.translate_call" "FunPtr expected";
	      
	      let ret = 
		match x with
		  | None -> None
		  | Some cil_lv -> Some (translate_typ (typeOfLval cil_lv))
	      in
	      let (_, line, _) = loc in
		("fptr_called_line_" ^ (string_of_int line), ret)
		  
	| _ -> error "Npkcompile.translate_call" "Left value not supported"
    in
      
    let ret_decl, ret_epilog = handle_retval name ret_type in
    let args_decls = handle_args_decls name args_exps in
    let args_prolog = handle_args_prolog [] (List.length args_exps) 0 args_exps in

    let fexp = 
      match lv with
	| (Var f, NoOffset) -> K.FunId f.vname
	| (Mem (Lval fptr), NoOffset) ->
	    let args_t = List.rev (List.map (fun (_, t, _) -> t) args_decls) in
	    let fptr_exp = K.Lval (translate_lval fptr, Newspeak.FunPtr) in
	      K.FunDeref (fptr_exp, (args_t, ret_type))
	| _ -> error "Npkcompile.translate_call" "Left value not supported"
    in
    let call_w_prolog = (K.Call fexp, loc)::args_prolog in
    let call_wo_ret = append_decls args_decls (List.rev call_w_prolog) in
    let res = append_decls ret_decl (call_wo_ret@(ret_epilog)) in
      Env.restore_loc_cnt ();
      res


let translate_fun name (locals, formals, body) =
 assert (Hashtbl.mem Env.fun_specs name);
  let spec = Hashtbl.find Env.fun_specs name in
  let floc = spec.K.ploc in
    (*Npkcontext.set_loc floc;*)
    (* TODO: cleanup, should call a Env update function *)
    spec.K.plocs <- Some locals;
    spec.K.pargs <- Some formals;

    Env.reset_lbl_gen ();
    let status = 
      match spec.K.prett with
	| None -> Env.empty_status ()
	| Some _ ->
	    Env.push_local ();
	    Env.new_ret_status ()
    in
      List.iter (Env.loc_declare false) formals;
      List.iter (Env.loc_declare true) locals;
      
      let body = translate_stmts status body.bstmts in
      let lbl = Env.get_ret_lbl () in
      let blk = [K.DoWith (body, lbl, []), floc] in
      let body = append_decls (Env.get_loc_decls ()) blk in
	
	(* TODO ?: Check only one body exists *)
	spec.K.pbody <- Some body
	
let translate_init x t =
  let glb_inits = ref [] in
    
  let rec expand off i =
    match i with
	SingleInit e ->
	  let o = Cilutils.offset_of t off in
	  let v = 
	    match translate_typ (typeOf e) with
		K.Scalar s -> (o, s, translate_exp e)
		  (* TODO: check all error messages! *)
	      | _ -> 
		  error "Npklink.translate_init" 
		    "unexpected type of SingleInit"
	  in
	    glb_inits := v::(!glb_inits)
      | CompoundInit (_, c) -> List.iter (expand_elem off) c

  and expand_elem prefix (off, i) = 
    expand (addOffset off prefix) i
  in

    match x with
	None -> None
      | Some i ->
	  expand NoOffset i;
	  Some (List.rev !glb_inits)

(* TODO: maybe should put first pass into npkcompile ?? *)
let translate_glb used_glb name x =
  let used = K.String_set.mem name used_glb in
  let defd = x.F.gdefd in
    Npkcontext.set_loc x.F.gloc;
    if (defd || used) then begin
      let init =
	if defd then begin
	  Some (translate_init x.F.ginit x.F.gtype)
	end else begin
	  assert (x.F.ginit = None);
	  None
	end
      in
      let t = translate_typ x.F.gtype in
      let glb = 
	{ 
	  K.gtype = t; 
	  K.gloc = x.F.gloc;
	  K.ginit = init;
	  K.gused = used
	} 
      in
	Hashtbl.add Env.glb_decls name glb
    end


(*=========================================*)
(* compile function, which wraps translate *)
(*=========================================*)

let compile in_name out_name  =
  if not (Filename.check_suffix in_name c_suffix)
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
    Npkcontext.forget_loc ();
    let (glb_used, glb_cstr, fun_specs, glb_decls) = 
      Firstpass.first_pass cil_file 
    in
      Env.glb_cstr := glb_cstr;

      Npkcontext.forget_loc ();
      print_debug "First pass done.";
      
      print_debug ("Translating "^in_name^"...");

      Hashtbl.iter translate_fun fun_specs;
      Hashtbl.iter (translate_glb glb_used) glb_decls;
      
      let (globs, funs) = Env.create_npkil in_name in
	
	Env.init_env ();
	
	if (!verb_npko) then begin
	  print_endline "Newspeak Object output";
	  print_endline "----------------------";
	  K.dump_npko (globs, funs);
	  print_newline ();
	end;
	
	Npkcontext.forget_loc ();
	if (out_name <> "") then begin
	  print_debug ("Writing "^(out_name)^"...");
	  let ch_out = open_out_bin out_name in
	    Marshal.to_channel ch_out "NPKO" [];
	    Marshal.to_channel ch_out (globs, funs) [];
	    close_out ch_out;
	    print_debug ("Writing done.");
	end;
	
	Env.init_env()

