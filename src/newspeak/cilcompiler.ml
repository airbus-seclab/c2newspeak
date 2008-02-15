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

open Npkcontext
open Npkutils

module F = Cilfirstpass
module K = Npkil

let set_loc loc = Npkcontext.set_loc (Npkutils.translate_loc loc)

let build_stmt stmtkind = (stmtkind, Npkcontext.get_loc ())

(*===================================*)
(* Misc. functions used by translate *)
(*===================================*)

let is_cil_label x =
  match x with
      Label (_, _, false) -> true
    | _ -> false

(*================================*)
(* The central translate function *)
(*================================*)

let rec translate_const c =
  match c with
      CInt64 (i, k, _) -> 
	let (s, n) = translate_ikind k in
	  if n > 64
	    || (n = 64 && s = Newspeak.Unsigned 
		&& Int64.compare i Int64.zero < 0) 
	  then error "Npkcompile.translate_const"
	    "integer too large: not representable";
	  K.Const (Newspeak.CInt64 i)
	  
    | CStr s -> Cilenv.get_cstr s

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
	("const '"^(Cilutils.string_of_exp (Const c))^"' not handled")

and translate_cast t e = 
  match t, e with
    | TNamed (info, _), e -> translate_cast info.ttype e
	
    | TPtr _, Const (CInt64 (c, _, _)) when c = Int64.zero -> 
	K.Const Newspeak.Nil
	
    | t, e -> begin
	let t_e = typeOf e in
	  match (translate_typ t, translate_typ t_e) with
	      (K.Scalar s, K.Scalar s_e) -> 
		translate_scalar_cast (translate_exp e, s_e) s

	    | _ ->
		error "Npkcompile.translate_cast"
		  ("translate cast: Invalid cast "
		    ^(Cilutils.string_of_cast (t_e, t) e))
      end
	
and translate_scalar_cast (e, t2) t1 = 
  match (t1, t2) with
      (Newspeak.Int k1, Newspeak.Int k2) when k1 = k2 -> e

    | (Newspeak.Int int_t, Newspeak.Int _) -> K.make_int_coerce int_t e

    | (Newspeak.Float _ as t', (Newspeak.Float _ as t))
    | (Newspeak.Float _ as t', (Newspeak.Int _ as t)) ->
	K.UnOp (K.Cast (t, t'), e)
	  
    | (Newspeak.Int (sign, sz') as kt', (Newspeak.Float sz as kt)) ->
	if sign = Newspeak.Unsigned then begin 
	  print_warning "Npkcompile.translate_scalar_cast"
	    ("cast from float to unsigned integer: "
	      ^"sign may be lost: "^(K.string_of_cast t2 t1))
	end;
	(K.UnOp (K.Cast (kt, kt'), e))
	  
    | (Newspeak.Ptr, Newspeak.Ptr) -> e

    | (Newspeak.FunPtr, Newspeak.FunPtr) -> e
		      
    | (Newspeak.Int (sign, sz), Newspeak.Ptr)
	when sz = Config.size_of_ptr && !castor_allowed ->
	print_warning "Npkcompile.translate_scalar_cast"
	  ("Probable invalid cast "^(K.string_of_cast t2 t1));
	  K.UnOp (K.PtrToInt (sign, sz), e)
	    
    | (Newspeak.Ptr, Newspeak.Int (sign, sz))
	when sz = Config.size_of_ptr && !castor_allowed ->
	print_warning "Npkcompile.translate_scalar_cast"
	  ("Probable invalid cast "^(K.string_of_cast t2 t1));
	  K.UnOp (K.IntToPtr (sign, sz), e)
	    
    | (Newspeak.Ptr as kt'), (Newspeak.FunPtr as kt) when !castor_allowed ->
	print_warning "Npkcompile.translate_scalar_cast"
	  ("Probable invalid cast "^(K.string_of_cast t2 t1));
	K.UnOp (K.Cast (kt, kt'), e)

    | _ -> 
	Npkcontext.error "Cilcompiler.translate_scalar_cast"
	  ("Invalid cast "^(K.string_of_cast t2 t1))
     
and translate_lval lv =
  match lv with
    | Var v, NoOffset -> Cilenv.get_var v

    | Mem e, NoOffset ->
	let sz = Cilutils.size_of (typeOfLval lv) in
	  K.Deref (translate_exp e, sz)

    | _ ->
	let (lv', offs) = removeOffsetLval lv in
	let t = typeOfLval lv' in
	  match offs with
	      Field (info, NoOffset) ->
		let o = Cilutils.offset_of t offs in
		  K.Shift (translate_lval lv', K.exp_of_int o)
		    
	    | Index (idx, NoOffset) ->
		let t = translate_typ t in
		let lv' = translate_lval lv' in
		let (elt_t, len) = K.array_of_typ t lv' in
		let elt_sz = K.size_of elt_t in
		let idx = translate_exp idx in
		let checked_index = 
		  K.UnOp (K.Belongs_tmp (Int64.zero, len), idx) 
		in
		let offs = 
		  K.BinOp (Newspeak.MultI, checked_index, K.exp_of_int elt_sz)
		in
		  K.Shift (lv', offs)
		    
	    | _ -> error "Npkcompile.translate_lval" "offset not handled"


(* TODO: See if there cannot be any factorisation here *)
and translate_exp e =
  match e with
      Const c -> translate_const c
    | SizeOf t -> K.exp_of_int ((Cilutils.size_of t) / 8)
    | SizeOfE e -> K.exp_of_int ((Cilutils.size_of (typeOf e)) / 8)
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
	  | _ -> error "Npkcompile.translate_exp.BNot" "integer type expected"
      end
	
    | UnOp (LNot, e, t) -> 
	let t' = translate_typ t in 
	let t = translate_typ (typeOf e) in begin
	    match (t, t') with
		(K.Scalar (Newspeak.Int k), K.Scalar (Newspeak.Int k')) -> 
		    let e = translate_exp e in
		    let e = 
		      translate_scalar_cast (e, Newspeak.Int k) 
			(Newspeak.Int k')
		    in
		      K.UnOp (K.Not, e)
	      | (K.Scalar Newspeak.Ptr, K.Scalar (Newspeak.Int k')) ->
		  let e = translate_exp e in
		    K.BinOp (Newspeak.Eq Newspeak.Ptr, e, K.Const Newspeak.Nil)
	      | _ -> 
		  error "Npkcompile.translate_exp.LNot" "integer type expected"
	  end
				      
    (* Binary operators *)
    (*------------------*)
	
    (* Arithmetic and floating point operations *)
    | BinOp (PlusA as o, e1, e2, t)   | BinOp (MinusA as o, e1, e2, t)
    | BinOp (Mult as o, e1, e2, t)    | BinOp (Div as o, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar (Newspeak.Int int_t) ->
	      K.make_int_coerce int_t (K.BinOp (translate_arith_binop o,
					       translate_exp e1, translate_exp e2))
		
	  | K.Scalar (Newspeak.Float sz) ->
	      K.BinOp (translate_float_binop sz o, 
		       translate_exp e1, translate_exp e2)
	  | _ -> error "Npkcompile.translate_exp" "integer or float type expected"
      end

    | BinOp (Mod as o, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar (Newspeak.Int int_t) ->
	      (K.BinOp (translate_arith_binop o,
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
	  | _ -> error "Npkcompile.translate_exp.Bop" "integer type expected"
      end
	
    (* Logical operations *)
    | BinOp (Shiftlt as o, e1, e2, t) | BinOp (Shiftrt as o, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar (Newspeak.Int int_t) ->
	      K.make_int_coerce int_t (K.BinOp (translate_logical_binop int_t o,
						translate_exp e1, translate_exp e2))
	  | _ -> error "Npkcompile.translate_exp.Shift" "integer type expected"
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
	K.UnOp (K.Not, translate_exp (BinOp (Eq, e1, e2, t)))
	
    (* Pointer / Integer addition *)
    | BinOp (IndexPI, e1, e2, t) | BinOp (PlusPI, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar Newspeak.Ptr -> 
	      let sz = Cilutils.size_of_subtyp t in
		K.BinOp (Newspeak.PlusPI, translate_exp e1,
			 K.BinOp (Newspeak.MultI, translate_exp e2, K.exp_of_int sz))
	  | K.Scalar Newspeak.FunPtr ->
	      error "Npkcompile.translate_exp"
		"pointer arithmetic forbidden on function pointers"
	  | _ -> error "Npkcompile.translate_exp" "data pointer type expected"
      end
	
    (* Pointer / Integer subtraction *) 
    | BinOp (MinusPI, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar Newspeak.Ptr -> 
	      let sz = Cilutils.size_of_subtyp t in
	      let v1 = translate_exp e1 in
	      let v2 = K.BinOp (Newspeak.MultI, translate_exp e2, K.exp_of_int sz) in
		K.BinOp (Newspeak.PlusPI, v1,
			 K.BinOp (Newspeak.MinusI, K.exp_of_int 0, v2))
	  | K.Scalar Newspeak.FunPtr ->
	      error "Npkcompile.translate_exp"
		"pointer arithmetic forbidden on function pointers"
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
	      ("scalar type expected for left value "
		^(Cilutils.string_of_lval lv))
      end
	
    | AddrOf lv -> begin
	match lv, translate_typ (typeOf e) with
	  | (Var f, NoOffset), K.Scalar Newspeak.FunPtr ->
	      K.AddrOfFun f.vname
	      
	  | _, K.Scalar Newspeak.Ptr ->
	      let (lv', offs) = removeOffsetLval lv in begin
		match offs with 
		  | Index (Const CInt64 (i, _, _), NoOffset) 
		      when Int64.compare i Int64.zero = 0 -> 
		      let t = typeOfLval lv' in
		      let t = translate_typ t in
		      let lv' = translate_lval lv' in
		      let sz = K.size_of_array t lv' in
			K.AddrOf (lv', sz)
			  
		  | Index (e, NoOffset) ->
		      let t = TPtr (typeOfLval lv, []) in
		      let offset = Index (Cil.zero, NoOffset) in
		      let base = AddrOf (addOffsetLval offset lv') in
			translate_exp (BinOp (PlusPI, base, e, t))
			
		  | _ -> 
		      let sz = Cilutils.size_of (typeOfLval lv) in
			K.AddrOf (translate_lval lv, K.Known sz)
		end
							 
	  | _ -> error "Npkcompile.translate_exp"
	      ("unexpected left value in AddrOf "
		^(Cilutils.string_of_lval lv))
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
	  ("expression '"^(Cilutils.string_of_exp e)^"' not handled")
	
	
	
and translate_stmtkind status kind =
  set_loc (get_stmtLoc kind);
  let loc = Npkcontext.get_loc () in
    match kind with
      | Instr il -> translate_instrlist il
	  
      | Return (None, _) -> [K.Goto (Cilenv.get_ret_lbl ()), loc]
	    
      | Return (Some e, _) ->
	  let typ = translate_typ (typeOf e) in
	  let lval = Cilenv.get_ret_var () in
	  let lbl = Cilenv.get_ret_lbl () in
	    [translate_set lval typ e, loc; K.Goto lbl, loc]

      | If (e, blk1, blk2, _) -> translate_if status e blk1.bstmts blk2.bstmts
	    
      | Block b -> translate_stmts status b.bstmts
	  
      | Loop (body, _, _, _)  ->
	  let loop = (K.InfLoop (translate_stmts status body.bstmts), loc) in
	  let lbl = Cilenv.get_brk_lbl () in
	    [K.DoWith ([loop], lbl, []), loc]
	      
      | Break _ -> [K.Goto (Cilenv.get_brk_lbl ()), loc]
	  
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
  set_loc (get_instrLoc i);
  match i with
      Set (lv, e, _) ->
	  let lval = translate_lval lv in
	  let typ = translate_typ (typeOfLval lv) in
	  let assignment = translate_set lval typ e in
	    (build_stmt assignment)::[]

    | Call (x, Lval lv, args, _) -> translate_call x lv args

    | Call (_, _, _, _) ->
	error "Npkcompile.translate_instr"
	  ("call '"^(Cilutils.string_of_instr i)^"' not handled")

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
		("left value expected instead of '"
		  ^(Cilutils.string_of_exp e)^"'")
      end
	
    | _ -> error "Npkcompile.translate_set" "invalid type"
	

and translate_if status e stmts1 stmts2 =
  let loc = Npkcontext.get_loc () in
    (* TODO: code cleanup: need to do this properly!! *)
    (* TODO: check the switch also *)
    (* TODO: make a sanity check algorithm *)
  let rec normalize e =
    match e with
      | UnOp (LNot, UnOp (LNot, e, _), _) -> normalize e
      | CastE (t, e) -> CastE (t, normalize e)
      | UnOp (LNot, BinOp ((Lt|Le|Gt|Ge|Eq|Ne|LAnd|LOr), _, _, _), _)
      | BinOp ((Lt|Le|Gt|Ge|Eq|Ne|LAnd|LOr), _, _, _) -> e
      | UnOp (LNot, e, t') -> begin
	  match translate_typ (typeOf e) with
	      K.Scalar (Newspeak.Ptr|Newspeak.FunPtr) -> 
		BinOp (Eq, e, Cilutils.null, Cil.intType)
	    | K.Scalar (Newspeak.Int (Newspeak.Unsigned, _)) ->
		BinOp (Eq, Cil.mkCast e Cil.uintType, 
		       Cil.mkCast Cil.zero Cil.uintType, Cil.uintType)
	    | _ -> BinOp (Eq, Cil.mkCast e Cil.intType, Cil.zero, Cil.intType)
	end
(* TODO: invert these !!!*)
      | _ -> begin
	  match translate_typ (typeOf e) with
	      K.Scalar (Newspeak.Ptr|Newspeak.FunPtr) -> 
		BinOp (Ne, e, Cilutils.null, Cil.intType)
	    | K.Scalar (Newspeak.Int (Newspeak.Unsigned, _)) ->
		BinOp (Ne, Cil.mkCast e Cil.uintType, 
		       Cil.mkCast Cil.zero Cil.uintType, Cil.uintType)
	    | _ -> BinOp (Ne, Cil.mkCast e Cil.intType, 
			  Cil.zero, Cil.intType)
	end
  in
  let e = normalize e in
  let cond1 = translate_exp e in
  let cond2 = K.negate cond1 in
  let body1 = translate_stmts status stmts1 in
  let body2 = translate_stmts status stmts2 in
      [(K.ChooseAssert [([cond1], body1); ([cond2], body2)], loc)]


(** Returns a set of blocks, each representing a choice of the case 
    statement. Each case is translated by appending a guard.
    It also builds the guard of the default statement. *)
and translate_switch status e stmt_list body =
  let status = ref status in
  let cases = ref [] in
  let default_cond = ref [] in
  let default_goto = ref [build_stmt (K.Goto (Cilenv.get_brk_lbl ()))] in

  let switch_exp = translate_exp e in

  let translate_case x =
    let lbl = Cilenv.new_lbl () in

    let rec translate_aux labels =
      match labels with
	  (* When two cases have the same body, then CIL creates only one 
	     instruction with several labels.
	     However this instruction is present several time in the list 
	     of labels.
	  *)
	  (Case (_, loc) | Default loc)::tl 
	    when Cilenv.mem_switch_lbl !status loc -> translate_aux tl
      | (Case (v, loc))::tl ->
	  let t = 
	    match translate_typ (typeOf v) with
		K.Scalar i -> i
	      | _ -> error "Npkcompile.tranlate_switch" "expression not scalar"
	  in
	  let cond = K.BinOp (Newspeak.Eq t, switch_exp, translate_exp v) in
	    status := Cilenv.add_switch_lbl !status loc lbl;
	    translate_aux tl;
	    cases := (cond::[], build_stmt (K.Goto lbl)::[])::!cases;
	    default_cond := (K.negate cond)::!default_cond

      | (Default loc)::tl -> 
	  (* TODO: have a get_default_lbl instead, with a default number 
	     for it, maybe, maybe not?? *)
	  status := Cilenv.add_switch_lbl !status loc lbl;
	  default_goto := [build_stmt (K.Goto lbl)];
	  translate_aux tl

      | [] -> ()

      | _ -> error "Npkcompile.translate_switch_cases" "invalid label"
    in
      translate_aux x.labels
  in
    
    Cilenv.reset_lbl_gen ();
    List.iter translate_case (List.rev stmt_list);
    let default_choice = (!default_cond, !default_goto) in
    let choices = [build_stmt (K.ChooseAssert (default_choice::!cases))] in

      translate_switch_body (Npkcontext.get_loc ()) !status choices body.bstmts

and translate_switch_body loc status body stmts =
  match stmts with
      [] -> [K.DoWith (List.rev body, Cilenv.get_brk_lbl (), []), loc]

    | hd::tl when hd.labels = [] ->
	let hd = translate_stmtkind status hd.skind in
	  translate_switch_body loc status (hd@body) tl

    | hd::tl -> 
	let lbl = 
	  match hd.labels with
	      (Case (_, loc))::_ | (Default loc)::_ ->
		set_loc loc;
		Cilenv.get_switch_lbl status loc		
	    | _ -> error "Npkcompile.translate_switch_body" "invalid label"
	in

	let body = [build_stmt (K.DoWith (List.rev body, lbl, []))] in
	  
	let hd = translate_stmtkind status hd.skind in
	  translate_switch_body loc status (hd@body) tl

	  
and translate_call x lv args_exps =
  let loc = Npkcontext.get_loc () in

  let handle_retval fname ret_type =
    match ret_type, x  with
	(* No return value *)
	None, None -> [], []

      (* Return value ignored *)
      | Some t, None ->
	  Cilenv.push_local ();
	  [("value_of_"^fname, t, loc)], []
	    
      (* Return value put into Lval cil_lv *)
      | Some t_given, Some cil_lv ->
	  let t_expected = translate_typ (typeOfLval cil_lv) in
	    Cilenv.push_local ();
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
    let build_unknown_args exps =
      let i = ref (-1) in
      let build_arg _ =
	i := !i + 1;
	"arg"^(string_of_int !i)
      in
	List.map build_arg exps
    in
    let rec handle_args e str =
      Cilenv.push_local ();
      let t = translate_typ (typeOf e) in
	(str, t, loc)
    in
    let args = 
      try Cilenv.get_args fname 
      with Not_found -> build_unknown_args exps 
    in
      try List.map2 handle_args exps args
      with Invalid_argument _ -> 
	error "Npkcompile.translate_call.handle_args_decls.handle_args" 
	  "This code should be unreachable"
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

    Cilenv.save_loc_cnt ();
    let (name, ret_type) = 
      match lv with
	| Var f, NoOffset ->
	    let name = f.vname in
	      (* TODO: code cleanup remove these ? *)
	    let arg_from_exp e = ("", typeOf e, []) in 
	    let args = Some (List.map arg_from_exp args_exps) in
	    let ret = match f.vtype with
		TFun (ret, _, _, _) -> ret
	      | _ -> 		    
		  error "Npkcompile.translate_call"
		    ("invalid type '"^(Cilutils.string_of_type f.vtype)^"'")
	    in
	    let (args, ret) = Npkutils.translate_ftyp (args, ret) in
	      Cilenv.update_fun_proto name (args, ret);
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
	    let args_t = List.map (fun (_, t, _) -> t) args_decls in
	    let fptr_exp = K.Lval (translate_lval fptr, Newspeak.FunPtr) in
	      K.FunDeref (fptr_exp, (args_t, ret_type))
	| _ -> error "Npkcompile.translate_call" "Left value not supported"
    in
    let call_w_prolog = (List.rev args_prolog)@((K.Call fexp, loc)::[]) in
    let call_wo_ret = K.append_decls args_decls call_w_prolog in
    let res = K.append_decls ret_decl (call_wo_ret@ret_epilog) in
      Cilenv.restore_loc_cnt ();
      res

let translate_local d =
  Cilenv.loc_declare d;
  let (_, n, t, loc) = d in
    (n, t, loc)

let translate_fun name (locals, formals, body) =
  let (floc, ret_t) = Cilenv.get_funspec name in

  let args = Some (List.map (fun (_, name, t, _) -> (name, t)) formals) in
    Cilenv.reset_lbl_gen ();
    let status = Cilenv.empty_status () in
      
      if ret_t <> None then Cilenv.push_local ();
      List.iter Cilenv.loc_declare formals;
      let locals = List.map translate_local locals in

      let body = translate_stmts status body.bstmts in
      let lbl = Cilenv.get_ret_lbl () in
      let blk = [K.DoWith (body, lbl, []), floc] in
      let body = K.append_decls locals blk in

	Cilenv.update_funspec name (args, body)


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

  and expand_elem prefix (off, i) = expand (addOffset off prefix) i in

    match x with
	None -> None
      | Some i ->
	  expand NoOffset i;
	  Some (List.rev !glb_inits)

(* TODO: maybe should put first pass into npkcompile ?? *)
let translate_glb used_glb name x =
  let used = K.String_set.mem name used_glb in
  let defd = x.F.gdefd in
  let loc = Npkutils.translate_loc x.F.gloc in
    Npkcontext.set_loc loc;
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
	Hashtbl.add Cilenv.glb_decls name (t, loc, init, used)
    end


(*=========================================*)
(* compile function, which wraps translate *)
(*=========================================*)

let compile in_name =
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
      Cilutils.dump stdout cil_file;
      print_newline ();
      print_newline ()
    end;

    print_debug "Running first pass...";
    Npkcontext.forget_loc ();
    let (glb_used, fun_specs, glb_decls) = F.first_pass cil_file in
      Npkcontext.forget_loc ();
      print_debug "First pass done.";
      
      print_debug ("Translating "^in_name^"...");

      Hashtbl.iter translate_fun fun_specs;
      Hashtbl.iter (translate_glb glb_used) glb_decls;
      
      let prog = Cilenv.create_npkil in_name in
	
	Npkcontext.forget_loc ();
	Cilenv.init_env ();
	prog
