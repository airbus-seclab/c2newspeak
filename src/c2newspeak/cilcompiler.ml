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

open Npkutils

module F = Cilfirstpass
module K = Npkil

module Nat = Newspeak.Nat

let set_loc loc = Npkcontext.set_loc (Npkutils.translate_loc loc)

let build_stmt stmtkind = (stmtkind, Npkcontext.get_loc ())

(*===================================*)
(* Misc. functions used by translate *)
(*===================================*)

let is_cil_label x =
  match x with
      Label (_, _, false) -> true
    | _ -> false

let size_of_array t lv =
  let (elt_sz, len) =
    match Cil.unrollType t with
	TArray (elt_t, len, _) -> (Cilutils.size_of elt_t, len)
    | _ -> Npkcontext.report_error "Cilcompiler.size_of_array" "array expected"
  in
  let len =
    try K.Known (Nat.of_int (Cil.lenOfArray len))
    with LenOfArray -> 
      match lv with
	  K.Global v -> K.Length v
	| _ -> 
	    Npkcontext.report_error "Cilcompiler.size_of_array" 
	      "unknown length of array"
  in
    (len, elt_sz)
	    


(*================================*)
(* The central translate function *)
(*================================*)

let rec translate_const c =
  match c with
    | CInt64 (i, k, _) ->
	let x = Nat.of_string (Int64.to_string i) in
	let (s, n) = translate_ikind k in
	let x =
	  match s with
(* Carefull that CIL uses Int64 negative values to represent large 
   unsigned 64 bits integers! *)
	      Newspeak.Unsigned when i < Int64.zero -> Nat.neg x
	    | _ -> x
	in
	  if n > 64 then begin
	    Npkcontext.report_error "Cilcompiler.translate_const" 
	      "integer too large: not representable"
	  end;
	  K.Const (Newspeak.CInt x)
	  
    | CStr s -> Cilenv.get_cstr s

    | CChr c -> K.exp_of_int (Char.code c)
	
    | CReal (f, _, Some s) -> K.Const (Newspeak.CFloat (f, s))
    | CReal (f, _, None) ->
	let s = string_of_float f in
	  Npkcontext.print_debug
	    ("Npkcompile.translate_const:"
	     ^" No string representation available for const "^s);
	  K.Const (Newspeak.CFloat (f, s))
	    
    | CWStr _ | CEnum _	-> 
	Npkcontext.report_error "Npkcompile.translate_const"
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
		Npkcontext.report_error "Npkcompile.translate_cast"
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
	  
    | (Newspeak.Int (sign, _) as kt', (Newspeak.Float _ as kt)) ->
	if sign = Newspeak.Unsigned then begin 
	  Npkcontext.report_warning "Npkcompile.translate_scalar_cast"
	    ("cast from float to unsigned integer: "
	      ^"sign may be lost: "^(K.string_of_cast t2 t1))
	end;
	(K.UnOp (K.Cast (kt, kt'), e))
	  
    | (Newspeak.Ptr, Newspeak.Ptr) -> e

    | (Newspeak.FunPtr, Newspeak.FunPtr) -> e
		      
    | (Newspeak.Int (sign, sz), Newspeak.Ptr) when sz = Config.size_of_ptr ->
	Npkcontext.report_accept_warning "Npkcompile.translate_scalar_cast"
	  ("dirty cast "^(K.string_of_cast t2 t1)) Npkcontext.DirtyCast;
	  K.UnOp (K.PtrToInt (sign, sz), e)
	    
    | (Newspeak.Ptr, Newspeak.Int (sign, sz)) when sz = Config.size_of_ptr ->
	Npkcontext.report_accept_warning "Npkcompile.translate_scalar_cast"
	  ("dirty cast "^(K.string_of_cast t2 t1)) Npkcontext.DirtyCast;
	  K.UnOp (K.IntToPtr (sign, sz), e)
	    
    | (Newspeak.Ptr as kt'), (Newspeak.FunPtr as kt) ->
	Npkcontext.report_accept_warning "Npkcompile.translate_scalar_cast"
	  ("dirty cast "^(K.string_of_cast t2 t1)) Npkcontext.DirtyCast;
	K.UnOp (K.Cast (kt, kt'), e)

    | _ -> 
	Npkcontext.report_error "Cilcompiler.translate_scalar_cast"
	  ("invalid cast "^(K.string_of_cast t2 t1))
     
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
	      Field (_, NoOffset) ->
		let o = Cilutils.offset_of t offs in
		  K.Shift (translate_lval lv', K.exp_of_int o)
		    
	    | Index (idx, NoOffset) ->
		let lv' = translate_lval lv' in
		let (len, elt_sz) = size_of_array t lv' in
		let idx = translate_exp idx in
		let checked_index = 
		  K.UnOp (K.Belongs_tmp (Nat.zero, len), idx) 
		in
		let offs = 
		  K.BinOp (Newspeak.MultI, checked_index, K.exp_of_int elt_sz)
		in
		  K.Shift (lv', offs)
		    
	    | _ -> 
		Npkcontext.report_error "Npkcompile.translate_lval" 
		  "offset not handled"


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
	  | _ -> 
	      Npkcontext.report_error "Npkcompile.translate_exp" 
		"integer or float type expected"
      end
	
    | UnOp (BNot, e, t) -> begin
	match translate_typ t with
	    K.Scalar (Newspeak.Int int_t) -> 
	      let b = Newspeak.domain_of_typ int_t in
		K.UnOp (K.BNot b, translate_exp e)
	  | _ -> 
	      Npkcontext.report_error "Npkcompile.translate_exp.BNot" 
		"integer type expected"
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
	      | (K.Scalar Newspeak.Ptr, K.Scalar (Newspeak.Int _)) ->
		  let e = translate_exp e in
		    K.BinOp (Newspeak.Eq Newspeak.Ptr, e, K.Const Newspeak.Nil)
	      | _ -> 
		  Npkcontext.report_error "Npkcompile.translate_exp.LNot" 
		    "integer type expected"
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
	  | _ ->
	      Npkcontext.report_error "Npkcompile.translate_exp" 
		"integer or float type expected"
      end

    | BinOp (Mod as o, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar (Newspeak.Int _) ->
	      (K.BinOp (translate_arith_binop o,
		       translate_exp e1, translate_exp e2))
		
	  | K.Scalar (Newspeak.Float sz) ->
	      K.BinOp (translate_float_binop sz o, 
		       translate_exp e1, translate_exp e2)
	  | _ -> 
	      Npkcontext.report_error "Npkcompile.translate_exp" 
		"integer or float type expected"
      end
	
    (* Bitwise operations *)
    | BinOp (BAnd as o, e1, e2, t)    | BinOp (BOr as o, e1, e2, t)
    | BinOp (BXor as o, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar (Newspeak.Int int_t) ->
	      K.BinOp (translate_logical_binop int_t o,
		       translate_exp e1, translate_exp e2)
	  | _ -> 
	      Npkcontext.report_error "Npkcompile.translate_exp.Bop" 
		"integer type expected"
      end
	
    (* Logical operations *)
    | BinOp (Shiftlt as o, e1, e2, t) | BinOp (Shiftrt as o, e1, e2, t) -> begin
	match translate_typ t with
	  | K.Scalar (Newspeak.Int int_t) ->
	      K.make_int_coerce int_t (K.BinOp (translate_logical_binop int_t o,
						translate_exp e1, translate_exp e2))
	  | _ -> 
	      Npkcontext.report_error "Npkcompile.translate_exp.Shift" 
		"integer type expected"
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
	      Npkcontext.report_error "Npkcompile.translate_exp"
		"pointer arithmetic forbidden on function pointers"
	  | _ -> 
	      Npkcontext.report_error "Npkcompile.translate_exp" 
		"data pointer type expected"
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
	      Npkcontext.report_error "Npkcompile.translate_exp"
		"pointer arithmetic forbidden on function pointers"
	  | _ ->
	      Npkcontext.report_error "Npkcompile.translate_exp" 
		"data pointer type expected"
      end
	
    (* Pointer difference *) 
    | BinOp (MinusPP, e1, e2, t) -> begin
	let t1 = typeOf e1 in
	let t2 = typeOf e2 in
	match translate_typ t1, translate_typ t2, translate_typ t with
	  | K.Scalar Newspeak.Ptr, K.Scalar Newspeak.Ptr, 
	    K.Scalar Newspeak.Int int_t -> 
	      let v1 = translate_exp e1 in
	      let v2 = translate_exp e2 in
	      let n = Cilutils.size_of_subtyp t1 in
		K.make_int_coerce int_t (
		  K.BinOp (Newspeak.DivI, 
			 K.BinOp (Newspeak.MinusPP, v1, v2), K.exp_of_int n))
	  | _ -> 
	      Npkcontext.report_error "Npkcompile.translate_exp" "data pointer type expected"
      end
	
    | Lval lv -> K.Lval (translate_lval lv, translate_typ (typeOfLval lv))
	
    | AddrOf lv -> begin
	match lv, translate_typ (typeOf e) with
	  | (Var f, NoOffset), K.Scalar Newspeak.FunPtr ->
	      let ft = Npkutils.ftyp_of_typ (typeOfLval lv) in
	      let (args, ret) = Npkutils.translate_ftyp ft in
	      let ft =
		match args with
		    Some args -> (List.map snd args, ret)
		  | None -> 
		      Npkcontext.report_error "Cilcompiler.translate_exp" 
			"case not handled yet"
	      in
		K.AddrOfFun (f.vname, ft)
	      
	  | _, K.Scalar Newspeak.Ptr ->
	      let (lv', offs) = removeOffsetLval lv in begin
		match offs with 
		  | Index (Const CInt64 (i, _, _), NoOffset) 
		      when Int64.compare i Int64.zero = 0 -> 
		      let t = typeOfLval lv' in
		      let lv' = translate_lval lv' in
		      let sz = K.Mult (size_of_array t lv') in
			K.AddrOf (lv', sz)
			  
		  | Index (e, NoOffset) ->
		      let t = TPtr (typeOfLval lv, []) in
		      let offset = Index (Cil.zero, NoOffset) in
		      let base = AddrOf (addOffsetLval offset lv') in
			translate_exp (BinOp (PlusPI, base, e, t))
			
		  | _ -> 
		      let sz = Cilutils.size_of (typeOfLval lv) in
			K.AddrOf (translate_lval lv, K.Known (Nat.of_int sz))
		end
							 
	  | _ -> 
	      Npkcontext.report_error "Npkcompile.translate_exp"
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
	Npkcontext.report_error "Npkcompile.translate_exp"
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
	  translate_switch loc status e stmt_list body
	    
      | Goto (x, _) ->
	  let rec explore_labels l =
	    match l with
		[] -> 
		  Npkcontext.report_error "Npkcompile.translate_stmtkind" "unexpected goto"
	      | (Label (_, _, false) as l)::r -> begin
		  try
		    translate_stmts status 
		      (Hashtbl.find F.code_to_duplicate l)
		  with Not_found -> explore_labels r
		end
	      | _::r -> explore_labels r
	  in
	    explore_labels (!x.labels)
	      
      | Continue _ | TryFinally _ | TryExcept _ ->
	  Npkcontext.report_error "Npkcompile.translate_stmtkind" "stmtkind not handled"
	  
	  

	  
and translate_stmts status stmts =
  match stmts with
      [] -> []
    | stmt::r when List.for_all is_cil_label stmt.labels ->
	let first = translate_stmtkind status stmt.skind in
	  first@(translate_stmts status r)
    | stmt::_ ->
	match stmt.labels with
	    (Case _ | Default _)::_ ->
	      Npkcontext.report_error "Npkcompile.translate_stmts" 
		"unstructed use of switch statement"
	  | _ -> 
	      Npkcontext.report_error "Npkcompile.translate_stmts" 
		"unexpected label"

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

    | Call (x, Lval lv, args, _) -> 
	let call = translate_call x lv args in
	  (build_stmt call)::[]

    | Call _ ->
	Npkcontext.report_error "Npkcompile.translate_instr"
	  ("call '"^(Cilutils.string_of_instr i)^"' not handled")

    | Asm _ ->
	Npkcontext.report_error "Npkcompile.translate_instr" 
	  "asm block not supported"
	  

	  
	  
and translate_set lval typ e = K.Set (lval, translate_exp e, typ)
	

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
      | UnOp (LNot, e, _) -> begin
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
  let body1 = (K.Guard cond1, loc)::(translate_stmts status stmts1) in
  let body2 = (K.Guard cond2, loc)::(translate_stmts status stmts2) in
    (K.Select (body1, body2), loc)::[]


(** Returns a set of blocks, each representing a choice of the case 
    statement. Each case is translated by appending a guard.
    It also builds the guard of the default statement. *)
and translate_switch switch_loc status e stmt_list body =
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
		| _ -> 
		    Npkcontext.report_error "Npkcompile.tranlate_switch" 
		      "expression not scalar"
	    in
	    let cond = K.BinOp (Newspeak.Eq t, switch_exp, translate_exp v) in
	      status := Cilenv.add_switch_lbl !status loc lbl;
	      translate_aux tl;
	      let body = 
		(K.Guard cond, switch_loc)::(build_stmt (K.Goto lbl))::[]
	      in
		cases := (K.Select (body, !cases), switch_loc)::[];
		default_cond := 
		  (K.Guard (K.negate cond), switch_loc)::!default_cond
		
	| (Default loc)::tl -> 
	    (* TODO: have a get_default_lbl instead, with a default number 
	     for it, maybe, maybe not?? *)
	    status := Cilenv.add_switch_lbl !status loc lbl;
	    default_goto := [build_stmt (K.Goto lbl)];
	    translate_aux tl
	      
	| [] -> ()
	    
	| _ -> Npkcontext.report_error "Npkcompile.translate_switch_cases" "invalid label"
    in
      translate_aux x.labels
  in
    
    Cilenv.reset_lbl_gen ();
    List.iter translate_case (List.rev stmt_list);
    let default_choice = !default_cond@(!default_goto) in
    let choices = [build_stmt (K.Select (default_choice, !cases))] in

      translate_switch_body (Npkcontext.get_loc ()) !status choices body.bstmts

and translate_switch_body loc status body stmts =
  match stmts with
      [] -> [K.DoWith (body, Cilenv.get_brk_lbl (), []), loc]

    | hd::tl when hd.labels = [] ->
	let hd = translate_stmtkind status hd.skind in
	  translate_switch_body loc status (body@hd) tl

    | hd::tl -> 
	let lbl = 
	  match hd.labels with
	      (Case (_, loc))::_ | (Default loc)::_ ->
		set_loc loc;
		Cilenv.get_switch_lbl status loc
	    | _ -> Npkcontext.report_error "Npkcompile.translate_switch_body" "invalid label"
	in

	let body = [build_stmt (K.DoWith (body, lbl, []))] in
	  
	let hd = translate_stmtkind status hd.skind in
	  translate_switch_body loc status (body@hd) tl

and translate_call ret f args =
  let args_t = Some (List.map (fun e -> ("", typeOf e, [])) args) in
  let ret_t =
    match f with
      | (Var f, NoOffset) ->
	  let ret_t = 
	    match f.vtype with
		TFun (ret, _, _, _) -> ret
	      | _ -> 		    
		  Npkcontext.report_error "Npkcompile.translate_call"
		    ("invalid type '"^(Cilutils.string_of_type f.vtype)^"'")
	  in
	    ret_t

      | (Mem (Lval fptr), NoOffset) ->
	  let typ = translate_typ (typeOfLval fptr) in
	    if typ <> K.Scalar Newspeak.FunPtr then begin
	      Npkcontext.report_error "Npkcompile.translate_call" 
		"FunPtr expected"
	    end;
	    begin
	      match ret with
		| None -> TVoid []
		| Some cil_lv -> typeOfLval cil_lv
	    end
      | _ -> 
	  Npkcontext.report_error "Npkcompile.translate_call" 
	    "left value not supported"
  in
  let ft = Npkutils.translate_ftyp (args_t, ret_t) in
  let ft' =
    match ft with
	(Some args, ret) -> (List.map snd args, ret)
      | _ -> 
	  Npkcontext.report_error "Cilcompiler.translate_exp" 
	    "case not handled yet"
  in
  let args = List.map (fun x -> K.In (translate_exp x)) args in
  let ret =
    match ret with
	None    -> None
      | Some lv -> Some (translate_lval lv)
  in
  let fn = 
    match f with
      | (Var f, NoOffset) -> K.FunId f.vname
      | (Mem (Lval fptr), NoOffset) ->
	  let fptr_exp = 
	    K.Lval (translate_lval fptr, K.Scalar Newspeak.FunPtr) 
	  in
	    K.FunDeref fptr_exp
      | _ -> 
	  Npkcontext.report_error "Npkcompile.translate_call" 
	    "left value not supported"
  in begin
    match f with
	(Var f, NoOffset) -> Cilenv.update_fun_proto f.vname ft
      | _ -> ()
    end;
    K.Call (args, ft', fn, ret)

let translate_local (_, n, t, loc) = (n, t, loc)

let translate_fun name (locals, formals, body) =
  let (floc, ret_t) = Cilenv.get_funspec name in

  let args = Some (List.map (fun (_, name, t, _) -> (name, t)) formals) in
    Cilenv.reset_lbl_gen ();
    let status = Cilenv.empty_status () in
      
      if ret_t <> None then Cilenv.push_local ();
      let ret_ids = if ret_t <> None then "!ret"::[] else [] in
      let arg_ids = List.map (fun (_, n, _, _) -> n) formals in
      let locals = List.map translate_local locals in

      let body = translate_stmts status body.bstmts in
      let lbl = Cilenv.get_ret_lbl () in
      let blk = [K.DoWith (body, lbl, []), floc] in
      let body = K.append_decls locals blk in

	Cilenv.update_funspec name (ret_ids, arg_ids, args, body)


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
		  Npkcontext.report_error "Npklink.translate_init" 
		    "unexpected type of SingleInit"
	  in
	    glb_inits := v::(!glb_inits)
      | CompoundInit (_, c) -> List.iter (expand_elem off) c

  and expand_elem prefix (off, i) = expand (addOffset off prefix) i in

    match x with
	None -> Npkil.Declared false
      | Some i ->
	  expand NoOffset i;
(*	  Some (List.rev !glb_inits)*)
	  Npkil.Declared true

(* TODO: maybe should put first pass into npkcompile ?? *)
let translate_glb used_glb name x =
  let used = K.String_set.mem name used_glb in
  let defd = x.F.gdefd in
  let loc = Npkutils.translate_loc x.F.gloc in
    Npkcontext.set_loc loc;
    if (defd || used) then begin
      let init =
	if defd then translate_init x.F.ginit x.F.gtype
	else begin
	  assert (x.F.ginit = None);
	  Npkil.Extern
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
  Cilutils.setCilPrinter !Npkcontext.cil_printer;
  useLogicalOperators := false;

  Npkcontext.print_debug ("Parsing "^in_name^"...");
  let cil_file = Frontc.parse in_name () in
    Npkcontext.print_debug ("Parsing done.");
    if !Npkcontext.verb_ast then begin
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

    Npkcontext.print_debug "Running first pass...";
    Npkcontext.forget_loc ();
    let (glb_used, fun_specs, glb_decls) = F.first_pass cil_file in
      Npkcontext.forget_loc ();
      Npkcontext.print_debug "First pass done.";
      
      Npkcontext.print_debug ("Translating "^in_name^"...");

      Hashtbl.iter translate_fun fun_specs;
      Hashtbl.iter (translate_glb glb_used) glb_decls;
      
      let prog = Cilenv.create_npkil in_name in
	
	Npkcontext.forget_loc ();
	Cilenv.init_env ();
	prog
