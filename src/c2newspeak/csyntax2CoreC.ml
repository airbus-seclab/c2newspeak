(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2009  Charles Hymans
  
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
*)

open Csyntax
module Nat = Newspeak.Nat
module C = CoreC

(* Constants *)
let ret_name = "!return"

(* TODO: should keep the enums in CoreC!!! such as csyntax!!
*)
(* TODO: not minimal, think about it *)
(* TODO: maybe possible to merge VarSymb and EnumSymb together
   but what about CompSymb??
*)
type symb =
  | VarSymb of string 
  | EnumSymb of C.exp
  | CompSymb of C.field_decl list

let find_field f r =
  try List.assoc f r 
  with Not_found -> 
    Npkcontext.report_error "Firstpass.translate_lv" 
      ("unknown field '"^f^"' in union or structure")

(* TODO: find a way to factor this with create_cstr?? *)
let seq_of_string str =
  let len = String.length str in
  let res = ref [(None, Data (exp_of_char '\x00'))] in
    for i = len - 1 downto 0 do
      res := (None, Data (exp_of_char str.[i]))::!res
    done;
    !res

let process (globals, specs) =
  (* TODO: find a way to remove Symbtbl and use a standard Hashtbl here! 
     but first needs to put the whole typing phase before firstpass
  *)
  let symbtbl = Symbtbl.create () in
  (* Used to generate static variables names *)
  let current_fun = ref "" in
  (* Counter of static variables, necessary to distinguish 2 statics in 
     different scope of the same function, who would have the same name
  *)
  let static_cnt = ref 0 in

  let get_static_name x loc =
    let (fname, _, _) = loc in
    let prefix = "!"^fname^"." in
    let prefix = 
      if !current_fun = "" then prefix else prefix^(!current_fun)^"."
    in
    let name = prefix^(string_of_int !static_cnt)^"."^x in
      incr static_cnt;
      name
  in

  let complete_typ_with_init t init =
    let rec process x =
      match x with
	  (Sequence seq, C.Array (t, None)) -> 
	    let n = C.exp_of_int (List.length seq) in
	      C.Array (t, Some n)
	| (_, t) -> t
    in
      match init with
	  None -> t
	| Some init -> process (init, t)
  in
    
  let add_var (t, x) = Symbtbl.bind symbtbl x (VarSymb x, t) in

(* TODO: find a way to factor declare_global and add_var
   maybe necessary to complete_typ_with_init in both cases...
   maybe just needs a is_global bool as argument..
   need to check with examples!!!
*)
  let update_global x name t = Symbtbl.update symbtbl x (VarSymb name, t) in

  let add_formals (args_t, ret_t) =
    add_var (ret_t, ret_name);
(* TODO: think about it, not nice to have this pattern match, since in
   a function declaration there are always arguments!! 
   None is not possible *)
    match args_t with
	Some args_t -> List.iter add_var args_t
      | None -> 
	  Npkcontext.report_error "Csyntax2CoreC.add_formals" "unreachable code"
  in
    
  let find_symb x = 
    try Symbtbl.find symbtbl x
    with Not_found -> 
      if (Gnuc.is_gnuc_token x) && (not !Npkcontext.accept_gnuc) then begin
	Npkcontext.report_accept_warning "Csyntax2CoreC.process.find_symb" 
	  ("unknown identifier "^x^", maybe a GNU C symbol") Npkcontext.GnuC
      end;
      Npkcontext.report_accept_warning "Csyntax2CoreC.process.find_symb" 
	("unknown identifier "^x^", maybe a function without prototype") 
	Npkcontext.MissingFunDecl;
      let info = (VarSymb x, C.Fun (None, C.int_typ)) in
	(* TODO: clean up find_compdef + clean up accesses to Symbtbl *)
	Symbtbl.bind symbtbl x info;
	info
  in

  let fields_of_comp name =
    try 
      let (c, _) = Symbtbl.find symbtbl name in
	match c with
	    CompSymb f -> f
	  | _ -> raise Not_found
    with Not_found -> 
      Npkcontext.report_error "Csyntax2CoreC.find_compdef" 
	("unknown structure or union "^name)
  in

  let update_funtyp f ft1 =
    let (symb, t) = Symbtbl.find symbtbl f in
    let ft2 = CoreC.ftyp_of_typ t in
    let ft = CoreC.min_ftyp ft1 ft2 in
      Symbtbl.update symbtbl f (symb, C.Fun ft)
  in

  let update_funsymb f static ft loc =
    let (fname, _, _) = loc in
    let f' = if static then "!"^fname^"."^f else f in
      try update_funtyp f ft
      with Not_found -> Symbtbl.bind symbtbl f (VarSymb f', C.Fun ft)
  in

  let translate_proto_ftyp f static (args, ret) loc =
    if args = None then begin
      Npkcontext.report_warning "Firstpass.check_proto_ftyp" 
	("incomplete prototype for function "^f)
    end;
    update_funsymb f static (args, ret) loc
  in

  let find_var x = 
    let (v, t) = find_symb x in
    let e =
      match v with
(* TODO: strange, see 696.c *)
	  VarSymb _ -> C.Var x
	| EnumSymb i -> i
	| _ -> 
	    Npkcontext.report_error "Csyntax2CoreC.find_var"
	      ("variable identifier expected: "^x)
    in
      (e, t)
  in

  let translate_unop x t = 
    match (x, t) with
	(Neg, (C.Int _| C.Float _)) -> (C.Neg, t)
      | (Not, C.Int _) -> (C.Not, C.int_typ)
      | (BNot, C.Int k) -> 
(* TODO: function promote should be in CoreC, not in Cir 
   (or even in Csyntax rather?) Or even better in Csyntax2CoreC??? *)
	  (C.BNot, C.Int (Cir.promote k))
      | _ -> 
	  Npkcontext.report_error "Csyntax2CoreC.translate_unop"
	    "unexpected unary operator and argument"
  in

  let translate_binop op t1 t2 = 
    let t = 
      match (op, t1, t2) with
	  (Minus, C.Ptr _, C.Ptr _) -> C.int_typ
	| ((Gt|Eq), _, _) -> C.int_typ
	| _ -> t1
    in
    let op =
(* TODO: have csyntax use coreC operators!!! idem for unop!! *) 
      match op with
	  Mult -> C.Mult
	| Plus -> C.Plus
	| Minus -> C.Minus
	| Div -> C.Div
	| Mod -> C.Mod
	| BAnd -> C.BAnd
	| BXor -> C.BXor
	| BOr -> C.BOr
	| Shiftl -> C.Shiftl
	| Shiftr -> C.Shiftr
	| Gt -> C.Gt
	| Eq -> C.Eq
    in
      (op, t)
  in

  let rec translate_exp_with_array e =
    match e with
	Cst (c, t) -> 
	  let t = translate_typ t in
	    (C.Cst (c, t), t)
      | Var x -> find_var x
      | Field (e, f) -> 
	  let (e, t) = translate_exp e in
	  let r = fields_of_comp (C.comp_of_typ t) in
	  let t = find_field f r in
	    (C.Field (e, f), t)
	      (* TODO: should merge Index and Deref in Csyntax, only have one of them!! *)
      | Index (a, idx) -> 
	  let (a, t) = translate_exp_with_array a in
	  let (idx, _) = translate_exp idx in begin
	      match t with
		  C.Array (t, _) -> (C.Index (a, idx), t)
		| C.Ptr t -> (C.Deref (C.Binop (C.Plus, a, idx)), t)
		| _ -> 
		    Npkcontext.report_error "Csyntax2CoreC.translate_exp"
		      "pointer or array expected"
	    end
(* TODO: should remove Deref from csyntax, use only index!! *)
      | Deref e -> translate_exp (Index (e, exp_of_int 0))
	  (*
	    let (e, t) = translate_exp e in
	    let t = C.deref_typ t in
	    (C.Deref e, t)
	  *)
      | AddrOf e -> 
	  let (e, t) = translate_exp_with_array e in
	    (C.AddrOf e, C.Ptr t)
      | Unop (op, e) -> 
	  let (e, t) = translate_exp e in
	  let (op, t) = translate_unop op t in
	    (C.Unop (op, e), t)
      | Binop (op, e1, e2) -> 
	  let (e1, t1) = translate_exp e1 in
	  let (e2, t2) = translate_exp e2 in
	  let (op, t) = translate_binop op t1 t2 in
	    (C.Binop (op, e1, e2), t)
      | IfExp (c, e1, e2) -> 
	  let (c, _) = translate_exp c in
	  let (e1, t) = translate_exp e1 in
	  let (e2, _) = translate_exp e2 in
	    (C.IfExp (c, e1, e2), t)
      | Call (f, args) -> 
	  let (f, (_, ret_t)) = translate_funexp f in
	  let args = translate_args args in
	    (C.Call (f, args), ret_t)
      | Sizeof t -> (C.Sizeof (translate_typ t), C.uint_typ)
      | SizeofE e -> 
	  let (e, _) = translate_exp e in
	    (C.SizeofE e, C.uint_typ)
      | Offsetof (t, f) -> (C.Offsetof (translate_typ t, f), C.uint_typ)
      | Str x -> (C.Str x, C.Ptr C.char_typ)
      | FunName -> translate_exp (Str !current_fun)
      | Cast (e, t) -> 
	  let (e, _) = translate_exp e in
	  let t = translate_typ t in
	    (C.Cast (e, t), t)
      | Set (lv, op, e) -> 
	  let (lv, t1) = translate_exp lv in
	  let (e, t2) = translate_exp e in
	  let (op, t) =
	    match op with
		None -> (None, t1)
	      | Some op -> 
		  let (op, t) = translate_binop op t1 t2 in
		    (Some op, t)
	  in
	    (C.Set (lv, op, e), t)
      | OpExp (op, e, is_after) ->
	  let (e, t) = translate_exp e in
	  let (op, t) = translate_binop op t C.int_typ in
	    (C.OpExp (op, e, is_after), t)
      | BlkExp blk -> 
	  let (blk, t) = translate_blk_exp blk in
	    (C.BlkExp blk, t)
	      
  and translate_exp e =
    match translate_exp_with_array e with
	(e, C.Array (t, _)) -> (C.AddrOf (C.Index (e, C.exp_of_int 0)), C.Ptr t)
      | x -> x

  (* TODO: introduce type funexp in corec??*)
  and translate_funexp f =
    let (f, ft) = translate_exp f in
      match ft with
	  C.Fun t -> (f, t)
	| C.Ptr (C.Fun t) -> (C.Deref f, t)
	| _ -> 
	    Npkcontext.report_error "Csyntax2CoreC.translate_call"
	      "function expression expected"

  and translate_args x = 
    let translate e = 
      let (e, _) = translate_exp e in
	e
    in
      List.map translate x

  and translate_typ t =
    match t with
	Void -> C.Void
      | Int i -> C.Int i
      | Bitfield (i, e) -> 
	  let (e, _) = translate_exp e in
	    C.Bitfield (i, e)
      | Float i -> C.Float i
      | Ptr t -> C.Ptr (translate_typ t)
      | Array (t, len) -> 
	  let t = translate_typ t in
	  let len = 
	    match len with
		None -> None
	      | Some e -> 
		  let (e, _) = translate_exp e in
		    Some e
	  in
	    C.Array (t, len)
      | Comp (x, is_struct) -> C.Comp (x, is_struct)
      | Fun ft -> C.Fun (translate_ftyp ft)
      | Va_arg -> C.Va_arg
      | Typeof x -> C.Typeof x

  and translate_ftyp (args_t, ret_t) =
    let args_t = 
      match args_t with
	  None -> None
	| Some x -> 
	    let x = List.map (fun (t, x) -> (translate_typ t, x)) x in
	      Some x
    in
    let ret_t = translate_typ ret_t in
      (args_t, ret_t)


  and translate_decl is_global loc x d =
    match d with
	VDecl (_, _, extern, Some _) when extern -> 
	  (* TODO: make a test for this case in local *)
	  Npkcontext.report_error "Firstpass.translate_global"
	    "extern globals can not be initizalized"
      | VDecl (_, static, extern, _) when static && extern -> 
	  (* TODO: make a test for this case in local *)
	  Npkcontext.report_error "Firstpass.translate_global"
	    ("static variable can not be extern")
      | VDecl (Fun _, _, _, Some _) -> 
	  (* TODO: make a test for this case in local *)
	  Npkcontext.report_error "Firstpass.translate_global"
	    ("unexpected initialization of function "^x)
      | VDecl (Fun ft, is_static, is_extern, _) -> 
	  let ft = translate_ftyp ft in
	    translate_proto_ftyp x is_static ft loc;
	    if (not is_global) then begin
	      Npkcontext.report_accept_warning "Firstpass.translate"
		"function declaration within block" Npkcontext.DirtySyntax
	    end;
	    C.VDecl (C.Fun ft, is_static, is_extern, None)	    
      | VDecl (t, is_static, is_extern, init) -> 
	  let t = translate_typ t in
	  let name = if is_static then get_static_name x loc else x in
	  let t = complete_typ_with_init t init in
	    if is_global || is_extern 
	    then update_global x name t
	    else add_var (t, x);
	    let init =
	      match init with
		  None -> None
		| Some init -> Some (translate_init t init)
	    in
	      C.VDecl (t, is_static, is_extern, init)
      | EDecl e -> 
	  let (e, _) = translate_exp e in
	    Symbtbl.bind symbtbl x (EnumSymb e, CoreC.int_typ);
	    C.EDecl e
      | CDecl (is_struct, fields) -> 
	  add_compdecl (x, (is_struct, fields));
	  let fields = List.map translate_field_decl fields in
	    C.CDecl (is_struct, fields)

  and translate_stmt (x, loc) = 
    Npkcontext.set_loc loc;
    match x with
	LocalDecl (x, d) -> C.LocalDecl (x, translate_decl false loc x d)
      | If (c, br1, br2) -> 
	  let (c, _) = translate_exp c in
	  let br1 = translate_blk br1 in
	  let br2 = translate_blk br2 in
	    C.If (c, br1, br2)
      | CSwitch (e, cases, default) -> 
	  let (e, _) = translate_exp e in
	  let cases = List.map translate_case cases in
	  let default = translate_blk default in
	    C.CSwitch (e, cases, default)
      | For (init, halt, body, continue) -> 
	  let init = translate_blk init in
	  let (halt, _) = translate_exp halt in
	  let body = translate_blk body in
	  let continue = translate_blk continue in
	    C.For (init, halt, body, continue)
      | DoWhile (body, e) -> 
	  let body = translate_blk body in
	  let (e, _) = translate_exp e in
	    C.DoWhile (body, e)
      | Exp e -> 
	  let (e, _) = translate_exp e in
	    C.Exp e
      | Break -> C.Break
      | Continue -> C.Continue
      | Return e -> 
	  let e = 
	    match e with
		None -> None
	      | Some e -> 
		  let (e, _) = translate_exp e in
		    Some e
	  in
	    C.Return e
      | Block body -> C.Block (translate_blk body)
      | Goto lbl -> C.Goto lbl
      | Label lbl -> C.Label lbl
      | UserSpec a ->  C.UserSpec (translate_assertion a)

  and translate_case (e, body, loc) = 
    let (e, _) = translate_exp e in
      (e, translate_blk body, loc)

  and translate_spec_token x =
    match x with
	SymbolToken x -> C.SymbolToken x
      | IdentToken x -> C.IdentToken x
      | CstToken (c, _) -> C.CstToken c
	  
  and translate_assertion x = List.map translate_spec_token x

  and translate_blk x = 
    (* TODO: remove this and replace symbtbl by a standard hashtbl *)
    Symbtbl.save symbtbl;
    let x = List.map (fun (x, loc) -> (translate_stmt (x, loc), loc)) x in
      Symbtbl.restore symbtbl;
      x

  and translate_blk_exp x =
    let rec translate_aux x =
      match x with
	  (Exp e, loc)::[] -> 
	    let (e, t) = translate_exp e in
	      ([], (e, t, loc))
	| (hd, loc)::tl -> 
	    let hd = (translate_stmt (hd, loc), loc) in
	    let (blk, e) = translate_aux tl in
	      (hd::blk, e)
	| [] -> 
	    Npkcontext.report_error "Csyntax2CoreC.translate_blk_exp" 
	      "expression expected at end of block"
    in
(*      Symbtbl.save symbtbl;*)
      let (blk, (e, t, loc)) = translate_aux x in
(*	Symbtbl.restore symbtbl;*)
	(blk@(C.Exp e, loc)::[], t)
  
  and add_compdecl (x, (is_struct, f)) =
    let f = List.map translate_field_decl f in
    let data = (CompSymb f, C.Comp (x, is_struct)) in
      Symbtbl.bind symbtbl x data

  and translate_field_decl (t, x, _) = (x, translate_typ t)

  and translate_init t x =
    match (x, t) with
	(Data e, _) -> 
	  let (e, _) = translate_exp e in
	    C.Data e
      | (Sequence seq, C.Array (t, _)) -> 
	  let seq = 
	    List.map (fun (x, init) -> (x, translate_init t init)) seq 
	  in
	    C.Sequence seq
      
      | (Sequence seq, C.Comp (s, true)) ->
	  let f = fields_of_comp s in
	    C.Sequence (translate_field_sequence seq f)

      | (Sequence ((Some f, init)::[]), C.Comp (s, false)) -> 
	  let r = fields_of_comp s in
	  let t = find_field f r in
	  let seq = (Some f, translate_init t init)::[] in
	    C.Sequence seq

      | (Sequence _, _) -> 
	  Npkcontext.report_error "Csyntax2CoreC.translate_init"
	    "this type of initialization not implemented yet"

  and translate_field_sequence seq fields =
    match (fields, seq) with
	((_, t)::fields, (expected_f, init)::seq) -> 
	  let init = translate_init t init in
	  let seq = translate_field_sequence seq fields in
	    (expected_f, init)::seq
      
      | ([], []) -> []

      | (_, []) ->
	  (*TODO:remove	    if (fields = []) then begin*)
	  Npkcontext.report_accept_warning 
	    "Firstpass.translate_init.translate_field_sequence" 
	    "missing initializers for structure" Npkcontext.DirtySyntax;
	    (*TODO:remove end;*)
	    []

      | _ -> Npkcontext.report_error "Csyntax2CoreC" "case not implemented yet"
  in

  let translate_global (x, loc) = 
    Npkcontext.set_loc loc;
    let x = 
      match x with
	  FunctionDef (f, (args_t, ret_t), static, body) -> 
	    current_fun := f;
	    let args_t = 
	      match args_t with
		  None -> []
		| Some args_t -> args_t
	    in
	    let ft = (Some args_t, ret_t) in
	    let ft = translate_ftyp ft in
	      update_funsymb f static ft loc;
	      Symbtbl.save symbtbl;
	      add_formals ft;
	      let body = translate_blk body in
		Symbtbl.restore symbtbl;
		current_fun := "";
		C.FunctionDef (f, ft, static, body)

	| GlbDecl (x, d) -> C.GlbDecl (x, translate_decl true loc x d)
    in
      (x, loc)
  in

  let globals = List.map translate_global globals in
  let specs = List.map translate_assertion specs in
    (globals, specs)
