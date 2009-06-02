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

(* TODO: not minimal, think about it *)
type symb =
  | GlobalSymb of string 
  | LocalSymb of C.exp 
  | EnumSymb of C.exp
  | CompSymb of C.field_decl list

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
      with Not_found -> Symbtbl.bind symbtbl f (GlobalSymb f', C.Fun ft)
  in

  let translate_proto_ftyp f static (args, ret) loc =
    if args = None then begin
      Npkcontext.report_warning "Firstpass.check_proto_ftyp" 
	("incomplete prototype for function "^f)
    end;
    update_funsymb f static (args, ret) loc
  in

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
	  (C.Sequence seq, C.Array (t, None)) -> 
	    let n = C.exp_of_int (List.length seq) in
	      C.Array (t, Some n)
	| (_, t) -> t
    in
      match init with
	  None -> t
	| Some init -> process (init, t)
  in

  let declare_global static x loc t init =
    let name = if static then get_static_name x loc else x in
    let t = complete_typ_with_init t init in
      Symbtbl.update symbtbl x (GlobalSymb name, t)
  in

  let translate_unop x = 
    match x with
	Neg -> C.Neg
      | Not -> C.Not
      | BNot -> C.BNot
  in

  let translate_binop x = 
    match x with
	Plus -> C.Plus
      | Minus -> C.Minus
      | Mult -> C.Mult
      | Div -> C.Div
      | Mod -> C.Mod
      | Gt -> C.Gt
      | Eq -> C.Eq
      | BAnd -> C.BAnd
      | BXor -> C.BXor
      | BOr -> C.BOr
      | Shiftl -> C.Shiftl
      | Shiftr -> C.Shiftr
  in

  let rec translate_exp e =
    match e with
	Cst c -> C.Cst (translate_cst c)
      | Var x -> C.Var x
      | Field (e, f) -> C.Field (translate_exp e, f)
      | Index (t, e) -> C.Index (translate_exp t, translate_exp e)
      | Deref e -> C.Deref (translate_exp e)
      | AddrOf e -> C.AddrOf (translate_exp e)
      | Unop (op, e) -> C.Unop (translate_unop op, translate_exp e)
      | Binop (op, e1, e2) -> 
	  let op = translate_binop op in
	  let e1 = translate_exp e1 in
	  let e2 = translate_exp e2 in
	    C.Binop (op, e1, e2)
      | IfExp (c, e1, e2) -> 
	  let c = translate_exp c in
	  let e1 = translate_exp e1 in
	  let e2 = translate_exp e2 in
	    C.IfExp (c, e1, e2)
      | Call (f, args) -> 
	  let f = translate_exp f in
	  let args = List.map translate_exp args in
	    C.Call (f, args)
      | Sizeof t -> C.Sizeof (translate_typ t)
      | SizeofE e -> C.SizeofE (translate_exp e)
      | Offsetof (t, f) -> C.Offsetof (translate_typ t, f)
      | Str x -> C.Str x
      | FunName -> C.FunName
      | Cast (e, t) -> C.Cast (translate_exp e, translate_typ t)
      | Set (lv, op, e) -> 
	  let lv = translate_exp lv in
	  let op =
	    match op with
		None -> None
	      | Some op -> Some (translate_binop op)
	  in
	  let e = translate_exp e in
	    C.Set (lv, op, e)
      | OpExp (op, e, is_after) ->
	  let op = translate_binop op in
	  let e = translate_exp e in
	    C.OpExp (op, e, is_after)
      | BlkExp blk -> C.BlkExp (translate_blk blk)	 

  and translate_typ t =
    match t with
	Void -> C.Void
      | Int i -> C.Int i
      | Bitfield (i, e) -> C.Bitfield (i, translate_exp e)
      | Float i -> C.Float i
      | Ptr t -> C.Ptr (translate_typ t)
      | Array (t, len) -> 
	  let t = translate_typ t in
	  let len = 
	    match len with
		None -> None
	      | Some e -> Some (translate_exp e)
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
	
  and translate_stmt (x, loc) = 
    Npkcontext.set_loc loc;
    match x with
	LocalDecl (x, d) -> C.LocalDecl (x, translate_decl loc x d)
      | If (c, br1, br2) -> 
	  let c = translate_exp c in
	  let br1 = translate_blk br1 in
	  let br2 = translate_blk br2 in
	    C.If (c, br1, br2)
      | CSwitch (e, cases, default) -> 
	  let e = translate_exp e in
	  let cases = List.map translate_case cases in
	  let default = translate_blk default in
	    C.CSwitch (e, cases, default)
      | For (init, halt, body, continue) -> 
	  let init = translate_blk init in
	  let halt = translate_exp halt in
	  let body = translate_blk body in
	  let continue = translate_blk continue in
	    C.For (init, halt, body, continue)
      | DoWhile (body, e) -> 
	  let body = translate_blk body in
	  let e = translate_exp e in
	    C.DoWhile (body, e)
      | Exp e -> C.Exp (translate_exp e)
      | Break -> C.Break
      | Continue -> C.Continue
      | Return e -> 
	  let e = 
	    match e with
		None -> None
	      | Some e -> Some (translate_exp e)
	  in
	    C.Return e
      | Block body -> C.Block (translate_blk body)
      | Goto lbl -> C.Goto lbl
      | Label lbl -> C.Label lbl
      | UserSpec a ->  C.UserSpec (translate_assertion a)

  and translate_case (e, body, loc) = (translate_exp e, translate_blk body, loc)

  and translate_cst (c, t) = (c, translate_typ t)

  and translate_spec_token x =
    match x with
	SymbolToken x -> C.SymbolToken x
      | IdentToken x -> C.IdentToken x
      | CstToken c -> C.CstToken (translate_cst c)
	  
  and translate_assertion x = List.map translate_spec_token x

  and translate_blk x = 
    List.map (fun (x, loc) -> (translate_stmt (x, loc), loc)) x 

  and translate_decl loc x d =
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
      | VDecl (t, is_static, is_extern, init) -> 
	  let t = translate_typ t in begin
	      match t with
		  C.Fun ft -> translate_proto_ftyp x is_static ft loc
		| _ -> ()
	    end;
	    let init =
	      match init with
		  None -> None
		| Some init -> Some (translate_init init)
	    in
	      C.VDecl (t, is_static, is_extern, init)
      | EDecl e -> 
	  let e = translate_exp e in
	    Symbtbl.bind symbtbl x (EnumSymb e, CoreC.int_typ);
	    C.EDecl e
      | CDecl (is_struct, fields) -> 
	  add_compdecl (x, (is_struct, fields));
	  let fields = List.map translate_field_decl fields in
	    C.CDecl (is_struct, fields)
  
  and add_compdecl (x, (is_struct, f)) =
    let f = List.map translate_field_decl f in
    let data = (CompSymb f, C.Comp (x, is_struct)) in
      Symbtbl.bind symbtbl x data

  and translate_field_decl (t, x, loc) = (translate_typ t, x, loc)

  and translate_init x =
    match x with
	Data e -> C.Data (translate_exp e)
      | Sequence seq -> 
	  C.Sequence (List.map (fun (x, init) -> (x, translate_init init)) seq)
  in

  let translate_global (x, loc) = 
    Npkcontext.set_loc loc;
    let x = 
      match x with
	  FunctionDef (f, (args_t, ret_t), static, body) -> 
	    let args_t = 
	      match args_t with
		  None -> []
		| Some args_t -> args_t
	    in
	    let ft = (Some args_t, ret_t) in
	    let ft = translate_ftyp ft in
	    let body = translate_blk body in
	      update_funsymb f static ft loc;
	      C.FunctionDef (f, ft, static, body)

	| GlbDecl (x, d) -> 
	    let d = translate_decl loc x d in begin
		match d with
		    C.VDecl (C.Fun _, _, _, _) -> ()
		  | C.VDecl (t, static, _, init) -> 
		      declare_global static x loc t init
		  | _ -> ()
	      end;
	      C.GlbDecl (x, d)
    in
      (x, loc)
  in

  let globals = List.map translate_global globals in
  let specs = List.map translate_assertion specs in
    (globals, specs)
