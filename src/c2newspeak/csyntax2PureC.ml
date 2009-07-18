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

(* TODO: 
   -> Parsing 
   -> simplification: (desugarize, normalisation) side-effects elimination, 
   boolean expression normalization, redundant
   expressions/statement (++, +=) removal
   -> CoreC 
   -> Typing: typing + implicit casts addition
   -> TypedC 
   -> optional Goto eliminatination *)

(* TODO: introduce progressively language PureC!! *)

open Csyntax
module Nat = Newspeak.Nat
module C = Csyntax


let exp_of_effects (pref, e, post) = 
  let loc = Npkcontext.get_loc () in
(* TODO: remove BlkExp avec csyntax2PureC!!! *)
  let e = 
    if pref = [] then e
    else C.BlkExp (pref@(C.Exp e, loc)::[], false) 
  in
    if post = [] then e
    else C.BlkExp (post@(C.Exp e, loc)::[], true)

let concat_effects blk1 blk2 =
  if (blk1 <> []) && (blk2 <> []) then begin
    Npkcontext.report_warning "Csyntax2PureC.concat_effect" 
      ("the order of execution of side-effects in expressions not specified, "
       ^"picking a random one, be careful")
  end;
  (* TODO: Could pick randomly this sequence *)
  blk1@blk2

(* TODO: have a warning for assignments within expressions!!! *)
let process (globals, specs) =
  let vcnt = ref 0 in
  let gen_tmp t = 
    let x = "!tmp"^(string_of_int (!vcnt)) in
    let decl = C.LocalDecl (x, C.VDecl (t, false, false, None)) in
      if (!vcnt = max_int) 
      then Npkcontext.report_error "Csyntax2PureC" "no more ids";
      incr vcnt;
      (decl, Var x)
  in

  let rec simplify_exp e =
    match e with
	Cst c -> ([], C.Cst c, [])
      | Var x -> ([], C.Var x, [])
      | RetVar -> ([], C.RetVar, [])
      | Field (e, f) -> 
	  let (pref, e, post) = simplify_exp e in
	    (pref, C.Field (e, f), post)
      | Index (a, idx) -> 
	  let (pref1, a, post1) = simplify_exp a in
	  let (pref2, idx, post2) = simplify_exp idx in
	  let pref = concat_effects pref1 pref2 in
	  let post = concat_effects post2 post1 in
	    (pref, C.Index (a, idx), post)
      | AddrOf e -> 
	  let (pref, e, post) = simplify_exp e in
	    (pref, C.AddrOf e, post)
      | Deref e -> 
	  let (pref, e, post) = simplify_exp e in
	    (pref, C.Deref e, post)
      | Unop (op, e) ->
	  let (pref, e, post) = simplify_exp e in
	    (pref, C.Unop (op, e), post)
      | Binop (op, e1, e2) -> 
	  let (pref1, e1, post1) = simplify_exp e1 in
	  let (pref2, e2, post2) = simplify_exp e2 in
	  let pref = concat_effects pref1 pref2 in
	  let post = concat_effects post2 post1 in
	    (pref, C.Binop (op, e1, e2), post)
      | Set set -> 
	  Npkcontext.report_accept_warning "Firstpass.translate_exp" 
	    "assignment within expression" Npkcontext.DirtySyntax;
	  simplify_set set
      | Call call ->
	  let loc = Npkcontext.get_loc () in
	  let (pref, call) = simplify_call call in
	  let (decl, lv) = gen_tmp (C.Typeof call) in
	  let call = C.Exp (C.Set (lv, None, call)) in
	    (pref@(decl, loc)::(call, loc)::[], lv, [])
      | OpExp (op, lv, is_after) -> 
	  let loc = Npkcontext.get_loc () in
	  let (pref, lv, post) = simplify_exp lv in
	  let e = C.Binop (op, lv, C.exp_of_int 1) in
	  let set = (C.Exp (C.Set (lv, None, e)), loc) in
	  let (pref, post) = 
	    if is_after then (pref, set::post) else (pref@set::[], post)
	  in
	    (pref, lv, post)
      | Str x -> ([], C.Str x, [])
      | FunName -> ([], C.FunName, [])
      | Sizeof t -> ([], C.Sizeof (simplify_typ t), [])
      | SizeofE e -> 
	  let (_, e, _) = simplify_exp e in
	    ([], C.SizeofE e, [])
      | Cast (e, t) -> 
	  let (pref, e, post) = simplify_exp e in
	  let t = simplify_typ t in
	    (pref, C.Cast (e, t), post)
      | BlkExp (blk, is_after) -> 
	  let (blk, e) =
	    match List.rev blk with
		(Exp e, _)::blk -> (List.rev blk, e)
	      | _ -> 
		  Npkcontext.report_error "Csyntax2PureC.simplify_exp" 
		    "non empty block expected"
	  in
	  let blk = simplify_blk blk in
	  let (pref, e, post) = simplify_exp e in
	  let (pref, post) =
	    if is_after then (pref, post@blk) else (blk@pref, post) 
	  in
	    (pref, e, post)
      | IfExp (Cst (Cir.CInt x, _), _, e) when Nat.compare x Nat.zero = 0 -> 
	  simplify_exp e
      | IfExp (Cst (Cir.CInt _, _), e, _) -> simplify_exp e
      | IfExp (c, e1, e2) ->  
	  let loc = Npkcontext.get_loc () in
	  let (decl, lv) = gen_tmp (C.Typeof e) in
	  let blk1 = (Exp (Set (lv, None, e1)), loc)::[] in
	  let blk2 = (Exp (Set (lv, None, e2)), loc)::[] in
	  let set = simplify_stmt (If (c, blk1, blk2), loc) in
	    ((decl, loc)::set::[], lv, [])
      | Offsetof (t, f) -> ([], C.Offsetof (simplify_typ t, f), [])
		  
  and simplify_set (lv, op, e) =
    let loc = Npkcontext.get_loc () in
      match e with
	  Call c -> 
	    let (pref1, lv) = simplify_exp_post lv in
	    let (pref2, e) = simplify_call c in
	    let pref = concat_effects pref1 pref2 in
	      (pref@(C.Exp (C.Set (lv, op, e)), loc)::[], lv, [])
	| _ -> 
	    let (pref1, lv, post1) = simplify_exp lv in
	    let (pref2, e, post2) = simplify_exp e in
	    let pref = concat_effects pref1 pref2 in
	      (pref@(C.Exp (C.Set (lv, op, e)), loc)::[], lv, post2@post1)

  and simplify_call (f, args) =
    let (pref1, f) = simplify_exp_post f in
    let (pref2, args) = simplify_args args in
    let pref = concat_effects pref1 pref2 in
      (pref, C.Call (f, args))

  and simplify_exp_post e =
    let loc = Npkcontext.get_loc () in
    let (pref, e, post) = simplify_exp e in
      if (post <> []) then begin
	let (decl, lv) = gen_tmp (C.Typeof e) in
	let set = C.Exp (C.Set (lv, None, e)) in
	  (pref@(decl, loc)::(set, loc)::post, lv)
      end else (pref, e)

  and simplify_args args = 
    match args with
	e::args -> 
	  let (pref1, args) = simplify_args args in
	  let (pref2, e) = simplify_exp_post e in
	  let pref = concat_effects pref1 pref2 in
	    (pref, e::args)
      | [] -> ([], [])

  and simplify_pure_exp e =
    let (pref, e, post) = simplify_exp e in
      if (pref <> []) || (post <> []) then begin
	Npkcontext.report_error "Csyntax2PureC.simplify_pure_exp" 
	  "expression without side-effects expected"
      end;
      e

  and simplify_exp_opt e =
    match e with
	Some e -> Some (simplify_pure_exp e)
      | None -> None

  and simplify_typ t =
    match t with
	Void -> C.Void
      | Int k -> C.Int k
      | Bitfield (k, sz) -> C.Bitfield (k, simplify_pure_exp sz)
      | Float n -> C.Float n
      | Ptr t -> C.Ptr (simplify_typ t)
      | Array (t, len) -> 
	  let t = simplify_typ t in
	  let len = simplify_exp_opt len in
	    C.Array (t, len)
      | Comp (name, is_struct) -> C.Comp (name, is_struct)
      | Fun ft -> C.Fun (simplify_ftyp ft)
      | Va_arg -> C.Va_arg
      | Typeof e -> C.Typeof (simplify_pure_exp e)

  and simplify_ftyp (args_t, ret_t) =
    let ret_t = simplify_typ ret_t in
    let args_t = 
      match args_t with
	  None -> None
	| Some args_t -> 
	    Some (List.map (fun (t, x) -> (simplify_typ t, x)) args_t)
    in
      (args_t, ret_t)

  and simplify_init x =
    match x with
	Data e -> 
	  let loc = Npkcontext.get_loc () in
	  let (pref, e, post) = simplify_exp e in
(* TODO: think about it. Not nice!!! *)
	  let e = 
	    if pref = [] then e
	    else C.BlkExp (pref@(C.Exp e, loc)::[], false) 
	  in
	  let e = 
	    if post = [] then e
	    else C.BlkExp (post@(C.Exp e, loc)::[], true) 
	  in
	    C.Data e
      | Sequence seq -> 
	  let seq = List.map (fun (f, i) -> (f, simplify_init i)) seq in
	    C.Sequence seq

  and simplify_bexp e =
    match e with
	IfExp (c, e1, e2) -> 
	  (* TODO: not really nice, improve *)
	  let c = simplify_bexp c in
	  let e1 = simplify_bexp e1 in
	  let e2 = simplify_bexp e2 in
	  let e = C.IfExp (c, e1, e2) in
	    (* TODO: think about it!! *)
	    e
      | Unop (Not, e) -> Unop (Not, simplify_bexp e)
      | _ -> exp_of_effects (simplify_exp e)

  and simplify_init_opt x =
    match x with
	None -> None
      | Some init -> Some (simplify_init init)

  and simplify_field_decl (t, f, loc) = (simplify_typ t, f, loc)

  and simplify_decl d =
    match d with
	VDecl (t, is_static, is_extern, init) -> 
	  let t = simplify_typ t in
	  let init = simplify_init_opt init in
	    C.VDecl (t, is_static, is_extern, init)
      | EDecl e -> EDecl (exp_of_effects (simplify_exp e))
      | CDecl (is_struct, fields) -> 
	  let fields = List.map simplify_field_decl fields in
	    C.CDecl (is_struct, fields)

  and simplify_blk x = List.map simplify_stmt x 

  and simplify_stmt (x, loc) =
    Npkcontext.set_loc loc;
    let x = simplify_stmtkind x in
      (x, loc)

  and simplify_stmtkind x =
    match x with
	LocalDecl (x, d) -> C.LocalDecl (x, simplify_decl d)
      | If (e, br1, br2) -> 
	  let e = simplify_bexp e in
	  let br1 = simplify_blk br1 in
	  let br2 = simplify_blk br2 in
	    C.If (e, br1, br2)
      | Exp e -> C.Block (simplify_stmt_exp e)
      | Return -> C.Return
      | Block body -> C.Block (simplify_blk body)
      | For (init, cond, body, suffix) -> 
	  let init = simplify_blk init in
	  let cond = simplify_bexp cond in
	  let body = simplify_blk body in
	  let suffix = simplify_blk suffix in
	    C.For (init, cond, body, suffix)
      | DoWhile (body, cond) -> 
	  let body = simplify_blk body in
	  let cond = simplify_bexp cond in
	    C.DoWhile (body, cond)
      | CSwitch (e, cases, default) -> 
	  let loc = Npkcontext.get_loc () in
	  let (pref, e, post) = simplify_exp e in
	  let cases = List.map (simplify_case post) cases in
	  let default = simplify_blk default in
	    C.Block (pref@(C.CSwitch (e, cases, default), loc)::[])
      | Break -> C.Break
      | Continue -> C.Continue
      | Goto lbl -> C.Goto lbl
      | Label lbl -> C.Label lbl
      | UserSpec x -> C.UserSpec x

  and simplify_case post (e, body, loc) = 
    let e = simplify_pure_exp e in
    let body = simplify_blk body in
      (e, post@body, loc)

(* TODO: I am sure, it could be simplified, with also simplify_set
   with a good destination language *)
  and simplify_stmt_exp e =
    match e with
	Set (lv, op, IfExp (c, e1, e2)) -> 
	  let e = IfExp (c, Set (lv, op, e1), Set (lv, op, e2)) in
	    simplify_stmt_exp e

      | IfExp (c, e1, e2) -> 
	  let loc = Npkcontext.get_loc () in
	  let blk1 = (Exp e1, loc)::[] in
	  let blk2 = (Exp e2, loc)::[] in
	    (simplify_stmtkind (If (c, blk1, blk2)), loc)::[]

      | Call call -> 
	  let loc = Npkcontext.get_loc () in
	  let (pref, call) = simplify_call call in
	    pref@(C.Exp call, loc)::[]

      | Set set -> 
	  let (pref, _, post) = simplify_set set in
	    pref@post

     | Cast (e, Void) -> 
	 Npkcontext.report_accept_warning "Csyntax2PureC.simplify_stmt_exp" 
	   "cast to void" Npkcontext.DirtySyntax;
	 simplify_stmt_exp e

      | _ -> 
	  let (pref, _, post) = simplify_exp e in
	    pref@post
  in

  let simplify_global (x, loc) =
    Npkcontext.set_loc loc;
    let x =
      match x with
	  FunctionDef (f, ft, static, body) -> 
	    let ft = simplify_ftyp ft in
	    let body = simplify_blk body in
	      C.FunctionDef (f, ft, static, body)
	| GlbDecl (x, d) -> C.GlbDecl (x, simplify_decl d)	  
    in
      (x, loc)
  in

  let globals = List.map simplify_global globals in
  (globals, specs)
