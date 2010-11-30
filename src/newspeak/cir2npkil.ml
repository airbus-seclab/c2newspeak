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
*)

open Cir

module N = Newspeak
module K = Npkil

module Nat = Newspeak.Nat

module Set = Set.Make(String)

let translate_scalar t =
  match t with
      Scalar t -> t
    | Void -> Npkcontext.report_error "Cir2npkil.translate_scalar" 
	"value void not ignored as it ought to be"
    | _ -> 
	Npkcontext.report_error "Cir2npkil.translate_scalar" 
	  ("unexpected non scalar type: "^(string_of_typ t))

let translate_arithmop op e1 e2 k = K.make_int_coerce k (K.BinOp (op, e1, e2))

let translate_cst c =
  match c with
      CInt i -> N.CInt i
    | CFloat f -> N.CFloat f

let translate src_lang prog fnames =
  let glbdecls = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in

  let used_glbs = ref Set.empty in

  (* Hashtbl of already translated types, to as to have some sharing *)
  let translated_typ = Hashtbl.create 100 in

  let rec translate_typ t =
    try Hashtbl.find translated_typ t 
    with Not_found ->
      let t' =
	match t with
	    Void -> 
	      Npkcontext.report_error "Compiler.translate_typ" 
		"type void not allowed here"
	  | Scalar t -> K.Scalar t
	  | Array (t, sz) -> K.Array (translate_typ t, sz)
	  | Struct (fields, sz) | Union (fields, sz) -> 
	      let translate_field (_, (o, t)) = (o, translate_typ t) in
		K.Region (List.map translate_field fields, sz)
	  | Fun -> 
	      Npkcontext.report_error "Compiler.translate_typ" 
		"function not allowed here"
      in
	Hashtbl.add translated_typ t t';
	t'
  in

  let translate_ftyp (args, ret) =
    let args = List.map translate_typ args in
    let args = args in
    let ret =
      match ret with
	  Void -> None
	| _ -> Some (translate_typ ret)
    in
      (args, ret)
  in

  let rec translate_lv lv =
    match lv with
	Local id -> K.Local id

      | Global x -> 
	  used_glbs := Set.add x !used_glbs;
	  K.Global x

      | Shift (lv, o) ->
	  let lv = translate_lv lv in
	  let o = translate_exp o in
	    K.Shift (lv, o)

      | Deref (e, t) ->
	  let e = translate_exp e in
	  let sz = size_of_typ t in
	    K.Deref (e, sz)

      | BlkLv _ ->
	  Npkcontext.report_error "Compiler.translate_lval"
	    "unexpected side-effect in left value"

  and translate_exp e =
    match e with
	Const i -> K.Const (translate_cst i)
      | Lval (lv, t) -> 
	  let lv = translate_lv lv in
	    K.Lval (lv, translate_typ t)

      | AddrOfFun (f, ft) -> K.AddrOfFun (f, translate_ftyp ft)

      | AddrOf (lv, Array (elt_t, len)) ->
(* TODO: put use of length_of_array in firstpass!!! *)
	  let sz = K.Mult (length_of_array len lv, size_of_typ elt_t) in
	  let lv = translate_lv lv in
	    K.AddrOf (lv, sz)

      | AddrOf (lv, t) ->
	  let lv = translate_lv lv in
	  let sz = Nat.of_int (size_of_typ t) in
	    K.AddrOf (lv, K.Known sz)

      | Unop (K.Not, e) -> K.negate (translate_exp e)

      | Unop (K.Cast (t, t'), e) -> 
	  let e = translate_exp e in
	    K.cast t e t'

      | Unop (op, e) -> 
	  let e = translate_exp e in
	    K.UnOp (op, e)

      | Binop (op, e1, e2) ->
	  let e1 = translate_exp e1 in
	  let e2 = translate_exp e2 in
	    K.BinOp (op, e1, e2)

      | BlkExp _ -> 
	  Npkcontext.report_error "Cir2npkil.translate_exp"
	    "unexpected side-effect in expression"

      | Call _ -> 
	  Npkcontext.report_error "Cir2npkil.translate_exp"
	    "unexpected call in expression"	  
  in

  let rec translate_blk x = 
    match x with
(* TODO: pass id up to npkil!! *)
	(Decl (t, x), loc)::body ->
	  Npkcontext.set_loc loc;
	  let t = translate_typ t in
	  let body = translate_blk body in
	    (K.Decl (x, t, body), loc)::[]

      | hd::tl -> 
	  let hd = translate_stmt hd in
	  let tl = translate_blk tl in
	    hd@tl
      | [] -> []
  
  and translate_set (lv, t, e) =
    let lv = translate_lv lv in
    let e = translate_exp e in
    let t = translate_typ t in
      K.Set (lv, e, t)

  and translate_stmt (x, loc) =
    Npkcontext.set_loc loc;
    match x with
	Block (body, None) -> translate_blk body
      | Block (body, Some lbl) ->
	  let body = translate_blk body in
	    (K.DoWith (body, lbl), loc)::[]

      | Set (lv, _, Call c) ->
	  let call = translate_call (Some lv) c in
	    (call, loc)::[]

      | Set x -> 
	  let set = translate_set x in
	    (set, loc)::[]

      | Goto lbl -> (K.Goto lbl, loc)::[]

      | Guard e -> 
	  let e = translate_exp e in
	    (K.Guard e, loc)::[]
	      
      | Select (body1, body2) -> 
	  let body1 = translate_blk body1 in
	  let body2 = translate_blk body2 in
	    (K.Select (body1, body2), loc)::[]
	  
      | Loop body -> (K.InfLoop (translate_blk body), loc)::[]

      | Switch switch -> translate_switch loc switch

      | Exp (Call c) -> 
	  let call = translate_call None c in
	    (call, loc)::[]

      | UserSpec x -> (translate_assertion loc x)::[]

      | Exp _ -> 
	  Npkcontext.report_error "Cir2npkil.translate_stmt" 
	    "unexpected expression as statement"

      | Decl _ -> 
	  Npkcontext.report_error "Cir2npkil.translate_stmt" "unreachable code"

  and translate_fn fn =
    match fn with
	Fname f -> K.FunId f
      | FunDeref e -> K.FunDeref (translate_exp e)
      
  and translate_args args =
    let in_vars = ref [] in
    let out_vars = ref [] in
    let collect_in_var x =
      match x with
	| In e -> in_vars := (translate_exp e)::!in_vars
	| InOut typ_lv -> in_vars := (translate_exp (Lval typ_lv))::!in_vars
	| Out _ -> ()
    in
    let collect_out_var x =
      match x with
	  Out (lv, _) | InOut (lv, _) -> 
	    out_vars := (translate_lv lv)::!out_vars
	| In _ -> ()
    in
      List.iter collect_in_var args;
      List.iter collect_out_var args;
      (List.rev !in_vars, List.rev !out_vars)

  and translate_call ret ((args_t, ret_t), fn, args) =
    (* TODO: cleanup this is really a bit of a hack *)
    let args =
      match ret with
	  Some lv -> (Out (lv, ret_t))::args
	| None -> args
    in
    let (in_vars, out_vars) = translate_args args in
    let ft = translate_ftyp (args_t, ret_t) in
    let fn = translate_fn fn in
      K.Call (in_vars, ft, fn, out_vars)

  and translate_switch loc (e, cases, default) =
    let e = translate_exp e in
    let default = translate_blk default in
    let rec translate_cases default_guard x =
      match x with
	  ((v, t), body)::tl ->
	    let v = translate_exp v in
	    let cond = K.BinOp (Newspeak.Eq t, e, v) in
	    let body = translate_blk body in
	    let default_guard = (K.Guard (K.negate cond), loc)::default_guard in
	    let body = (K.Guard cond, loc)::body in
	    let choices = translate_cases default_guard tl in
	      (K.Select (body, choices), loc)::[]
	| [] -> 
	    let body = default_guard@default in
	      body
    in
      translate_cases [] cases
  
  and translate_token x =
    match x with
	SymbolToken c -> K.SymbolToken c
      | IdentToken x -> K.IdentToken x
      | LvalToken (lv, t) -> K.LvalToken (translate_lv lv, translate_typ t)
      | CstToken c -> K.CstToken (translate_cst c)

  and translate_assertion loc x = 
    (K.UserSpec (List.map translate_token x), loc) 
  in
	  
  let translate_glbdecl x (t, loc, init) =
    Npkcontext.set_loc loc;
    let t = translate_typ t in 
      Hashtbl.add glbdecls x (t, loc, init, false)
  in

  let translate_fundef f (ret_id, args_id, (args, t), body) =
(* TODO: remove normalize!! *)
    let body = Cir.normalize body in
    let body = translate_blk body in
    let ft = translate_ftyp (args, t) in
      Hashtbl.add fundefs f (ret_id::[], args_id, ft, body)
  in

  let flag_glb x = 
    try  
      let (t, loc, init, _) = Hashtbl.find glbdecls x in
 	Hashtbl.replace glbdecls x (t, loc, init, true)
    with Not_found -> 
      Npkcontext.report_error "Cir2npkil.flag_glb" ("illegal use of " ^ x ^ " (undefined type)" )
      
  in
(* TODO: remove normalize!!! *) 
(* TODO: this phase is stack memory intensive!! not nice!!! *)
(* TODO: find a way to get rid of it!! For instance, put all variables at the
   function scope level!! *)
  let init = Cir.normalize prog.init in
  let init = translate_blk init in
    Hashtbl.iter translate_glbdecl prog.globals;
    Hashtbl.iter translate_fundef prog.fundecs;
    Set.iter flag_glb !used_glbs;
    { K.fnames = fnames; K.globals = glbdecls; K.init = init;
      K.fundecs = fundefs; K.src_lang = src_lang }
