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

let translate (cglbdecls, cfundefs, specs) fnames =
  let glbdecls = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in

  let used_glbs = ref Set.empty in
  let stack_height = ref 0 in
  let env = Hashtbl.create 50 in
  let push id = 
    incr stack_height;
    Hashtbl.add env id !stack_height
  in
  let pop id = 
    decr stack_height;
    Hashtbl.remove env id
  in

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
	Var id -> 
	  let x = Hashtbl.find env id in
	    K.Local (!stack_height - x)

      | Global x -> 
	  used_glbs := Set.add x !used_glbs;
	  K.Global x

      | Shift (lv, o) ->
	  let lv = translate_lv lv in
	  let o = translate_exp o in
	    K.Shift (lv, o)

      | Deref (e, t) ->
	  let e = translate_exp e in
	  let sz = size_of t in
	    K.Deref (e, sz)

      | BlkLv _ ->
	  Npkcontext.report_error "Compiler.translate_lval"
	    "unexpected side-effect in left value"

  and translate_exp e =
    match e with
	Const i -> K.Const (translate_cst i)
      | Lval (lv, t) -> 
	  let lv = translate_lv lv in
	    K.Lval (lv, translate_scalar t)

      | AddrOfFun (f, ft) -> K.AddrOfFun (f, translate_ftyp ft)

      | AddrOf (lv, Array (elt_t, len)) ->
(* TODO: put use of length_of_array in firstpass!!! *)
	  let sz = K.Mult (length_of_array len lv, size_of elt_t) in
	  let lv = translate_lv lv in
	    K.AddrOf (lv, sz)

      | AddrOf (lv, t) ->
	  let lv = translate_lv lv in
	  let sz = Nat.of_int (size_of t) in
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

      | Pref _ -> 
	  Npkcontext.report_error "Compiler.translate_exp"
	    "unexpected side-effect in expression"

      | Call _ -> 
	  Npkcontext.report_error "Compiler.translate_exp"
	    "unexpected call in expression"	  
  in

  let rec translate_blk x = 
    match x with
	(Decl (t, x, id), loc)::body ->
	  Npkcontext.set_loc loc;
	  let t = translate_typ t in
	    push id;
	    let body = translate_blk body in
	      pop id;
	      (K.Decl (x, t, body), loc)::[]

      | hd::tl -> 
	  let hd = translate_stmt hd in
	  let tl = translate_blk tl in
	    hd@tl
      | [] -> []
  
  and translate_set (lv, t, e) =
    let lv = translate_lv lv in
      match (t, e) with
	  ((Struct _| Union _), Lval (lv', _)) -> 
	    let lv' = translate_lv lv' in
	    let sz = size_of t in
	      K.Copy (lv, lv', sz)
      | _ ->
	  let e = translate_exp e in
	  let t = translate_scalar t in
	    K.Set (lv, e, t)

  and translate_stmt (x, loc) =
    Npkcontext.set_loc loc;
    match x with
	Block (body, None) -> translate_blk body
      | Block (body, Some (lbl, action)) ->
	  let body = translate_blk body in
	  let action = translate_blk action in
	    (K.DoWith (body, lbl, action), loc)::[]

      | Set (lv, _, Call c) ->
	  let call = translate_call loc (Some lv) c in
	    call::[]

      | Set x -> 
	  let set = translate_set x in
	    (set, loc)::[]

      | Goto lbl -> (K.Goto lbl, loc)::[]

      | If (e, body1, body2) ->
	  let cond1 = translate_exp e in
	  let body1 = translate_blk body1 in
	  let body2 = translate_blk body2 in begin
	    match cond1 with
(* TODO: remove this code and have it as a simplification for newspeak rather 
*) 
		(* TODO: isn't this redundant with firstpass?? *)
		K.Const N.CInt i when Nat.compare i Nat.zero <> 0 -> 
		  body1
	      | K.Const N.CInt _ -> body2
	      | _ -> 
		  let cond2 = K.negate cond1 in
		  let body1 = (K.Guard cond1, loc)::body1 in
		  let body2 = (K.Guard cond2, loc)::body2 in
		    (K.Select (body1, body2), loc)::[]
	  end

      | Loop body -> (K.InfLoop (translate_blk body), loc)::[]

      | Switch switch -> translate_switch loc switch

      | Exp (Call c) -> 
	  let call = translate_call loc None c in
	    call::[]

      | UserSpec x -> (K.UserSpec (translate_assertion x), loc)::[]

      | Exp _ -> 
	  Npkcontext.report_error "Compiler.translate_stmt" 
	    "unexpected expression as statement"

      | Decl _ -> 
	  Npkcontext.report_error "Compiler.translate_stmt" "unreachable code"

  and append_args loc args fid f =
    let rec append x args =
      match args with
	  (e::args, t::args_t) ->
	    let id = fresh_id () in
	      push id;
	      let lv = Var id in
	      let set = translate_set (lv, t, e) in
	      let t = translate_typ t in
	      let call = append (x+1) (args, args_t) in
	      let arg = fid^".arg"^(string_of_int x) in
		pop id;
		(K.Decl (arg, t, (set, loc)::call::[]), loc)
		  
	| _ -> 
	    let fn = translate_fn f in
	      (K.Call fn, loc)
    in
      append 1 args
      
  and translate_fn fn =
    match fn with
	Fname f -> K.FunId f
      | FunDeref (e, ft) -> 
	  let e = translate_exp e in
	  let ft = translate_ftyp ft in
	    K.FunDeref (e, ft)

  and translate_call loc ret ((args_t, ret_t), f, args) =
    let fid =
      match f with
	  Fname f -> f
	| _ -> "fptr_call"
    in
    let args = (args, args_t) in
      match (ret_t, ret) with
	  (Void, _) -> append_args loc args fid f
	| (_, Some (Var id)) when Hashtbl.find env id = !stack_height ->
	    append_args loc args fid f	    
	| _ ->
	    let t = translate_typ ret_t in
	    let id = fresh_id () in
	      push id;
	      let post = 
		match ret with
		    Some lv -> 
		      let e = Lval (Var id, ret_t) in
		      let set = translate_set (lv, ret_t, e) in
			(set, loc)::[]
		  | None -> []
	      in
	      let call = append_args loc args fid f in
		pop id;
		(K.Decl ("value_of_"^fid, t, call::post), loc)
		
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
      | LvalToken lv -> K.LvalToken (translate_lv lv)
      | CstToken c -> K.CstToken (translate_cst c)

  and translate_assertion x = List.map translate_token x in
	  
  let translate_init (o, t, e) =
    let e = translate_exp e in
      (o, t, e)
  in

  let translate_glb_init x =
        match x with
	None -> None 
      | Some None -> Some None
      | Some Some init ->
	  let init = List.map translate_init init in
	    Some (Some init)
  in

  let translate_glbdecl x (t, loc, init) =
    Npkcontext.set_loc loc;
    let init = translate_glb_init init in
    let t = translate_typ t in
      Hashtbl.add glbdecls x (t, loc, init, false)
  in

  let translate_fundef f ((args, t), _, ((ret_id, args_id), body)) =
    (* push return value *)
    push ret_id;
    (* push arguments *)
    List.iter push args_id;
    let body = Cir.normalize body in
    let body = translate_blk body in
      List.iter pop args_id;
      pop ret_id;
      let ft = translate_ftyp (args, t) in
	Hashtbl.add fundefs f (ft, body)
  in

  let flag_glb x =
    let (t, loc, init, _) = Hashtbl.find glbdecls x in
      Hashtbl.replace glbdecls x (t, loc, init, true)
  in

    Hashtbl.iter translate_glbdecl cglbdecls;
    Hashtbl.iter translate_fundef cfundefs;
    Set.iter flag_glb !used_glbs;
    let specs = List.map translate_assertion specs in
      (fnames, glbdecls, fundefs, specs)
