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

open Lexing
open Csyntax

(* TODO: code cleanup: break this file in several module. 
   do factorisation (with Cilcompiler too), when possible. 
   In particular K.size_of  and align *)
module N = Newspeak
module K = Npkil

let ret_lbl = 0
let brk_lbl = 1

(* TODO: check that integer don't have a default type (like int) *)
let kind_of_int64 i =
  let sign =
    if Int64.compare i (Int64.of_string "2147483647") > 0 
    then N.Unsigned else N.Signed
  in
    (sign, Config.size_of_int)

let translate_unop op e =
  match op with
      Not -> K.UnOp (K.Not, e)

let translate_array lv (t, n) =
  let n =
    match (n, lv) with
	(Some n, _) -> K.Known n
      | (_, K.Global x) -> K.Length x
      | _ -> Npkcontext.error "Compiler.translate_array" "Unknown array size"
  in
    (t, n)

let translate fname (_, cglbdecls, cfundefs) = 
  let glbdecls = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in
  let stack_height = ref 0 in

  let push () = incr stack_height in
  let pop () = decr stack_height in

  let rec translate_typ t =
    match t with
	Void -> 
	  Npkcontext.error "Compiler.translate_typ" "void not allowed here"
      | Int _ | Float _ | Ptr _ -> K.Scalar (translate_scalar t)
      | Array (t, sz) -> K.Array (translate_typ t, sz)
      | StructOrUnion (_, fields, n) -> 
	  let translate_field (_, (o, t)) = (o, translate_typ t) in
	  K.Region (List.map translate_field fields, n)
      | Fun _ -> 
	  Npkcontext.error "Compiler.translate_typ" "function not allowed here"

  and translate_scalar t =
    match t with
      | Int i -> N.Int i
      | Float n -> N.Float n
      | Ptr (Fun _) -> N.FunPtr
      | Ptr _ -> N.Ptr
      | _ -> 
	  Npkcontext.error "Compiler.translate_scalar" 
	    "Unexpected non scalar type"
  in

  let translate_ftyp (args, ret) =
    let args = List.map translate_typ args in
    let ret =
      match ret with
	  Void -> None
	| _ -> Some (translate_typ ret)
    in
      (args, ret)
  in

  let translate_binop op e1 e2 =
    let translate_arithmop op k = 
      let e1 = K.make_int_coerce k e1 in
      let e2 = K.make_int_coerce k e2 in
	K.make_int_coerce k (K.BinOp (op, e1, e2))
    in

      match op with
	  Mult k -> translate_arithmop N.MultI k
	| Plus k -> translate_arithmop N.PlusI k
	| Minus k -> translate_arithmop N.MinusI k
	| PlusP t ->	
	    let stride = K.exp_of_int (size_of t) in 
	      K.BinOp (N.PlusPI, e1, K.BinOp (N.MultI, e2, stride))
	
	(* TODO: clean bug ? maybe a cast is necessary ? *)
	| Gt t -> 
	    let t = translate_scalar t in
	      K.BinOp (N.Gt t, e1, e2)
	   
	| Eq t -> 
	    let t = translate_scalar t in
	      K.BinOp (N.Eq t, e1, e2)
  in

  let rec translate_lv lv =
    match lv with
(* TODO: code cleanup *)
	Local x -> K.Local (!stack_height - x)

      | Global x -> K.Global x

      | Field (lv, f, o) ->
	  let lv = translate_lv lv in
	  let o = K.exp_of_int o in
	    K.Shift (lv, o)

      | Index (lv, a, e) -> 
	  let lv = translate_lv lv in
	  let (t, n) = translate_array lv a in
	  let i = translate_exp e in
	  let sz = K.exp_of_int (size_of t) in
	  let o = K.UnOp (K.Belongs_tmp (Int64.zero, K.Decr n), i) in
	  let o = K.BinOp (N.MultI, o, sz) in
	    K.Shift (lv, o)

      | Deref (e, sz) ->
	  let e = translate_exp e in
	    K.Deref (e, sz)

  and translate_bexp (e, t) =
    let e = translate_exp e in
    let t = translate_scalar t in
      match (e, t) with
	  (K.Lval _, N.Int _) -> K.negate (K.BinOp (N.Eq t, e, K.zero))
	| _ -> e

  and translate_exp e =
    match e with
	Const i -> K.Const (N.CInt64 i)

      | Lval (lv, t) -> 
	  let lv = translate_lv lv in
	    K.Lval (lv, translate_scalar t)

      | AddrOf (Index (lv, a, e), _) ->
	  let lv = translate_lv lv in
	  let i = translate_exp e in
	  let (t, n) = translate_array lv a in
	  let sz = size_of t in
	  let sz_e = K.exp_of_int sz in
	  let o = K.UnOp (K.Belongs_tmp (Int64.zero, n), i) in
	  let o = K.BinOp (N.MultI, o, sz_e) in
	  let n = K.Mult (n, sz) in
	    K.BinOp (N.PlusPI, K.AddrOf (lv, n), o)

      | AddrOf (lv, t) ->
	  let lv = translate_lv lv in
	  let sz = size_of t in
	    K.AddrOf (lv, K.Known sz)

      | Unop (op, e) -> 
	  let e = translate_exp e in
	    translate_unop op e

      | Binop (op, e1, e2) ->
	  let e1 = translate_exp e1 in
	  let e2 = translate_exp e2 in
	    translate_binop op e1 e2

      | Call _ -> 
	  Npkcontext.error "Compiler.translate_exp" 
	    "Call inside expression not implemented yet"
  in

  let rec append_decls d body =
    match d with
	(t, x, loc)::tl -> 
	  let t = translate_typ t in
	    (K.Decl (x, t, append_decls tl body), loc)::[]
      | [] -> body
  in

  let rec translate_blk x =
    match x with
	hd::tl -> (translate_stmt hd)@(translate_blk tl)
      | [] -> []

  and translate_set loc ((lv, t), (e, t')) =
    let lv = translate_lv lv in
    let t = translate_scalar t in
    let e = translate_exp e in
    let t' = translate_scalar t' in
    let e = K.cast t' e t in
      (K.Set (lv, e, t), loc)
    
  and translate_local_init loc x (offset, t', (e, t)) =
    let lv = translate_lv (Local x) in
    let lv = K.Shift (lv, K.exp_of_int offset) in
    let e = translate_exp e in
    let t = translate_scalar t in
    let t' = translate_scalar t' in
    let e = K.cast t e t' in
      (K.Set (lv, e, t'), loc)

  and translate_stmt (x, loc) =
    Npkcontext.set_loc loc;
    match x with
      | Init (x, init) -> List.map (translate_local_init loc x) init

      | Set (lv, (Call x, _)) -> translate_call loc (Some lv) x

      | Set (lv, e) -> (translate_set loc (lv, e))::[]

      | If ((e, body, loc)::tl) ->
	  let cond1 = translate_bexp e in
	  let body = translate_blk body in
	  let else_body = translate_stmt (If tl, loc) in begin
	    match cond1 with
		K.Const N.CInt64 i when Int64.compare i Int64.zero <> 0 -> body
	      | K.Const N.CInt64 _ -> else_body
	      | _ -> 
		  let cond2 = K.negate cond1 in
		    (K.ChooseAssert [([cond1], body); 
				     ([cond2], else_body)], loc)::[]
	  end

      | If [] -> []

      | While (e, body) ->
	  let cond1 = translate_bexp e in
	  let cond2 = K.negate cond1 in
	  let body = translate_blk body in
	  let brk = (K.Goto brk_lbl, loc)::[] in
	  let cond = K.ChooseAssert [([cond1], []); ([cond2], brk)] in
	  let loop = (K.InfLoop ((cond, loc)::body), loc)::[] in
	    (K.DoWith (loop, brk_lbl, []), loc)::[]

      | DoWhile (body, e) ->
	  let cond1 = translate_bexp e in
	  let cond2 = K.negate cond1 in
	  let body = translate_blk body in
	  let brk = (K.Goto brk_lbl, loc)::[] in
	  let cond = K.ChooseAssert [([cond1], []); ([cond2], brk)] in
	  let loop = (K.InfLoop (body@[cond, loc]), loc)::[] in
	    (K.DoWith (loop, brk_lbl, []), loc)::[]

      | Return -> (K.Goto ret_lbl, loc)::[]

      | Exp (Call x) -> translate_call loc None x
      
      | Break -> (K.Goto brk_lbl, loc)::[]
      
      | Switch (e, cases) -> translate_switch loc e cases

      | Exp _ -> 
	  Npkcontext.error "Compiler.translate_stmt" 
	    "Expressions as statements not implemented yet"

  and translate_call loc r ((f, (args_t, ret_t)), args_exp) =
    let ret_name = "value_of_"^f in
    let (ret_decl, ret_set) = 
      match (r, ret_t) with
	  (None, Void) -> ([], [])
	| (None, _) ->
	    push ();
	    let ret_decl = (ret_t, ret_name, loc) in
	      (ret_decl::[], [])
	| (Some _, Void) -> 
(* TODO: code cleanup: change error message *)
	    Npkcontext.error "Compiler.translate_call" "No return value"
	| (Some lv, _) -> 
	    push ();
	    let ret_decl = (ret_t, ret_name, loc) in
(* TODO: code cleanup, merge with push ?? *)
	    let ret_set = translate_set loc 
	      (lv, (Lval (Local !stack_height, ret_t), ret_t)) 
	    in
	      (ret_decl::[], ret_set::[])
    in
      List.iter (fun _ -> push ()) args_t;

(* TODO: code cleanup: change error message *)
      if (List.length args_t <> List.length args_exp) then begin
	Npkcontext.error "Compiler.translate_call" 
	  ("Different types for function "^f)
      end;

(* TODO: code cleanup: merge with push *)
      let (decls, sets) = translate_args loc args_t args_exp in
	pop ();
	List.iter (fun _ -> pop ()) args_t;
	
	let call = (K.Call (K.FunId f), loc)::[] in
	let call = sets@call in
	let call = append_decls decls call in
	  (* TODO: code optimization: write this so that there is no @ 
	     (by putting ret_set under the scope of local variables too) *)
	let call = call@ret_set in
	let call = append_decls ret_decl call in
	  call

(* TODO: code cleanup *)
  and translate_args loc args_t args_e =
    let rec translate args_t args_e =
    match (args_t, args_e) with
	((t, x)::args_t, e::args_e) ->
	  let decl = (t, x, loc) in
	  let (decls, sets, n) = translate args_t args_e in
	  let set = translate_set loc ((Local (!stack_height - n), t), e) in
	    (decl::decls, set::sets, n+1)

      | ([], []) -> ([], [], 0)
      | _ -> invalid_arg "Unreachable statement"
    in
    let (decls, sets, _) = translate args_t args_e in
      (decls, sets)

  and translate_switch loc e cases =
    let switch_exp = translate_exp e in
    let default_lbl = brk_lbl in
    let default_cond = ref [] in
    let default_goto = ref [K.Goto default_lbl, loc] in

    let lbl_cnt = ref 2 in
    let choices = ref [] in

    let found_case lbl v =
      match v with
	  Some (v, t) ->
	    let v = translate_exp v in
	    let t = translate_scalar t in
	    let cond = K.BinOp (Newspeak.Eq t, switch_exp, v) in
	      default_cond := (K.negate cond)::!default_cond;
	      choices := (cond::[], [K.Goto lbl, loc])::!choices
	| None -> default_goto := [K.Goto lbl, loc]
    in
    let rec translate_cases x =
      match x with
	  (v, [], case_loc)::(v', body, _)::tl ->
	    let (lbl, cases) = translate_cases ((v', body, case_loc)::tl) in
	    found_case (lbl - 1) v;
	      (lbl, cases)
	| (v, body, case_loc)::tl ->
	    let (lbl, tl) = translate_cases tl in
	    let body = translate_blk body in
	      found_case lbl v;
	      (lbl + 1, (lbl, body, case_loc)::tl)
	| [] -> (2, [])
    in

    let (_, cases) = translate_cases cases in
    let default_choice = (!default_cond, !default_goto) in
    let switch = [K.ChooseAssert (default_choice::!choices), loc] in

    let rec append_cases x =
      match x with
	  (lbl, body, loc)::tl -> 
	    let tl = append_cases tl in
	      (K.DoWith (tl, lbl, []), loc)::body
	| [] -> switch
    in
      (* TODO: optimize this, do not rev cases, maybe have it as a reference *)
    let switch = append_cases (List.rev cases) in
      [K.DoWith (switch, default_lbl, []), loc]
	
  in

  let translate_init x =
    match x with
	None -> None 
      | Some None -> Some None
      | Some Some init ->
	  (* TODO: code factorisation with the local init ??? *)
	  let translate (o, t', (e, t)) = 
	    (* TODO: should I take t, or translate the one I get from e ??? *)
	    let e = translate_exp e in
	    let t = translate_scalar t in
	    let t' = translate_scalar t' in
	    let e = K.cast t e t' in
	      (o, t', e)
	  in
	  let init = List.map translate init in
	    Some (Some init)
  in

  let translate_glbdecl x (t, loc, init) =
    let init = translate_init init in
    let t = translate_typ t in
      (* TODO: maybe do this in a first, since there may be the need for
	 a variable not encountered yet, in init *)
      Hashtbl.add glbdecls x (t, loc, init, true)
  in

  let translate_fundef f ((args, t), loc, body) =
    let (args_t, args_name) = List.split args in
    let body =
      match body with
	  None -> None
	| Some (locals, body) ->
	    push ();
	    List.iter (fun _ -> push ()) args;
(* TODO: change arguments organisation of push arguments to avoid currification *)
	    List.iter (fun _ -> push ()) locals;
	    let body = translate_blk body in
	      List.iter (fun _ -> pop ()) args_name;
	      pop ();
	      let body = (K.DoWith (body, ret_lbl, []), loc)::[] in
	      let body = append_decls locals body in
		List.iter (fun _ -> pop ()) locals;
		Some body
    in
    let ft = translate_ftyp (args_t, t) in
      Hashtbl.add fundefs f (ft, body)
  in

    Hashtbl.iter translate_glbdecl cglbdecls;
    Hashtbl.iter translate_fundef cfundefs;

    (fname, glbdecls, fundefs)


let parse fname =
  let cin = open_in fname in
  let lexbuf = Lexing.from_channel cin in
    Lexer.init fname lexbuf;
    try
      let cprog = Parser.translation_unit Lexer.token lexbuf in
	close_in cin;
	cprog
    with Parsing.Parse_error -> 
      let pos = Lexing.lexeme_start_p lexbuf in
      let line_nb = string_of_int pos.pos_lnum in
      let lexeme = Lexing.lexeme lexbuf in
	Npkcontext.error "Parser.parse_error" 
	  ("syntax error: line "^line_nb^", unexpected token: "^lexeme)


let compile fname =
  let prog = parse fname in
  let prog = Firstpass.translate fname prog in
    translate fname prog
  
