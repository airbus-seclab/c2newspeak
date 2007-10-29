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

(* TODO: code cleanup: define an Env and primitives on it, and get it 
   out of this *)
(* TODO: put Env in common with cilcompiler, pass Env as an argument to 
   translation (or as a global local to translation) *)
open Lexing
open Csyntax

module N = Newspeak
module K = Npkil
(* TODO: code cleanup: break this file in several module. 
   do factorisation, when possible. In particular K.size_of  and align *)

(* TODO: check that integer don't have a default type (like int) *)
let kind_of_int64 i =
  let sign =
    if Int64.compare i (Int64.of_string "2147483647") > 0 
    then N.Unsigned else N.Signed
  in
    (sign, Config.size_of_int)

let promote k = 
  match k with
      (_, n) when n < Config.size_of_int -> (N.Signed, Config.size_of_int)
    | _ -> k

let translate_unop op (e, t) =
  match op with
      Not -> (K.UnOp (K.Not, e), Int (N.Signed, Config.size_of_int))

let translate_arithmop op =
  match op with
      Plus -> N.PlusI
    | Mult -> N.MultI
    | _ -> 
	Npkcontext.error "Compiler.translate_arithmop" 
	  "Unexpected arithmetic operator"

let translate_array lv a =
  let (t, n) = array_of_typ a in
  let n =
    match (n, lv) with
	(Some n, _) -> K.Known n
      | (_, K.Global x) -> K.Length x
      | _ -> Npkcontext.error "Compiler.translate_array" "Unknown array size"
  in
    (t, n)

let translate fname (_, glbdecls, fundefs) = 
  let env = Env.create () in

  let push loc (t, x) = Env.push env x t loc in
  let pop = Env.pop env in
  let get_var = Env.get_var env in
  let get_ret_typ () = Env.get_ret_typ env in

  let push_dummy loc = push loc (Void, "dummy") in
  let pop_dummy () = pop "dummy" in

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

  let update_fundef = Env.update_funbody env in

  let update_ftyp f ftyp = Env.update_ftyp env f (translate_ftyp ftyp) in

  let translate_binop op (e1, t1) (e2, t2) =
    match (op, t1, t2) with
	((Mult|Plus), Int k1, Int k2) -> 
	  let k1 = promote k1 in
	  let k2 = promote k2 in
	  let k = N.max_ikind k1 k2 in
	  let e1 = K.make_int_coerce k e1 in
	  let e2 = K.make_int_coerce k e2 in
	  let op = translate_arithmop op in
	    (K.make_int_coerce k (K.BinOp (op, e1, e2)), Int k)
	      
      | (Plus, Ptr t, Int _) ->	
	  let stride = K.exp_of_int (size_of t) in 
	    (K.BinOp (N.PlusPI, e1, K.BinOp (N.MultI, e2, stride)), t1)
	      (* TODO: clean bug ? maybe a cast is necessary ? *)
      | (Gt, _, _) -> 
	  let t = Int (N.Signed, Config.size_of_int) in
	    (K.BinOp (N.Gt (translate_scalar t1), e1, e2), t)
	      
      | (Eq, t1, t2) when t1 = t2 -> 
	  let t = translate_scalar t1 in
	    (K.BinOp (N.Eq t, e1, e2), Int (N.Signed, Config.size_of_int))
	      
      | _ -> 
	  Npkcontext.error "Compiler.translate_binop" 
	    "unexpected binary operator and arguments"
  in

  let build_args f args loc =
    let formals =
      try 
	let ((args, _), _, _) = Hashtbl.find fundefs f in
	  args
      with Not_found -> 
	let i = ref (-1) in
	let build (_, t) =
	  incr i;
	  (t, "arg"^(string_of_int !i))
	in
	  List.map build args
    in
    let decls = List.map (fun (t, x) -> (t, x, loc)) formals in
    let rec build_set args =
      match args with
	  (e, t)::tl -> 
	    let (i, tl) = build_set tl in
	    let lv = K.Local i in
	      (i+1, (K.Set (lv, e, translate_scalar t), loc)::tl)
	| [] -> (0, [])
    in
    let (_, set) = build_set args in
      (decls, set)
  in

  let rec translate_lv lv =
    match lv with
	Var x -> get_var x

      | Field (lv, f) ->
	  let (lv, t) = translate_lv lv in
	  let r = fields_of_typ t in
	  let (o, t) = List.assoc f r in
	  let o = K.Const (N.CInt64 (Int64.of_int o)) in
	    (K.Shift (lv, o), t)

      | Index (lv, e) -> 
	  let (lv, t) = translate_lv lv in
	  let (t, n) = translate_array lv t in
	  let (i, _) = translate_exp e in
	  let sz = K.exp_of_int (size_of t) in
	  let o = K.UnOp (K.Belongs_tmp (Int64.zero, K.Decr n), i) in
	  let o = K.BinOp (N.MultI, o, sz) in
	    (K.Shift (lv, o), t)

      | Deref e ->
	  let (e, t) = translate_exp e in
	  let t = deref_typ t in
	    (K.Deref (e, size_of t), t)

  and translate_bexp e =
    let (e, t) = translate_exp e in
    let t = translate_scalar t in
      match (e, t) with
	  (K.Lval _, N.Int _) -> K.negate (K.BinOp (N.Eq t, e, K.zero))
	| _ -> e

  and translate_exp e =
    match e with
	Const i -> (K.Const (N.CInt64 i), Int (kind_of_int64 i))

      | Lval lv -> 
	  let (lv, t) = translate_lv lv in
	    (K.Lval (lv, translate_scalar t), t)

      | AddrOf Index (lv, e) ->
	  let (lv, t) = translate_lv lv in
	  let (i, _) = translate_exp e in
	  let (t', n) = translate_array lv t in
	  let sz = size_of t' in
	  let sz_e = K.Const (N.CInt64 (Int64.of_int sz)) in
	  let o = K.UnOp (K.Belongs_tmp (Int64.zero, n), i) in
	  let o = K.BinOp (N.MultI, o, sz_e) in
	  let n = K.Mult (n, sz) in
	    (K.BinOp (N.PlusPI, K.AddrOf (lv, n), o), Ptr t)

      | AddrOf lv ->
	  let (lv, t) = translate_lv lv in
	  let sz = size_of t in
	    (K.AddrOf (lv, K.Known sz), Ptr t)

      | Unop (op, e) -> 
	  let v = translate_exp e in
	    translate_unop op v

      | Binop (op, e1, e2) ->
	  let v1 = translate_exp e1 in
	  let v2 = translate_exp e2 in
	    translate_binop op v1 v2

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

  and translate_set loc (lv, e) =
    let (lv, t) = translate_lv lv in
    let (e, t') = translate_exp e in
    let t = translate_scalar t in
    let t' = translate_scalar t' in
    let e = K.cast t' e t in
      (K.Set (lv, e, t), loc)
    

  and translate_stmt (x, loc) =
    Npkcontext.set_loc loc;
    match x with
      | Decl (d, body) -> 
	  push loc d;
	  translate_blk body

      | Set (lv, Call x) -> 
	  translate_call loc (Some lv) x

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
	  let lbl = Env.get_brk_lbl () in
	  let brk = (K.Goto lbl, loc)::[] in
	  let cond = K.ChooseAssert [([cond1], []); ([cond2], brk)] in
	  let loop = (K.InfLoop ((cond, loc)::body), loc)::[] in
	    (K.DoWith (loop, lbl, []), loc)::[]

      | DoWhile (body, e) ->
	  let cond1 = translate_bexp e in
	  let cond2 = K.negate cond1 in
	  let body = translate_blk body in
	  let lbl = Env.get_brk_lbl () in
	  let brk = (K.Goto lbl, loc)::[] in
	  let cond = K.ChooseAssert [([cond1], []); ([cond2], brk)] in
	  let loop = (K.InfLoop (body@[cond, loc]), loc)::[] in
	    (K.DoWith (loop, lbl, []), loc)::[]

      | Return (Call x) -> 
	  let t = get_ret_typ () in
	  let () = push loc (t, "tmp") in
	  let (lv, t) = translate_lv (Var (Env.get_ret_name ())) in
	  let decl = (t, "tmp", Newspeak.dummy_loc fname)::[] in
	  let call = translate_call loc (Some (Var "tmp")) x in
	  let t = translate_scalar t in
	  let set = (K.Set (lv, K.Lval (K.Local 0, t), t), loc)::[] in
	    pop "tmp";
	    append_decls decl (call@set)

      | Return e -> 
	  let set = translate_set loc (Var (Env.get_ret_name ()), e) in
	  let lbl = Env.get_ret_lbl () in
	    set::(K.Goto lbl, loc)::[]

      | Exp (Call x) -> translate_call loc None x
      
      | Break -> [K.Goto (Env.get_brk_lbl ()), loc]
      
      | Switch (e, cases) -> translate_switch loc e cases

      | Exp _ -> 
	  Npkcontext.error "Compiler.translate_stmt" 
	    "Expressions as statements not implemented yet"

  and translate_call loc r (f, args) =
    (* TODO: code robustness: should compare arguments types and 
       expected function type *)
    
    (* push_dummy is a bit of a hack, since we do not have the type of 
       the return value or arguments, but still need to compile expressions
       with the right number of local variable,
       we push a dummy variable in the local env.
    *)
    push_dummy loc;
    let (ret_t, ret_decl, ret_set) = 
      match r with
	  None -> (Void, [], [])
	| Some lv -> 
	    let (r, t) = translate_lv lv in
	      (* TODO: code factorisation: many occurences of this: *)
	    let ret_t = translate_scalar t in
	      (t,
	      (t, "value_of_"^f, loc)::[], 
	      (K.Set (r, K.Lval (K.Local 0, ret_t), ret_t), loc)::[])
    in
      (* TODO: code cleanup put this together with the first push_dummy *)
    List.iter (fun _ -> push_dummy loc) args;

    let args = (List.map translate_exp args) in
    let args_t = List.map snd args in
    let (decls, set) = build_args f args loc in
      pop_dummy ();
      List.iter (fun _ -> pop_dummy ()) args;
      update_ftyp f (args_t, ret_t);

    let call = (K.Call (K.FunId f), loc)::[] in
    let call = set@call in
    let call = append_decls decls call in
      (* TODO: code optimization: write this so that there is no @ 
	 (by putting ret_set under the scope of local variables too) *)
    let call = call@ret_set in
    let call = append_decls ret_decl call in
      call

  and translate_switch loc e cases =
    let (switch_exp, _) = translate_exp e in
    let default_lbl = Env.get_brk_lbl () in
    let default_cond = ref [] in
    let default_goto = ref [K.Goto default_lbl, loc] in

    let lbl_cnt = ref 2 in
    let choices = ref [] in

    let found_case lbl v =
      match v with
	  Some v ->
	    let (v, t) = translate_exp v in
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
	  let translate (o, t, e) = 
	    (* TODO: should I take t, or translate the one I get from e ??? *)
	    let (e, _) = translate_exp e in
	      (o, translate_scalar t, e) 
	  in
	  let init = List.map translate init in
	    Some (Some init)
  in

  let translate_glbdecl x (t, loc, init) =
    match (t, init) with
	(Fun (args, t), (None | Some None)) ->
	  let (args_t, _) = List.split args in
	    update_ftyp x (args_t, t)
	      
      | (Fun _, _) ->
	  Npkcontext.error "Compiler.translate_glbdecl" 
	    "Unexpected initialization"
      | _ ->
	  let init = translate_init init in
	    (* TODO: maybe do this in a first, since there may be the need for
	       a variable not encountered yet, in init *)
	    Env.add_global env x t (translate_typ t, loc, init, true)
  in

  let translate_fundef f ((args, t), loc, body) =
    let (args_t, args_name) = List.split args in
      update_ftyp f (args_t, t);
      push loc (t, Env.get_ret_name ());
      List.iter (push loc) args;
      let body = translate_blk body in
	List.iter pop args_name;
	pop (Env.get_ret_name ());
	let body = (K.DoWith (body, Env.get_ret_lbl (), []), loc)::[] in
	let decls = Env.get_locals env in
	let body = append_decls decls body in
	  update_fundef f body
  in

    Hashtbl.iter translate_glbdecl glbdecls;
    Hashtbl.iter translate_fundef fundefs;

    Env.extract_prog env fname


let parse fname =
  let cin = open_in fname in
  let lexbuf = Lexing.from_channel cin in
    Lexer.init fname lexbuf;
    try
      let cprog = Parser.cprog Lexer.token lexbuf in
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
  let prog = Firstpass.translate prog in
    translate fname prog
  
