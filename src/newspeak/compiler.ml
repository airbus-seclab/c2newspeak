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
(* TODO: code optimization: get rid of env, and use the globals Hashtbl
   rename it to env ! *)
open Lexing
open Csyntax

module N = Newspeak
module K = Npkil
(* TODO: code cleanup: break this file in several module. 
   do factorisation, when possible. In particular K.size_of  and align *)

(* TODO: code cleanup: think about this!! *)
(* TODO: check that integer don't have a default type (like int) *)
(* TODO: put Env in common with cilcompiler, pass Env as an argument to 
   translation (or as a global local to translation) *)
let kind_of_int64 i =
  let sign =
    if Int64.compare i (Int64.of_string "2147483647") > 0 
    then N.Unsigned else N.Signed
  in
    (sign, Config.size_of_int)

let ret_vname = "!return"

(*
let size_of_cscalar t = 
  match t with
      CInt (_, n) -> n
    | CFloat n -> n
    | CPtr _ -> Config.size_of_ptr
    | CFunPtr -> Config.size_of_ptr
*)
(* TODO: remove any function that is dead code
let rec size_of t =
  match t with
      CScalar t -> size_of_cscalar t
    | CArray (t, Some n) -> (size_of t) * n
    | CRegion (_, n) -> n
    | CArray _ -> Npkcontext.error "Compiler.size_of" "unknown size of array"
    | CVoid -> Npkcontext.error "Compiler.size_of" "size of void unknown"
    | CFun _ -> Npkcontext.error "Compiler.size_of" "size of function"
*)

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

(* TODO: code cleanup: remove this: remove any use of this *)
let dummy_loc = ("", -1, -1)

let get_ret_lbl () = 0
let get_brk_lbl () = 1

(* TODO: code cleanup: this could be in npkil and also used by cilcompiler ? *)
let cast t e t' =
  match (t, t') with
      _ when t = t' -> e
    | (N.Int _, N.Int k) -> K.make_int_coerce k e
    | (N.Int _, N.Ptr) when e = K.Const (N.CInt64 Int64.zero) -> K.Const N.Nil

    | (N.Ptr, N.Int k) when !Npkcontext.castor_allowed -> 
	Npkcontext.print_warning "Compiler.cast"
	  ("Probable invalid cast "^(K.string_of_cast t t'));
	K.UnOp (K.PtrToInt k, e)
    | _ -> 
	Npkcontext.error "Compiler.cast"
	  ("Invalid cast "^(K.string_of_cast t t'))

let translate_unop op (e, t) =
  match op with
      Not -> (K.UnOp (K.Not, e), Int (N.Signed, Config.size_of_int))

let promote k = 
  match k with
      (_, n) when n < Config.size_of_int -> (N.Signed, Config.size_of_int)
    | _ -> k

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

let compile fname = 
  let globals = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in
  let global_env = Hashtbl.create 100 in
  let env = Hashtbl.create 100 in
    (* maps each function name to its list of formal declarations *)
    (* TODO: remove this: not necessary anymore !!! *)
  let formals = Hashtbl.create 100 in
  let vcnt = ref 0 in
  let push loc (t, x) =
    incr vcnt;
    Hashtbl.add env x (!vcnt, t, loc)
  in
  let pop x =
    vcnt := !vcnt - 1;
    Hashtbl.remove env x
  in

  let push_dummy loc = push loc (Void, "dummy") in

  let pop_dummy () = pop "dummy" in

  let get_var x =
    try 
      let (n, t, _) = Hashtbl.find env x in
	(K.Local (!vcnt - n), t)
    with Not_found -> 
      try
	let t = Hashtbl.find global_env x in
	  (K.Global x, t)
      with Not_found ->
	Npkcontext.error "Compiler.get_var" ("Variable "^x^" not declared")
  in

  let get_locals () =
    let res = ref [] in
    let get_var x (i, t, loc) = res := (i, (t, x, loc))::!res in
      Hashtbl.iter get_var env;
      Hashtbl.clear env;
      let res = List.sort (fun (i, _) (j, _) -> compare i j) !res in
      let (_, res) = List.split res in
	res
  in

  let rec translate_typ t =
    match t with
	Void -> 
	  Npkcontext.error "Compiler.translate_typ" "void not allowed here"
      | Scalar t -> K.Scalar (translate_scalar t)
      | Array (t, sz) -> K.Array (translate_typ t, sz)
      | StructOrUnion (_, fields, n) -> 
	  let translate_field (_, (o, t)) = (o, translate_typ t) in
	  K.Region (List.map translate_field fields, n)
      | Fun _ -> 
	  Npkcontext.error "Compiler.translate_typ" "function not allowed here"
	    
  and translate_scalar t = 
    match t with
	Int i -> N.Int i
      | Float n -> N.Float n      
      | Ptr (Fun _) -> N.FunPtr
      | Ptr _ -> N.Ptr
  in
  
  let translate_typ_as_ret t =
    match t with
	Void -> None
      | _ -> Some (translate_typ t)
  in

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
	  (* TODO: code cleanup: do not use size_of on csyntax, use it
	     on newspeak type. Remove redundant code. *)
	  let stride = K.exp_of_int (size_of t) in 
	    (K.BinOp (N.PlusPI, e1, K.BinOp (N.MultI, e2, stride)), t1)
	      (* TODO: clean bug ? maybe a cast is necessary ? *)
      | (Gt, _, _) -> 
	  let t = Int (N.Signed, Config.size_of_int) in
	    (K.BinOp (N.Gt (translate_scalar t1), e1, e2), t)
	      
      | (Eq, t1, t2) when t1 = t2 -> 
	  (* TODO: code cleanup factor return type, with Not *)
	  let t = translate_scalar t1 in
	    (K.BinOp (N.Eq t, e1, e2), Int (N.Signed, Config.size_of_int))
	      
      | _ -> 
	  Npkcontext.error "Compiler.translate_binop" 
	    "unexpected binary operator and arguments"
  in

  let register_formals f t = Hashtbl.add formals f t in
  
  let build_args f args loc =
    let formals =
      try Hashtbl.find formals f
      with Not_found -> 
	let i = ref (-1) in
	let build (_, t) =
	  incr i;
	  (Scalar t, "arg"^(string_of_int !i))
	in
	  List.map build args
    in
    let decls = 
      List.map2 (fun (_, t) (_, x) -> (Scalar t, x, loc)) args formals
    in
    let i = ref (-1) in
    let build_set (e, t) =
      incr i;
      let lv = K.Local !i in
	(K.Set (lv, e, translate_scalar t), loc)
    in
      (* TODO: code optimization: remove these List.rev!!! *)
    let set = List.rev (List.map build_set (List.rev args)) in
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
	  let t = deref_scalar t in
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
	  let t = scalar_of_typ t in
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

      | And _ -> 
	  Npkcontext.error "Compiler.translate_exp" 
	    "should be unreachable statement"
  in

  let update_fundef f (ret, args) body =
    let ret = translate_typ_as_ret ret in
    let args = List.map translate_typ args in
      try
	let (prev_args, prev_ret, prev_body) = Hashtbl.find fundefs f in
	  if args <> prev_args 
	  then begin 
	    Npkcontext.error "Compiler.update_fundef" 
	      ("different number of arguments for function "^f)
	  end;
	  if ret <> prev_ret 
	  then begin
	    Npkcontext.error "Compiler.update_fundef" 
	      "Return type do not match"
	  end;
	  match (body, prev_body) with
	      (None, _) -> ()
	    | (Some _, None) -> Hashtbl.replace fundefs f (args, ret, body)
	    | (Some _, Some _) -> 
		Npkcontext.error "Compiler.update_fundef" 
		  "Multiple definition of function body"
      with Not_found -> Hashtbl.add fundefs f (args, ret, body)
  in
  
  let rec append_decls d body =
    match d with
	(t, x, loc)::tl -> 
	  let t = translate_typ t in
	    (K.Decl (x, t, append_decls tl body), loc)::[]
      | [] -> body
  in

  let rec translate_blk (decls, body) =
    let push_local (d, loc) = push loc d in
      List.iter push_local decls;
      translate_stmt_list body

  and translate_stmt_list x =
    match x with
	hd::tl -> (translate_stmt hd)@(translate_stmt_list tl)
      | [] -> []

  and translate_set loc (lv, e) =
    (* TODO: code cleanup *)
    let (lv, t) = translate_lv lv in
    let (e, t') = translate_exp e in
      (* TODO: code cleanup: put these two together ?? *)
    let t = translate_scalar (scalar_of_typ t) in
    let t' = translate_scalar t' in
    let e = cast t' e t in
      (K.Set (lv, e, t), loc)
    

  and translate_stmt (x, loc) =
    Npkcontext.set_loc loc;
    match x with
      | Set (lv, Call x) -> 
	  translate_call loc (Some lv) x

      | Set (lv, e) -> (translate_set loc (lv, e))::[]


      | If ((And (e1, e2), body, loc)::tl) ->
	  let body = ([], [If ((e2, body, loc)::tl), loc]) in
	    translate_stmt (If ((e1, body, loc)::tl), loc)

      | If ((e, body, loc)::tl) ->
	  let cond1 = translate_bexp e in
	  let cond2 = K.negate cond1 in
	  let body = translate_blk body in
	  let else_body = translate_stmt (If tl, loc) in
	    (K.ChooseAssert [([cond1], body); ([cond2], else_body)], loc)::[]

      | If [] -> []

      | While (e, body) ->
	  let cond1 = translate_bexp e in
	  let cond2 = K.negate cond1 in
	  let body = translate_blk body in
	  let lbl = get_brk_lbl () in
	  let brk = (K.Goto lbl, loc)::[] in
	  let cond = K.ChooseAssert [([cond1], []); ([cond2], brk)] in
	  let loop = (K.InfLoop ((cond, loc)::body), loc)::[] in
	    (K.DoWith (loop, lbl, []), loc)::[]

      | DoWhile (body, e) ->
	  let cond1 = translate_bexp e in
	  let cond2 = K.negate cond1 in
	  let body = translate_blk body in
	  let lbl = get_brk_lbl () in
	  let brk = (K.Goto lbl, loc)::[] in
	  let cond = K.ChooseAssert [([cond1], []); ([cond2], brk)] in
	  let loop = (K.InfLoop (body@[cond, loc]), loc)::[] in
	    (K.DoWith (loop, lbl, []), loc)::[]

      | Return (Call x) -> 
	  let (_, t) = translate_lv (Var ret_vname) in
	  let () = push loc (t, "tmp") in
	    (* TODO: this is totally a hack, clean up by having an 
	       intermediate language
	       with explicit types :*)
	  let (lv, t) = translate_lv (Var ret_vname) in
	  let decl = (t, "tmp", dummy_loc)::[] in
	  let call = translate_call loc (Some (Var "tmp")) x in
	  let t = translate_scalar (scalar_of_typ t) in
	  let set = (K.Set (lv, K.Lval (K.Local 0, t), t), loc)::[] in
	    pop "tmp";
	    append_decls decl (call@set)

      | Return e -> 
	  let set = translate_set loc (Var ret_vname, e) in
	  let lbl = get_ret_lbl () in
	    set::(K.Goto lbl, loc)::[]

      | Exp (Call x) -> translate_call loc None x
      
      | Break -> [K.Goto (get_brk_lbl ()), loc]
      
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
	    let ret_t = translate_scalar (scalar_of_typ t) in
	      (t,
	      (t, "value_of_"^f, loc)::[], 
	      (K.Set (r, K.Lval (K.Local 0, ret_t), ret_t), loc)::[])
    in
      (* TODO: code cleanup put this together with the first push_dummy *)
    List.iter (fun _ -> push_dummy loc) args;

    let args = (List.map translate_exp args) in
    let args_t = List.map (fun (_, x) -> Scalar x) args in
    let (decls, set) = build_args f args loc in
      pop_dummy ();
      List.iter (fun _ -> pop_dummy ()) args;
      update_fundef f (ret_t, args_t) None;

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
    let default_lbl = get_brk_lbl () in
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
	  (v, ([], []), case_loc)::(v', body, _)::tl ->
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

  let translate_init is_extern x =
    match x with
	None when is_extern -> None
      | _ when is_extern -> 
	  Npkcontext.error "Compiler.translate_init" 
	    "Extern globals can not be initizalized"
      | None -> Some None
      | Some e -> 
	  Npkcontext.error "Compiler.translate_init" "Not implemented yet"
  in

  let translate_glbdecl x (t, loc, init, is_extern) =
    match (t, init) with
	(Fun (t, args), None) ->
	  let (args_t, _) = List.split args in
	    update_fundef x (t, args_t) None
	      
      | (Fun _, _) ->
	  Npkcontext.error "Compiler.translate_glbdecl" 
	    "Unexpected initialization"
      | _ ->
	  let init = translate_init is_extern init in
	    (* TODO: maybe do this in a first, since there may be the need for
	       a variable not encountered yet, in init*)
	    Hashtbl.add global_env x t;
	    Hashtbl.add globals x (translate_typ t, loc, init, true)
  in

  let translate_fundef f ((t, args), loc, body) =
    push loc (t, ret_vname);
    List.iter (push loc) args;
    register_formals f args;
    let body = translate_blk body in
    let (args_t, args_name) = List.split args in
      List.iter pop args_name;
      pop ret_vname;
      let body = (K.DoWith (body, get_ret_lbl (), []), loc)::[] in
      let decls = get_locals () in
      let body = append_decls decls body in
	update_fundef f (t, args_t) (Some body)
  in

  let cprog = parse fname in
  let (_, g, f) = Firstpass.translate cprog in
    
    Hashtbl.iter translate_glbdecl g;
    Hashtbl.iter translate_fundef f;
    (fname, globals, fundefs)
