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

(* Translates bare_csyntax to csyntax *)
open Csyntax
open Bare_csyntax
module C = Csyntax

let ret_pos = 1
let ret_name = "!return"

let char_typ = C.Int (Newspeak.Signed, Config.size_of_char)

let rec simplify_bexp e =
  match e with
      Var _ | Field _ | Index _ | Deref _ | Call _ -> 
	Unop (Not, Binop (Eq, e, Cst Int64.zero))
    | Unop (Not, e) -> Unop (Not, simplify_bexp e)
    | _ -> e

let translate_proto_ftyp f (args, ret) = 
  let args =
    match args with
	[] -> 
	  Npkcontext.print_warning "Firstpass.translate_proto_ftyp" 
	    ("Incomplete prototype for function "^f);
	  []
      | (C.Void, _)::[] -> []
      | _ -> args  
  in
  (args, ret)



(* TODO: code cleanup: find a way to factor this with create_cstr
   in Npkil *)
let seq_of_string str =
  let len = String.length str in
  let res = ref [Data (Cst Int64.zero)] in
    for i = len - 1 downto 0 do
      let c = Char.code str.[i] in
	res := (Data (Cst (Int64.of_int c)))::!res
    done;
    !res

let translate_arithmop op (e1, k1) (e2, k2) =
  let k = Newspeak.max_ikind (C.promote k1) (C.promote k2) in
  let t = C.Int k in
    (op k, C.cast (e1, C.Int k1) t, C.cast (e2, C.Int k2) t)

let translate_unop op = 
  match op with
      Not -> C.Not

let rec translate_binop op (e1, t1) (e2, t2) =
  match (op, t1, t2) with
      (* Arithmetic operations *)
      (Mult, C.Int k1, C.Int k2) -> 
	translate_arithmop (fun x -> C.Mult x) (e1, k1) (e2, k2)
    | (Plus, C.Int k1, C.Int k2) -> 
	translate_arithmop (fun x -> C.Plus x) (e1, k1) (e2, k2)
    | (Minus, C.Int k1, C.Int k2) -> 
	translate_arithmop (fun x -> C.Minus x) (e1, k1) (e2, k2)
    | (Plus, C.Ptr t, C.Int _) -> (C.PlusP t, e1, e2)
    | (Plus, C.Array (elt_t, _), _) -> 
	let t = C.Ptr elt_t in
	  translate_binop Plus (C.cast (e1, t1) t, t) (e2, t2)

      (* Integer comparisons *)
    | (Gt, C.Int k1, C.Int k2) -> 
	translate_arithmop (fun x -> C.Gt (C.Int x)) (e1, k1) (e2, k2)
    | (Eq, C.Int k1, C.Int k2) -> 
	translate_arithmop (fun x -> C.Eq (C.Int x)) (e1, k1) (e2, k2)

      (* Pointer comparisons *)
    | (Eq, C.Ptr _, C.Ptr _) ->	(C.Eq t1, e1, e2)
    | (Eq, C.Int _, C.Ptr _) ->
	let e1 = C.cast (e1, t1) t2 in
	  (C.Eq t2, e1, e2)
    | (Eq, C.Ptr _, C.Int _) ->
	let e2 = C.cast (e2, t2) t1 in
	  (C.Eq t1, e1, e2)
	  
    | _ ->
	Npkcontext.error "Csyntax.type_of_binop" 
	  "Unexpected binary operator and arguments"

let translate fname (compdefs, globals) =
  let glbdecls = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in

(* Local variables environment *)
  let vcnt = ref 0 in
  let local_env = Hashtbl.create 100 in
  let locals = ref [] in

  let tmp_cnt = ref 0 in

  let get_locals () =
    let x = !locals in
      locals := [];
      vcnt := 0;
      Hashtbl.clear local_env;
      x
  in

  let push_var loc (t, x) = 
    incr vcnt;
    Hashtbl.add local_env x (!vcnt, t, loc);
    !vcnt
  in
    
  let push_local = push_var in
  let pop_local x =
    let (_, t, loc) = Hashtbl.find local_env x in
      locals := (t, x, loc)::!locals;
      Hashtbl.remove local_env x 
  in
  let push_formals loc (args_t, ret_t) = 
    let _ = push_var loc (ret_t, ret_name) in
    let _ = List.map (push_var loc) args_t in
      ()
  in

  let create_tmp t loc = 
    let name = "tmp"^(string_of_int !tmp_cnt) in
    let x = push_local loc (t, name) in
      incr tmp_cnt;
      (C.Local x, name)
  in

(* TODO: could be a function name inside *)
  let get_var x = 
    try 
      let (n, t, _) = Hashtbl.find local_env x in
	(C.Local n, t)
    with Not_found -> 
      try 
	let (t, _, _) = Hashtbl.find glbdecls x in
	  (C.Global x, t)
      with Not_found ->
	try 
	  let (ft, _, _) = Hashtbl.find fundefs x in
	    (C.Global x, C.Fun ft)
	with Not_found ->
	  Npkcontext.error "Firstpass.translate.typ_of_var" 
	    ("Unknown variable "^x)
  in

  let typ_of_var x =
    let (_, t) = get_var x in
      t
  in

  let get_ret_typ () =
    try 
      let (_, t, _) = Hashtbl.find local_env ret_name in
	t
    with Not_found -> 
      Npkcontext.error "Firstpass.get_ret_typ" 
	"Function does not return any value"

  in

  let rec translate_lv x =
    match x with
	Var x -> ([], get_var x)
      | Field (lv, f) -> 
	  let (pref, (lv, t)) = translate_lv lv in
	  let r = C.fields_of_typ compdefs t in
	  let (o, t) = List.assoc f r in
	    (pref, (C.Field (lv, f, o), t))
      | Index (lv, e) -> 
	  let (lv_pref, (lv', t)) = translate_lv lv in begin
	    match t with
		Array (t, n) ->
		  let (e_pref, (i, _)) = translate_exp e in
		    (lv_pref@e_pref, (C.Index (lv', (t, n), i), t))
	      | Ptr _ -> translate_lv (Deref (Binop (Plus, lv, e)))
	      | _ -> 
		  Npkcontext.error "Firstpass.translate_lv" 
		    "Array or pointer type expected"
	  end
      | Deref e -> 
	  let (pref, (e, t)) = translate_exp e in
	  let t = deref_typ t in
	    (pref, (C.Deref (e, t), t))

      | _ -> Npkcontext.error "Firstpass.translate_lv" "Left value expected"

  and translate_exp x =
    match x with
	Cst i -> ([], (C.Const i, C.typ_of_cst i))

      | Var _ | Field _ | Index _ | Deref _ -> 
	  let (pref, (lv, t)) = translate_lv x in
	    (pref, (C.Lval (lv, t), t))

      | AddrOf lv -> 
	  let (pref, (lv, t)) = translate_lv lv in
	    (pref, (C.AddrOf (lv, t), C.Ptr t))

      | Unop (op, e) -> 
	  let (pref, (e, t)) = translate_exp e in
	  let op = translate_unop op in
	  let t = C.typ_of_unop op in
	    (pref, (C.Unop (op, e), t))

      | Binop (op, e1, e2) -> 
	  let (pref1, e1) = translate_exp e1 in
	  let (pref2, e2) = translate_exp e2 in
	  let (op, e1, e2) = translate_binop op e1 e2 in
	  let t = C.typ_of_binop op in
	    (pref1@pref2, (C.Binop (op, e1, e2), t))

      | Call (f, args) -> translate_call f args

      | SizeofV x -> 
	  let t = typ_of_var x in
	    translate_exp (Sizeof t)

      (* TODO: should check that the size of all declarations is less than max_int *)
      | Sizeof t -> ([], (C.exp_of_int (size_of compdefs t), int_typ))

      | Str str -> 
	  let e = add_glb_cstr str in
	    ([], e)

      (* TODO: this could be the unique place with And (remove the And down in 
	 the code of if then else !!! *)
      | And (e1, e2) -> 
	  let loc = Npkcontext.get_loc () in
	  let (tmp, tmp_name) = create_tmp int_typ loc in
	  let tmp_e = (C.Lval (tmp, int_typ), int_typ) in
	  let stmt = 
	    If (And (e1, e2), 
	       (Exp (Set (Var tmp_name, Cst Int64.one)), loc)::[], 
	       (Exp (Set (Var tmp_name, Cst Int64.zero)), loc)::[])
	  in
	  let pref = translate_stmt (stmt, loc) in
	    pop_local tmp_name;
	    (pref, tmp_e)

      | Or (e1, e2) -> 
	  let loc = Npkcontext.get_loc () in
	  let (tmp, tmp_name) = create_tmp int_typ loc in
	  let tmp_e = (C.Lval (tmp, int_typ), int_typ) in
	  let stmt = 
	    If (Or (e1, e2), 
	       (Exp (Set (Var tmp_name, Cst Int64.one)), loc)::[], 
	       (Exp (Set (Var tmp_name, Cst Int64.zero)), loc)::[])
	  in
	  let pref = translate_stmt (stmt, loc) in
	    pop_local tmp_name;
	    (pref, tmp_e)


      | Cast (e, t) -> 
	  let (pref, e) = translate_exp e in
	  let e = C.cast e t in
	    (pref, (e, t))

      | Set (lv, e) -> 
	  let loc = Npkcontext.get_loc () in
	  let (lv_pref, (lv, t)) = translate_lv lv in
	  let (e_pref, e) = translate_exp e in
	  let e = C.cast e t in
	    (lv_pref@e_pref@((C.Set (lv, t, e), loc)::[]), (Lval (lv, t), t))

      | ExpPlusPlus lv ->
	  let loc = Npkcontext.get_loc () in
	  let (pref_lv, (lv', t)) = translate_lv lv in
	  let (tmp, tmp_name) = create_tmp t loc in
	  let e = C.Lval (lv', t) in
	  let sav_set = (C.Set (tmp, t, e), loc) in
	  let incr_set = Set (lv, Binop (Plus, Var tmp_name, Cst Int64.one)) in
	  let (pref, _) = translate_exp incr_set in
	    pop_local tmp_name;
	    (pref_lv@(sav_set::pref), (C.Lval (tmp, t), t))


  and translate_arg e (t, _) = 
    let (pref, e) = translate_exp e in
      (pref, C.cast e t)

  (*
    TODO: a final optimization to remove unused variables....
  *)
  and translate_call f args =
    let loc = Npkcontext.get_loc () in
    let (pref_fn, fn, ft) = translate_fn f in
    let (args_t, ret_t) = ft in
    let (pref, args) = 
      try List.split (List.map2 translate_arg args args_t) 
      with Invalid_argument "List.map2" ->
	Npkcontext.error "Compiler.translate_call" 
	  ("Different types at function call")
    in
    let pref = List.concat pref in
    let (lv, e) =
      match ret_t with
	  (* Any expression is ok, since it is going to be thrown away *)
	  C.Void -> (None, C.exp_of_int 1)
	| _ -> 
	    let (tmp, tmp_name) = create_tmp ret_t loc in
	      pop_local tmp_name;
	      (Some tmp, C.Lval (tmp, ret_t))
    in
    let call = (C.Call (lv, (fn, ft), args), loc) in
      (pref_fn@pref@(call::[]), (e, ret_t))

  and translate_fn f =
    let (pref, (f, t)) = translate_lv f in
    let (fn, t) = 
      match t with
	  C.Ptr ft -> (C.Deref (C.Lval (f, t), ft), ft)
	| _ -> (f, t)
    in
    let t = C.ftyp_of_typ t in
      (pref, fn, t)

  and translate_exp_option e =
    match e with
	None -> ([], None)
      | Some e -> 
	  let (pref, e) = translate_exp e in
	    (pref, Some e)

  and translate_init t x =
    let res = ref [] in
    let rec translate o t x =
      match (x, t) with
	  (Data (Str str), C.Array _) -> 
	    let seq = seq_of_string str in
	      translate o t (Sequence seq)

	| (Data e, _) -> 
	    let (pref, e) = translate_exp e in 
	    let e = cast e t in
	      if (pref <> []) then begin 
		Npkcontext.error "Firstpass.translate_init"
		  "Expression without side-effects expected"
	      end;
	      res := (o, t, e)::!res;
	      t

	| (Sequence seq, C.Array (t, sz)) -> 
	    let n = 
	      match sz with
		  Some n -> n
		| None -> List.length seq
	    in
	      translate_sequence o t n seq;
	      C.Array (t, Some n)

	| (Sequence seq, C.Struct n) -> 
	    let (f, _) = Hashtbl.find compdefs n in
	      List.iter2 (translate_field_sequence o) f seq;
	      C.Struct n

	| _ -> 
	    Npkcontext.error "Firstpass.translate_init"
	      "This type of initialization not implemented yet"

    and translate_field_sequence o (f, (f_o, t)) x =
      let o = o + f_o in
      let _ = translate o t x in
	()

    and translate_sequence o t n seq =
      match seq with
	  hd::tl when n > 0 -> 
	    let _ = translate o t hd in
	    let o = o + C.size_of compdefs t in
	      translate_sequence o t (n-1) tl
	| _::_ -> 
	    Npkcontext.print_warning 
	      "Firstpass.translate_init.translate_sequence" 
	      "Too many initializers for array"

	(* TODO: code cleanup: We fill with zeros, because CIL does too. 
	   But it shouldn't be done like that:
	   the region should be init to 0 by default and then filled with
	   values ?? *)
	| [] when n > 0 -> 
	    let _ = fill_with_zeros o t in
	    let o = o + C.size_of compdefs t in
	      translate_sequence o t (n-1) []
	| [] -> ()

    and fill_with_zeros o t =
      match t with
	  C.Int _ -> res := (o, t, C.Const Int64.zero)::!res
	| C.Array (t, Some n) -> 
	    let sz = C.size_of compdefs t in
	    let o = ref o in
	      for i = 0 to n - 1 do
		fill_with_zeros !o t;
		o := !o + sz
	      done

	| _ -> 
	    Npkcontext.error "Firstpass.translate_init.fill_with_zeros"
	      "This type of initialization not implemented yet"
    in
    match x with
	None -> (t, None)
      | Some init -> 
	  let t = translate 0 t init in
	    (t, Some (List.rev !res))

  and add_glb_cstr str =
    let name = "!"^fname^".const_str_"^str in
    let a = (char_typ, Some ((String.length str) + 1)) in
    let t = C.Array a in
      if not (Hashtbl.mem glbdecls name) then begin
	let loc = (fname, -1, -1) in
	let (t, init) = translate_init t (Some (Data (Str str))) in
	  Hashtbl.add glbdecls name (t, loc, Some init)
      end;
      (C.AddrOf (C.Index (C.Global name, a, C.Const Int64.zero), t), 
      C.Ptr char_typ)

  and translate_blk x =
    match x with
      | (Decl (x, t, init), loc)::tl -> 
	  Npkcontext.set_loc loc;
	  let (t, init) = translate_init t init in
	  let n = push_local loc (t, x) in
	  let tl = translate_blk tl in begin 
	    pop_local x;
	    match init with
		None -> tl
	      | Some init -> (C.Init (n, init), loc)::tl
	  end

      | hd::tl -> 
	  let hd = translate_stmt hd in
	  let tl = translate_blk tl in
	    hd@tl

      | [] -> []

  and translate_stmt (x, loc) =
    Npkcontext.set_loc loc;
    match x with
	If (And (e1, e2), blk1, blk2) ->
	  let if_e1_blk = (If (e2, blk1, blk2), loc)::[] in
	    translate_stmt (If (e1, if_e1_blk, blk2), loc)

      | If (Or (e1, e2), blk1, blk2) ->
	  let if_not_e1_blk = (If (e2, blk1, blk2), loc)::[] in
	    translate_stmt (If (e1, blk1, if_not_e1_blk), loc)

      | If (Unop (Not, (And _ as e)), blk1, blk2) ->
	  translate_stmt (If (e, blk2, blk1), loc)

      | If (e, blk1, blk2) ->
	  let e = simplify_bexp e in
	  let (pref, (e, _)) = translate_exp e in
	  let blk1 = translate_blk blk1 in
	  let blk2 = translate_blk blk2 in
	    pref@((C.If (e, blk1, blk2), loc)::[])

      | While (e, body) ->
	  let loop_exit = translate_stmt (If (e, [], (Break, loc)::[]), loc) in
	  let body = translate_blk body in
	    (C.Loop (loop_exit@body, []), loc)::[]

      | DoWhile (body, e) -> 
	  let body = translate_blk body in
	  let loop_exit = translate_stmt (If (e, [], (Break, loc)::[]), loc) in
	    (C.Loop (body@loop_exit, []), loc)::[]

      | For (init, e, body, step) ->
	  let init = translate_blk init in
	  let loop_exit = translate_stmt (If (e, [], (Break, loc)::[]), loc) in
	  let body = translate_blk body in
	  let step = translate_blk step in
	    init@(C.Loop (loop_exit@body, step), loc)::[]

      | Return None -> (C.Return, loc)::[]
	      
      | Return (Some e) ->
	  let set = (Exp (Set (Var ret_name, e)), loc) in
	  let return = (Return None, loc) in
	    translate_blk (set::return::[])

      | Exp e ->
	  let (pref, _) = translate_exp e in
	    pref

      | Switch (e, cases) -> 
	  let (pref, (e, _)) = translate_exp e in
	  let pref = ref pref in
	  let translate_case (e, body, loc) = 
	    let (pref_case, e) = translate_exp_option e in
	      pref := pref_case@(!pref);
	      (e, translate_blk body, loc) 
	  in
	  let cases = List.map translate_case cases in
	    !pref@(C.Switch (e, cases), loc)::[]

      | Break -> (C.Break, loc)::[]

      | Continue -> (C.Continue, loc)::[]

      | Block body -> translate_blk body

      | Decl _ -> 
	  Npkcontext.error "Firstpass.translate.translate_stmt" 
	    "Unreachable statement"
  in

  let update_fundef f (t, loc, body) =
    try
      let (prev_t, _, prev_body) = Hashtbl.find fundefs f in
	if not (C.ftyp_equals t prev_t) then begin
	  Npkcontext.error "Firstpass.update_fundef"
	    ("Different types for function "^f)
	end;
	match (body, prev_body) with
	    (None, _) -> ()
	  | (Some _, None) -> Hashtbl.replace fundefs f (t, loc, body)
	  | (Some _, Some _) -> 
	      Npkcontext.error "Firstpass.update_fundef"
		("Multiple definitions of function "^f^" body")
    with Not_found -> Hashtbl.add fundefs f (t, loc, body) 
  in

  let translate_global (x, loc) =
    Npkcontext.set_loc loc;
    match x with
	FunctionDef (x, t, body) ->
	  let ft = C.ftyp_of_typ t in
	  let _ = push_formals loc ft in
	  let body = translate_blk body in
	  let locals = get_locals () in
	    update_fundef x (ft, loc, Some (locals, body))

(* TODO: put this check in parser ?? *)
      | GlbDecl (_, _, is_extern, Some _) when is_extern -> 
	  Npkcontext.error "Firstpass.translate_global"
	    "Extern globals can not be initizalized"
 
      | GlbDecl (x, t, is_extern, init) ->
	  begin match (t, init) with
	      (Fun ft, None) -> 
		let ft = translate_proto_ftyp x ft in
		  update_fundef x (ft, loc, None)
	    | (Fun ft, Some _) -> 
		Npkcontext.error "Firstpass.translate_global"
		  ("Unexpected initialization of function "^x)
	    | _ -> 
		let (t, init) = translate_init t init in
		let init = if is_extern then None else Some init in
		  Hashtbl.add glbdecls x (t, loc, init)
	  end
  in
  
    List.iter translate_global globals;
    (compdefs, glbdecls, fundefs)
