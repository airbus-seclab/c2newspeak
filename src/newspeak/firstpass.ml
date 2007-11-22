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

let kind_of_arithmop k1 k2 =
  let k1 = C.promote k1 in
  let k2 = C.promote k2 in
    Newspeak.max_ikind k1 k2

let translate_unop op = 
  match op with
      Not -> C.Not

let translate_binop op t1 t2 =
  match (op, t1, t2) with
      (Mult, C.Int k1, C.Int k2) -> C.Mult (kind_of_arithmop k1 k2)
    | (Plus, C.Int k1, C.Int k2) -> C.Plus (kind_of_arithmop k1 k2)
    | (Minus, C.Int k1, C.Int k2) -> C.Minus (kind_of_arithmop k1 k2)
    | (Plus, C.Ptr t, C.Int _) -> C.PlusP t
    | (Gt, _, _) when t1 = t2 -> C.Gt t1
    | (Eq, _, _) when t1 = t2 -> C.Eq t1
    | _ ->
	Npkcontext.error "Csyntax.type_of_binop" 
	  "Unexpected binary operator and arguments"
  

let translate fname cprog =
  let glbdecls = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in

(* Local variables environment *)
  let vcnt = ref 0 in
  let local_env = Hashtbl.create 100 in
  let locals = ref [] in

  let get_locals () =
    let x = !locals in
      locals := [];
      vcnt := 0;
      Hashtbl.clear local_env;
      x
  in
(* TODO: don't use vcnt ??? Do not correspond !!! *)
  let typ_of_fun f =
    let (t, _, _) = Hashtbl.find fundefs f in
      t
  in

  let push_var loc (t, x) = 
    incr vcnt;
    Hashtbl.add local_env x (!vcnt, t, loc);
    !vcnt
  in
    
  let push_local = push_var in
  let pop_local loc (t, x) = 
    locals := (t, x, loc)::!locals;
    Hashtbl.remove local_env x 
  in
  let push_formals loc (args_t, ret_t) = 
    let _ = push_var loc (ret_t, ret_name) in
    let _ = List.map (push_var loc) args_t in
      ()
  in

  let get_var x = 
    try 
      let (n, t, _) = Hashtbl.find local_env x in
	(C.Local n, t)
    with Not_found -> 
      try 
	let (t, _, _, _) = Hashtbl.find glbdecls x in
	  (C.Global x, t)
      with Not_found ->
	Npkcontext.error "Firstpass.translate.typ_of_var" 
	  ("Unknown variable "^x)
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
	Var x -> get_var x
      | Field (lv, f) -> 
	  let (lv, t) = translate_lv lv in
	  let r = C.fields_of_typ t in
	  let (o, t) = List.assoc f r in
	    (C.Field (lv, f, o), t)
      | Index (lv, e) -> 
	  let (lv, t) = translate_lv lv in
	  let (t, n) = array_of_typ t in
	  let (i, _) = translate_exp e in
	    (C.Index (lv, (t, n), i), t)
      | Deref e -> 
	  let (e, t) = translate_exp e in
	  let t = deref_typ t in
	    (C.Deref (e, size_of t), t)

  and translate_exp x =
    match x with
	Cst i -> (C.Const i, C.typ_of_cst i)

      | Lval lv -> 
	  let (lv, t) = translate_lv lv in
	    (C.Lval (lv, t), t)

      | AddrOf lv -> 
	  let (lv, t) = translate_lv lv in
	    (C.AddrOf (lv, t), C.Ptr t) 

      | Unop (op, e) -> 
	  let (e, t) = translate_exp e in
	  let op = translate_unop op in
	  let t = C.typ_of_unop op in
	    (C.Unop (op, e), t)

      | Binop (op, e1, e2) -> 
	  let (e1, t1) = translate_exp e1 in
	  let (e2, t2) = translate_exp e2 in
	  let op = translate_binop op t1 t2 in
	  let t = C.typ_of_binop op in
	    (C.Binop (op, e1, e2), t)

      | Call (f, args) -> 
	  let (args_t, ret_t) = typ_of_fun f in
	  let fn = (f, (args_t, ret_t)) in
	    (C.Call (fn, List.map translate_exp args), ret_t)

      | SizeofV x -> 
	  let (e, t) = translate_exp (Lval (Var x)) in
	    (C.exp_of_int (size_of t), int_typ)

      (* TODO: should check that the size of all declarations is less than max_int *)
      | Sizeof t -> (C.exp_of_int (size_of t), int_typ)

      | And _ -> 
	  Npkcontext.error "Firstpass.translate_exp" "Unexpected And operator"
  in

  let translate_exp_option e =
    match e with
	None -> None
      | Some e -> Some (translate_exp e)
  in

  let rec translate_init t x =
    let res = ref [] in
    let o = ref 0 in
    let rec translate t x =
      match (x, t) with
	  (Data e, _) -> 
	    let e = translate_exp e in 
	      res := (!o, t, e)::!res;
	      t

	| (Sequence seq, C.Array (t, sz)) -> 
	    let n = 
	      match sz with
		  Some n -> n
		| None -> List.length seq
	    in
	      translate_sequence t n seq;
	      C.Array (t, Some n)

	| (CstStr str, C.Array _) -> 
	    let seq = seq_of_string str in
	      translate t (Sequence seq)

	| (CstStr str, C.Ptr _) -> 
	    let e = add_glb_cstr str in
	      res := (!o, t, e)::!res;
	      t

	| _ -> 
	    Npkcontext.error "Firstpass.translate_init"
	      "This type of initialization not implemented yet"

    and translate_sequence t n seq =
      match seq with
	  hd::tl when n > 0 -> 
	    let _ = translate t hd in
	      o := !o + C.size_of t;
	      translate_sequence t (n-1) tl
	| _::_ -> 
	    Npkcontext.print_warning 
	      "Firstpass.translate_init.translate_sequence" 
	      "Too many initializers for array"

	(* TODO: code cleanup: We fill with zeros, because CIL does too. 
	   But it shouldn't be done like that:
	   the region should be init to 0 by default and then filled with
	   values ?? *)
	| [] when n > 0 -> 
	    let _ = fill_with_zeros t in
	      o := !o + C.size_of t;
	      translate_sequence t (n-1) []
	| [] -> ()

    and fill_with_zeros t =
      match t with
	  C.Int _ -> res := (!o, t, (C.Const Int64.zero, t))::!res
	| C.Array (t, Some n) -> 
	    let sz = C.size_of t in
	      for i = 0 to n - 1 do
		fill_with_zeros t;
		o := !o + sz
	      done

	| _ -> 
	    Npkcontext.error "Firstpass.translate_init.fill_with_zeros"
	      "This type of initialization not implemented yet"
    in
    match x with
	None -> (t, None)
      | Some init -> 
	  let t = translate t init in
	    (t, Some (List.rev !res))

  and add_glb_cstr str =
    let name = "!"^fname^".const_str_"^str in
    let a = (char_typ, Some ((String.length str) + 1)) in
    let t = C.Array a in
      if not (Hashtbl.mem glbdecls name) then begin
	let loc = (fname, -1, -1) in
	let (t, init) = translate_init t (Some (CstStr str)) in
	  Hashtbl.add glbdecls name (t, loc, Some init, true)
      end;
      (C.AddrOf (C.Index (C.Global name, a, C.Const Int64.zero), t), 
      C.Ptr t)
  in

  let rec translate_blk x =
    match x with
      | (Decl (x, t, init), loc)::tl -> 
	  Npkcontext.set_loc loc;
	  let (t, init) = translate_init t init in
	  let n = push_local loc (t, x) in
	  let tl = translate_blk tl in begin 
	    pop_local loc (t, x);
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
	Set (lv, e) -> 
	  let lv = translate_lv lv in
	  let e = translate_exp e in
	  (C.Set (lv, e), loc)::[]

      | If ((And (e1, e2), body, loc)::tl) ->
	  let body = (If ((e2, body, loc)::tl), loc)::[] in
	    translate_stmt (If ((e1, body, loc)::tl), loc)

      | If branches -> 
	  let translate_case (e, body, loc) = 
	    (translate_exp e, translate_blk body, loc) 
	  in
	  let branches = List.map translate_case branches in
	    (C.If branches, loc)::[]

      | While (e, body) ->
	  let e = translate_exp e in
	  let body = translate_blk body in
	    (C.While (e, body), loc)::[]

      | DoWhile (body, e) -> 
	  let body = translate_blk body in
	  let e = translate_exp e in
	    (C.DoWhile (body, e), loc)::[]

      | Return None -> (C.Return, loc)::[]
	      
      | Return (Some e) ->
	  let lv = (Local ret_pos, get_ret_typ ()) in
	  let e = translate_exp e in
	    (C.Set (lv, e), loc)::(translate_stmt (Return None, loc))

      | Exp e -> 
	  let (e, _) = translate_exp e in
	    (C.Exp e, loc)::[]

      | Switch (e, cases) -> 
	  let (e, _) = translate_exp e in
	  let translate_case (e, body, loc) = 
	    (translate_exp_option e, translate_blk body, loc) 
	  in
	  let cases = List.map translate_case cases in
	    (C.Switch (e, cases), loc)::[]

      | Break -> (C.Break, loc)::[]

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
      | GlbDecl (_, _, is_extern, _, Some _) when is_extern -> 
	  Npkcontext.error "Firstpass.translate_global"
	    "Extern globals can not be initizalized"
 
      | GlbDecl (x, t, is_extern, is_const, init) ->
	  begin match (t, init) with
	      (Fun ft, None) -> update_fundef x (ft, loc, None)
	    | (Fun ft, Some _) -> 
		Npkcontext.error "Firstpass.translate_global"
		  ("Unexpected initialization of function "^x)
	    | _ -> 
		let (t, init) = translate_init t init in
		let init = if is_extern then None else Some init in
		  Hashtbl.add glbdecls x (t, loc, init, is_const)
	  end
  in
  
  List.iter translate_global cprog;
  (Hashtbl.create 100, glbdecls, fundefs)
