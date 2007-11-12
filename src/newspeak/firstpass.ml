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

let int64_to_int x =
  if Int64.compare x (Int64.of_int max_int) > 0 
  then Npkcontext.error "Firstpass.int64_to_int" "integer too big";
  if Int64.compare x (Int64.of_int max_int) > 0 
  then Npkcontext.error "Firstpass.int64_to_int" "expecting positive integer";
  Int64.to_int x

(* TODO: check this for various architecture ? 
   Here align everything on 4 *)
let align o sz = 
  let offset = o mod 4 in
  if offset = 0 then o
  else if offset + sz <= 4 then o
  else (o - offset) + 4

let char_typ = C.Int (Newspeak.Signed, Config.size_of_char)

(* TODO: remove this code
  (* TODO: code cleanup: find a way to factor this with create_cstr
     in Npkil *)
let init_of_string str =
  let len = String.length str in
  let res = ref [(len, char_typ, C.Const Int64.zero)] in
    for i = len - 1 downto 0 do 
      let c = Char.code str.[i] in
	res := (i, char_typ, C.Const (Int64.of_int c))::!res
    done;
    (len + 1, Some !res)
*)

let seq_of_string str =
  let len = String.length str in
  let res = ref [Data (Const Int64.zero)] in
    for i = len - 1 downto 0 do
      let c = Char.code str.[i] in
	res := (Data (Const (Int64.of_int c)))::!res
    done;
    !res

let translate fname cprog =
(* TODO: use Env!!! *)
  let typedefs = Hashtbl.create 100 in
  let glbdecls = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in
(* TODO: have just an env! *)
  let local_env = Hashtbl.create 100 in
  let vcnt = ref 0 in
  let locals = ref [] in

  let add_local x t loc = locals := (t, x, loc)::!locals in
  let get_locals () =
    let x = !locals in
      locals := [];
      vcnt := 0;
      x
  in
(* TODO: don't use vcnt ??? Do not correspond !!! *)
  let typ_of_fun f =
    let (t, _, _) = Hashtbl.find fundefs f in
      t
  in

  let push_var x t loc = 
    incr vcnt;
    Hashtbl.add local_env x (!vcnt, t, loc);
    !vcnt
  in
  let pop_var x = 
    Hashtbl.remove local_env x 
  in

(* TODO: code cleanup, use Env always *)
  let push_formals (args, t) loc =
    let _ = push_var (Env.get_ret_name ()) t loc in
      List.iter (fun (t, x) -> let _ = push_var x t loc in ()) args
  in
(* TODO: code cleanup, this is probably unnecessary *)
  let pop_formals (args, _) =
    List.iter (fun (_, x) -> pop_var x) args;
    pop_var (Env.get_ret_name ())
  in

  let get_var x = 
    try 
      let (n, t, _) = Hashtbl.find local_env x in
	(C.Local (n, t), t)
    with Not_found -> 
      try 
	let (t, _, _) = Hashtbl.find glbdecls x in
	  (C.Global (x, t), t)
      with Not_found ->
	Npkcontext.error "Firstpass.translate.typ_of_var" 
	  ("Unknown variable "^x)
  in

  let rec translate_decl (b, v) =
    let b = translate_base_typ b in
    let (t, x) = translate_var_modifier b v in
      (t, x)
	
  and translate_base_typ t =
    match t with
	Integer (s, t) -> C.Int (s, size_of_ityp t)
      | Struct f -> C.StructOrUnion (translate_struct_fields f)
      | Union f -> C.StructOrUnion (translate_union_fields f)
      | Void -> C.Void
      | Name x -> 
	  try Hashtbl.find typedefs x
	  with Not_found -> 
	    Npkcontext.error "Firstpass.translate_base_typ" ("Unknown type "^x)

  and translate_struct_fields f =
    let rec translate o f =
      match f with
	  d::f ->
	    let (t, x) = translate_decl d in
	    let sz = C.size_of t in
	    let o = align o sz in
	    let (f, n) = translate (o+sz) f in
	      ((x, (o, t))::f, n)
	| [] -> ([], align o Config.size_of_int)
    in
    let (f, n) = 
      match f with
	  d::[] ->
	    let (t, x) = translate_decl d in
	    let sz = C.size_of t in
	      ((x, (0, t))::[], sz)
	| _ -> translate 0 f 
    in
      (true, f, n)

  and translate_union_fields f =
    let n = ref 0 in
    let translate d =
      let (t, x) = translate_decl d in
      let sz = size_of t in
	if !n < sz then n := sz;
	(x, (0, t))
    in
      (false, List.map translate f, !n)

  and translate_var_modifier b v =
    match v with
	Abstract -> (b, C.undefined)
      | Variable x -> (b, x)
      | Function (Variable f, args) -> 
	  (C.Fun (List.map translate_decl args, b), f)
      | Function (Pointer v, args) -> 
	  let args = List.map translate_decl args in
	    translate_var_modifier (C.Ptr (C.Fun (args, b))) v
      | Array (v, n) -> 
	  let n = 
	    match n with
		None -> None
	      | Some n -> Some (int64_to_int n) 
	  in
	    translate_var_modifier (C.Array (b, n)) v
      | Pointer v -> translate_var_modifier (C.Ptr b) v
      | Function _ -> 
	  Npkcontext.error "Firstpass.translate_var_modifier" 
	    "case not implemented yet"
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
	  let (t, _) = array_of_typ t in
	  let (i, _) = translate_exp e in
	    (C.Index (lv, i), t)
      | Deref e -> 
	  let (e, t) = translate_exp e in
	  let t = deref_typ t in
	    (C.Deref e, t)

  and translate_exp x =
    match x with
	Const i -> (C.Const i, C.typ_of_cst i)
      | Lval lv -> 
	  let (lv, t) = translate_lv lv in
	    (C.Lval lv, t)
      | AddrOf lv -> 
	  let (lv, t) = translate_lv lv in
	    (C.AddrOf lv, C.Ptr t) 
      | Unop (op, e) -> 
	  let (e, t) = translate_exp e in
	  let t = C.typ_of_unop op in
	    (C.Unop (op, e), t)
      | Binop (op, e1, e2) -> 
	  let (e1, t1) = translate_exp e1 in
	  let (e2, t2) = translate_exp e2 in
	  let t = C.typ_of_binop op t1 t2 in
	    (C.Binop (op, e1, e2), t)
      | Call (f, args) -> 
	  let (_, t) = typ_of_fun f in
	  let translate_arg x = 
	    let (e, _) = translate_exp x in
	      e
	  in
	    (C.Call (f, List.map translate_arg args), t)
      | SizeofV x -> 
	  let (e, _) = translate_exp (Lval (Var x)) in
	    (C.Sizeof e, C.int_typ)
      | Sizeof d ->
	  let (t, _) = translate_decl d in
	  let sz = size_of t in
(* TODO: should check that the size of all declarations is less than max_int *)
	    translate_exp (Const (Int64.of_int sz))
      | And _ -> 
	  Npkcontext.error "Firstpass.translate_exp" "Unexpected And operator"
  in

  let translate_exp_option e =
    match e with
	None -> None
      | Some e -> 
	  let (e, _) = translate_exp e in
	    Some e
  in

  let rec translate_init t x =
    let res = ref [] in
    let o = ref 0 in
    let rec translate t x =
      match (x, t) with
	  (Data e, _) -> 
	    let (v, _) = translate_exp e in 
	      res := (!o, t, v)::!res;
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
	   values.*)
	| [] when n > 0 -> 
	    let _ = fill_with_zeros t in
	      o := !o + C.size_of t;
	      translate_sequence t (n-1) []
	| [] -> ()

    and fill_with_zeros t =
      match t with
	  C.Int _ -> res := (!o, t, C.Const Int64.zero)::!res
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
    let t = C.Array (char_typ, None) in
    let t =
      if (Hashtbl.mem glbdecls name) then t
      else begin
	let loc = (fname, -1, -1) in
	let (t, init) = translate_init t (Some (CstStr str)) in
	  Hashtbl.add glbdecls name (t, loc, Some init);
	  t
      end
    in
      C.AddrOf (C.Index (C.Global (name, t), C.Const Int64.zero))
  in

  let rec translate_blk x =
    match x with
      | (Decl (d, init), loc)::tl -> 
	  Npkcontext.set_loc loc;
	  let (t, x) = translate_decl d in
	  let (t, init) = translate_init t init in
	  let n = push_var x t loc in
	  let tl = translate_blk tl in begin 
	    pop_var x;
	    add_local x t loc;
	    match init with
		None -> tl
	      | Some init -> (C.Init ((n, t), init), loc)::tl
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
	  let (lv, _) = translate_lv lv in
	  let (e, _) = translate_exp e in
	  (C.Set (lv, e), loc)::[]

      | If ((And (e1, e2), body, loc)::tl) ->
	  let body = (If ((e2, body, loc)::tl), loc)::[] in
	    translate_stmt (If ((e1, body, loc)::tl), loc)

      | If branches -> 
	  let translate_case (e, body, loc) = 
	    let (e, _) = translate_exp e in
	      (e, translate_blk body, loc) 
	  in
	  let branches = List.map translate_case branches in
	    (C.If branches, loc)::[]

      | While (e, body) ->
	  let (e, _) = translate_exp e in
	  let body = translate_blk body in
	    (C.While (e, body), loc)::[]

      | DoWhile (body, e) -> 
	  let body = translate_blk body in
	  let (e, _) = translate_exp e in
	    (C.DoWhile (body, e), loc)::[]

      | Return e -> 
	  let (e, _) = translate_exp e in
	  (C.Return e, loc)::[]

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

(* TODO: code cleanup: put in Csyntax, change function name!!! *)
  let compare f (args, ret) (prev_args, prev_ret) =
    let (args, _) = List.split args in
    let (prev_args, _) = List.split prev_args in
      if (args, ret) <> (prev_args, prev_ret) then begin
	Npkcontext.error "Firstpass.update_fundef"
	  ("Different types for function "^f)
      end
  in

  let update_fundef f (t, loc, body) =
    try
      let (prev_t, _, prev_body) = Hashtbl.find fundefs f in
	compare f t prev_t;
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
	FunctionDef (x, body) ->
	  let (t, x) = translate_decl x in
	  let ft = C.ftyp_of_typ t in
	  let () = push_formals ft loc in
	  let body = translate_blk body in
(*
	  let () = pop_formals ft in*)
(* TODO: cleanup *)
	    Hashtbl.clear local_env;
	  let locals = get_locals () in
	    update_fundef x (ft, loc, Some (locals, body))

      | GlbDecl (is_extern, _, Some _) when is_extern -> 
	  Npkcontext.error "Firstpass.translate_global"
	    "Extern globals can not be initizalized"
 
      | GlbDecl (is_extern, d, init) -> 
	  let (t, x) = translate_decl d in
	    begin match (t, init) with
		(Fun ft, None) -> update_fundef x (ft, loc, None)
	      | (Fun ft, Some _) -> 
		  Npkcontext.error "Firstpass.translate_global"
		    ("Unexpected initialization of function "^x)
	      | _ -> 
		  let (t, init) = translate_init t init in
		  let init = if is_extern then None else Some init in
		    Hashtbl.add glbdecls x (t, loc, init)
	    end

      | Typedef x -> 
	  let (t, x) = translate_decl x in
	    Hashtbl.add typedefs x t
  in
  
  List.iter translate_global cprog;
  (Hashtbl.create 100, glbdecls, fundefs)
