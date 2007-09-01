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

(* TODO: code optimization: get rid of env, and use the globals Hashtbl
   rename it to env ! *)
open Lexing
open Csyntax

module N = Newspeak
module K = Npkil

let ret_vname = "!return"

(* TODO: check this for various architecture ? 
   Here align everything on 4 *)
let align t o = 
  if (o mod 4) = 0 then o
  else if K.size_of t <= (4 - (o mod 4)) then o
  else o + (4 - (o mod 4))

let align_end o = align (K.Scalar (N.Int (N.Signed, 4))) o

let rec offset_of f t =
  match t with
      (x, _, _)::tl when x <> f -> offset_of f tl
    | (_, o, t)::_ -> (o, t)
    | [] -> Npkcontext.error "Compiler.offset_of" ("field "^f^" not defined")
      

let int64_to_int x =
  if Int64.compare x (Int64.of_int max_int) > 0 
  then Npkcontext.error "Compiler.int64_to_int" "integer too big";
  if Int64.compare x (Int64.of_int max_int) > 0 
  then Npkcontext.error "Compiler.int64_to_int" "expecting positive integer";
  Int64.to_int x

let parse fname =
  let cin = open_in fname in
  let lexbuf = Lexing.from_channel cin in
    try
      let cprog = Parser.cprog Lexer.token lexbuf 
      in
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

let scalar_of_typ t = 
  match t with 
      K.Scalar t -> t
    | _ -> Npkcontext.error "Compiler.scalar_of_typ" "scalar type expected"

let region_of_typ t =
  match t with
      K.Region (fields, _) -> fields
    | _ -> 
	Npkcontext.error "Compiler.region_of_typ" 
	  "struct or union type expected"

let array_of_typ lv t =
  match (t, lv) with
      (K.Array (t, Some n), _) -> (t, K.Known n)
    | (K.Array (t, None), K.Global x) -> (t, K.Length x)
    | _ -> 
	Npkcontext.error "Compiler.array_of_typ" "Array of known size expected"

let translate_sign s =
  match s with
      Signed -> N.Signed
    | Unsigned -> N.Unsigned

let translate_ityp s t =
  match t with
      Char -> N.Int (s, Config.size_of_char)
    | Int -> N.Int (s, Config.size_of_int)

let rec translate_var_modifier b v =
  match v with
      Variable x -> (b, x)
    | FunctionName x -> 
	Npkcontext.error "Compiler.translate_decl" 
	  "local function definition not allowed"
    | Array (v, n) -> 
	let n = Some (int64_to_int n) in
	  translate_var_modifier (K.Array (b, n)) v
    | FunctionProto (Pointer v, _) -> 
	translate_var_modifier (K.Scalar N.FunPtr) v
    | Pointer v -> translate_var_modifier (K.Scalar N.Ptr) v
    | FunctionProto _ -> 
	Npkcontext.error "Compiler.translate_var_modifier" 
	  "case not implemented yet"

let rec translate_decl d =
  match d with
      Declaration (b, v, loc) -> 
	let b = translate_base_typ b in
	let (b, x) = translate_var_modifier b v in
	  (b, x, loc)
    | _ -> 
	Npkcontext.error "Compiler.translate_decl" 
	  "Unexpected function definition"

and translate_base_typ t =
  match t with
      Integer (s, t) -> K.Scalar (translate_ityp (translate_sign s) t)
    | Struct f -> K.Region (translate_struct_fields f)
    | Union f -> K.Region (translate_union_fields f)
    | Void -> 
	Npkcontext.error "Compiler.translate_base_typ"
	  "void type not allowed in variable declaration"

and translate_struct_fields f =
  let rec translate o f =
    match f with
	d::f -> 
	  let (t, x, _) = translate_decl d in
	  let sz = K.size_of t in
	  let o = align t o in
	  let (f, n) = translate (o+sz) f in
	    ((x, o, t)::f, n)
      | [] -> 
	  let o = align_end o in
	    ([], o)
  in
    match f with
	d::[] -> 
	  let (t, x, _) = translate_decl d in
	  let sz = K.size_of t in
	    ((x, 0, t)::[], sz)
      | _ -> translate 0 f

and translate_union_fields f =
  let n = ref 0 in
  let translate d =
    let (t, x, _) = translate_decl d in
    let sz = K.size_of t in
      if !n < sz then n := sz;
      (x, 0, t)
  in
    (List.map translate f, !n)
	    
let rec translate_fun_decl (b, v) =
  let b = 
    match b with
	Void -> None
      | _ -> Some (translate_base_typ b) 
  in
  let rec translate b v =
    match v with
	Variable x -> 
	  Npkcontext.error "Compiler.translate_fun_decl" "unreachable code"
      | FunctionName f -> (b, f)
      | Array _ -> 
	  Npkcontext.error "Compiler.translate_fun_decl" 
	    "function can not return array"
      | FunctionProto (Pointer v, _) -> 
	  Npkcontext.error "Compiler.translate_fun_decl" 
	    "function can not return function pointer"
      | Pointer v -> translate (Some (K.Scalar N.Ptr)) v
      | FunctionProto _ -> 
	  Npkcontext.error "Compiler.translate_var_modifier" 
	    "case not implemented yet"
  in
    translate b v

let get_ret_lbl () = 0

(* TODO: code cleanup: this could be in npkil and also used by cilcompiler ? *)
let cast e t =
  match (e, t) with
      (K.Const N.CInt64 _, N.Int _) -> e
    | (K.Const N.CInt64 c, N.Ptr) when c = Int64.zero -> K.Const N.Nil
    | (K.Const N.CInt64 c, N.Ptr) -> 
	Npkcontext.error "Compiler.cast" "cast from pointer to int forbidden"
    | (K.Lval (lv, t'), _) when t <> t' -> 
	Npkcontext.error "Compiler.cast" "case not implemented yet"
    | (K.Lval _, _) -> e
    | (K.BinOp ((N.PlusI|N.MultI), _, _), N.Int t) ->
	K.make_int_coerce t e
    | _ -> Npkcontext.error "Compiler.cast" "case not implemented yet"

let translate_binop op e1 e2 =
  let op =
    match op with
	Plus -> N.PlusI
      | Mult -> N.MultI
  in
    K.BinOp (op, e1, e2)

let compile fname = 
  let globals = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in
  let env = Hashtbl.create 100 in
  let vcnt = ref 0 in
  let push x t =
    incr vcnt;
    Hashtbl.add env x (!vcnt, t)
  in
  let pop x =
    vcnt := !vcnt - 1;
    Hashtbl.remove env x
  in
  let get_var x =
    try 
      let (n, t) = Hashtbl.find env x in
	(K.Local (!vcnt - n), t)
    with Not_found -> 
      try
	let (t, _, _, _) = Hashtbl.find globals x in
	  (K.Global x, t)
      with Not_found ->
	Npkcontext.error "Compiler.get_var" ("Variable "^x^" not declared")
  in
  let get_ret_var () = 
    try
      let (n, t) = Hashtbl.find env ret_vname in
	(K.Local (!vcnt - n), t)
    with Not_found -> 
	Npkcontext.error "Compiler.get_ret_var" 
	  ("function does not return a value")
  in
    
  let rec translate_lv lv =
    match lv with
	Var x -> get_var x
      | Field (lv, f) ->
	  let (lv, t) = translate_lv lv in
	  let r = region_of_typ t in
	  let (o, t) = offset_of f r in
	  let o = K.Const (N.CInt64 (Int64.of_int o)) in
	    (K.Shift (lv, o), t)
      | Index (lv, e) -> 
	  let (lv, t) = translate_lv lv in
	  let (t, n) = array_of_typ lv t in
	  let i = translate_exp e in
	  let sz = K.Const (N.CInt64 (Int64.of_int (K.size_of t))) in
	  let o = K.UnOp (K.Belongs_tmp (Int64.zero, K.Decr n), i) in
	  let o = K.BinOp (N.MultI, o, sz) in
	    (K.Shift (lv, o), t)

  and translate_exp e =
    match e with
	Const i -> K.Const (N.CInt64 i)
      | Lval lv -> 
	  let (lv, t) = translate_lv lv in
	  let t = scalar_of_typ t in
	    K.Lval (lv, t)
      | Binop (op, e1, e2) ->
	  let e1 = translate_exp e1 in
	  let e2 = translate_exp e2 in
	    translate_binop op e1 e2
  in

  let rec translate_blk (decls, body) =
    match decls with
	[] -> translate_stmt_list body
      | d::tl ->
	  let (t, x, loc) = translate_decl d in
	    push x t;
	    let body = translate_blk (tl, body) in
	      pop x;
	      (K.Decl (x, t, body), loc)::[]

  and translate_stmt_list x =
    match x with
	hd::tl -> (translate_stmt hd)@(translate_stmt_list tl)
      | [] -> []

  and translate_stmt (x, loc) =
    match x with
      | Set (lv, e) -> 
	  let (lv, t) = translate_lv lv in
	  let e = translate_exp e in
(* TODO: code cleanup: put these two together ?? *)
	  let t = scalar_of_typ t in
	  let e = cast e t in
	    (K.Set (lv, e, t), loc)::[]

      | Return e -> 
	  let e = translate_exp e in
	  let (lv, t) = get_ret_var () in
	  let t = scalar_of_typ t in
	  let e = cast e t in
	  let lbl = get_ret_lbl () in
	    (K.Set (lv, e, t), loc)::(K.Goto lbl, loc)::[]
  in

  let translate_global x =
    match x with
	FunctionDef (b, v, loc, body) -> 
	  let (t, f) = translate_fun_decl (b, v) in
	  let body =
	    match t with
		None -> translate_blk body
	      | Some t -> 
		  push ret_vname t;
		  let body = translate_blk body in
		    pop ret_vname;
		    body
	  in
	  let body = (K.DoWith (body, get_ret_lbl (), []), loc)::[] in
	    Hashtbl.add fundefs f ([], t, Some body) 
	      
      | Declaration _ -> 
	  let (t, x, loc) = translate_decl x in
	    Hashtbl.add globals x (t, loc, Some None, true)
  in

  let cprog = parse fname in
    
    List.iter translate_global cprog;
    (fname, globals, fundefs)
