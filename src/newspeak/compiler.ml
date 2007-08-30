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

(* TODO: check this for various architecture ? 
   Here align everything on 4 *)
let align t o = 
  if (o mod 4) = 0 then o
  else if K.size_of t <= (4 - (o mod 4)) then o
  else o + (4 - (o mod 4))

let align_end o = align (K.Scalar (N.Int (N.Signed, 4))) o

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

let scalar_of_t t = 
  match t with 
      K.Scalar t -> t
    | _ -> Npkcontext.error "Compiler.translate_stmt" "scalar type expected"

let get_var env x =
  let rec get n env =
    match env with
      | (y, t)::_ when y = x -> (n, t)
      | _::tl -> get (n+1) tl
      | [] -> 
	  Npkcontext.error "Compiler.get_var" ("Variable "^x^"not declared")
  in
    get 0 env

let translate_sign s =
  match s with
      Signed -> N.Signed
    | Unsigned -> N.Unsigned

let translate_ityp s t =
  match t with
      Char -> N.Int (s, Config.size_of_char)
    | Int -> N.Int (s, Config.size_of_int)

let rec translate_fun_decl (b, v, loc) =
  let b = 
    match b with
	Void -> None
      | _ -> Some (translate_base_typ b) 
  in
  let rec translate b v =
    match v with
	Variable x -> 
	  Npkcontext.error "Compiler.translate_fun_decl" "unreachable code"
      | FunctionName f -> (b, f, loc)
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

and translate_decl (b, v, loc) =
  let b = translate_base_typ b in
  let rec translate b v =
    match v with
	Variable x -> (b, x, loc)
      | FunctionName x -> 
	  Npkcontext.error "Compiler.translate_fun_decl" 
	    "local function definition not allowed"
      | Array (v, n) -> 
	  let n = Some (int64_to_int n) in
	    translate (K.Array (b, n)) v
      | FunctionProto (Pointer v, _) -> translate (K.Scalar N.FunPtr) v
      | Pointer v -> translate (K.Scalar N.Ptr) v
      | FunctionProto _ -> 
	  Npkcontext.error "Compiler.translate_var_modifier" 
	    "case not implemented yet"
  in
    translate b v

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
	  let (t, _, _) = translate_decl d in
	  let sz = K.size_of t in
	  let o = align t o in
	  let (f, n) = translate (o+sz) f in
	    ((o, t)::f, n)
      | [] -> 
	  let o = align_end o in
	    ([], o)
  in
    match f with
	d::[] -> 
	  let (t, _, _) = translate_decl d in
	  let sz = K.size_of t in
	    ((0, t)::[], sz)
      | _ -> translate 0 f

and translate_union_fields f =
  let n = ref 0 in
  let translate d =
    let (t, _, _) = translate_decl d in
    let sz = K.size_of t in
      if !n < sz then n := sz;
      (0, t)
  in
    (List.map translate f, !n)

let rec translate_decl_list env x =
  match x with
      d::tl ->
	let (t, x, loc) = translate_decl d in
	let (tl, env) = translate_decl_list env tl in
	  ((x, t, loc)::tl, (x, t)::env)
    | [] -> ([], env)

let rec append_decls decls body =
  match decls with
      (x, t, loc)::tl -> 
	let (_, n,_) = loc in
	let body = append_decls tl body in
	  (K.Decl (x, t, body), loc)::[]
    | [] -> body

let rec translate_lv env lv =
  match lv with
      Var x -> 
	let (n, t) = get_var env x in
	  (K.Local n, t)

and translate_exp env e =
  match e with
      Const i -> K.Const (N.CInt64 i)

let rec translate_blk env (decls, body) =
  let (decls, env) = translate_decl_list env decls in
  let body = List.map (translate_stmt env) body in
    append_decls decls body

and translate_stmt env (x, loc) = (translate_stmtkind env x, loc)

and translate_stmtkind env x =
  match x with
    | Set (lv, e) -> 
	let (lv, t) = translate_lv env lv in
	let e = translate_exp env e in
	let t = scalar_of_t t in
	  K.Set (lv, e, t)
	    
let compile fname = 
  let globals = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in
    
  let translate_global x =
    match x with
	FunctionDef (d, body) -> 
	  let (t, f, _) = translate_fun_decl d in
	  let body = translate_blk [] body in
	    Hashtbl.add fundefs f ([], None, Some body) 

      | GlobalDecl d -> 
	  let (t, x, loc) = translate_decl d in
	    Hashtbl.add globals x (t, loc, Some None, false)
  in

  let cprog = parse fname in
    
    List.iter translate_global cprog;
    (fname, globals, fundefs)
