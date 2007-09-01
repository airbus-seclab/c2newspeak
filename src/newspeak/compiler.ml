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
	    
let compile fname = 
  let globals = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in
  let env = Hashtbl.create 100 in
  let vcnt = ref 0 in
  let push_local x t =
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
    
  let rec translate_lv lv =
    match lv with
	Var x -> get_var x
      | Field (lv, f) ->
	  let (lv, t) = translate_lv lv in
	  let t = region_of_typ t in
	  let (o, t) = offset_of f t in
	  let o = K.Const (N.CInt64 (Int64.of_int o)) in
	    (K.Shift (lv, o), t)

  and translate_exp e =
    match e with
	Const i -> K.Const (N.CInt64 i)
      | Lval lv -> 
	  let (lv, t) = translate_lv lv in
	  let t = scalar_of_typ t in
	    K.Lval (lv, t)
  in

  let rec translate_blk (decls, body) =
    match decls with
	[] -> List.map translate_stmt body
      | d::tl ->
	  let (t, x, loc) = translate_decl d in
	    push_local x t;
	    let body = translate_blk (tl, body) in
	      pop x;
	      (K.Decl (x, t, body), loc)::[]
	      
  and translate_stmt (x, loc) = (translate_stmtkind x, loc)
    
  and translate_stmtkind x =
    match x with
      | Set (lv, e) -> 
	  let (lv, t) = translate_lv lv in
	  let e = translate_exp e in
	  let t = scalar_of_typ t in
	    K.Set (lv, e, t)
  in

let translate_global x =
    match x with
	FunctionDef (d, body) -> 
	  let (t, f, _) = translate_fun_decl d in
	  let body = translate_blk body in
	    Hashtbl.add fundefs f ([], None, Some body) 

      | GlobalDecl d -> 
	  let (t, x, loc) = translate_decl d in
	    Hashtbl.add globals x (t, loc, Some None, true)
  in

  let cprog = parse fname in
    
    List.iter translate_global cprog;
    (fname, globals, fundefs)
