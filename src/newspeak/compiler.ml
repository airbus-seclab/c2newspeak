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

module N = Newspeak
module K = Npkil

(* TODO: check this for various architecture ? 
   Here align everything on 4 *)
let align t o = 
  if (o mod 4) = 0 then o
  else if K.size_of t <= (4 - (o mod 4)) then o
  else o + (4 - (o mod 4))

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

let translate_var_modifier b v =
  let rec translate b v =
    match v with
	Variable x -> (b, x)
      | Array (v, n) -> 
	  let n = Some (int64_to_int n) in
	    translate (K.Array (b, n)) v
(*	  let (t, x) = translate v in
	    (K.Array (t, n), x)
*)
      | Pointer v -> translate (K.Scalar N.Ptr) v
(*	  let (_, x) = translate v in
	  (K.Scalar N.Ptr, x)
*)
  in
    translate b v

let rec translate_decl (b, v) =
  let b = translate_base_typ b in
    translate_var_modifier b v

and translate_base_typ t =
  match t with
      Integer (s, t) -> K.Scalar (translate_ityp (translate_sign s) t)
    | Struct f -> K.Region (translate_fields f)

and translate_fields f =
  let rec translate o f =
    match f with
	(b, v)::f -> 
	  let (t, _) = translate_decl (b, v) in
	  let sz = K.size_of t in
	  let o = align t o in
	  let (f, n) = translate (o+sz) f in
	    ((o, t)::f, n+sz)
      | [] -> ([], 0)
  in
    translate 0 f

let rec translate_lv env lv =
  match lv with
      Var x -> 
	let (n, t) = get_var env x in
	  (K.Local n, t)

and translate_exp env e =
  match e with
      Const i -> K.Const (N.CInt64 i)

let rec translate_blk env x =
  match x with
    | hd::tl -> (translate_stmt env hd)::(translate_blk env tl)
    | [] -> []

and translate_stmt env (x, loc) = (translate_stmtkind env x, loc)

and translate_stmtkind env x =
  match x with
      Decl ((b, v), body) ->
	let (t, x) = translate_decl (b, v) in
	let body = translate_blk ((x, t)::env) body in
	  K.Decl (x, t, body)

    | Set (lv, e) -> 
	let (lv, t) = translate_lv env lv in
	let e = translate_exp env e in
	  let t = scalar_of_t t in
	    K.Set (lv, e, t)

let compile fname = 
  let globals = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in

  let translate_fundec (f, body) = 
    let body = translate_blk [] body in
      Hashtbl.add fundefs f ([], None, Some body) 
  in

  let translate_global x = translate_fundec x in

  let cprog = parse fname in
    
    List.iter translate_global cprog;
    (fname, globals, fundefs)
