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

let translate_sign s =
  match s with
      Signed -> N.Signed
    | Unsigned -> N.Unsigned

let translate_ityp s t =
  match t with
      Char -> N.Int (s, Config.size_of_char)
    | Int -> N.Int (s, Config.size_of_int)

let translate_typ t =
  match t with
      Integer (s, t) -> K.Scalar (translate_ityp (translate_sign s) t)
    | Pointer _ -> K.Scalar N.Ptr

let get_var env x =
  let rec get n env =
    match env with
      | (y, t)::_ when y = x -> (n, t)
      | _::tl -> get (n+1) tl
      | [] -> 
	  Npkcontext.error "Compiler.get_var" ("Variable "^x^"not declared")
  in
    get 0 env

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
      Decl (x, t, body) ->
	let t = translate_typ t in
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
