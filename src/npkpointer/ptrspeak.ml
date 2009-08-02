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

type prog = (vars * t)

and t = (fundecs * stmt list)

and vars = (string, string) Hashtbl.t

and fundecs = (Newspeak.fid, string list) Hashtbl.t

and stmt = 
    Set of (exp * exp)
  | Call of (exp * string list)

and exp =
    Const
  | Var of string
  | Deref of exp

let rec string_of_exp e =
  match e with
      Const -> "Cte"
    | Var x -> x
    | Deref e -> "["^(string_of_exp e)^"]"

let string_of_fun f params =
  let params = ListUtils.to_string (fun x -> x) ", " params in
    f^"("^params^")"

let string_of_stmt x =
  match x with
      Set (e1, e2) ->
	let s1 = string_of_exp e1 in
	let s2 = string_of_exp e2 in
	  s1^" = "^s2
    | Call (f, params) -> 
	let f = string_of_exp f in
	  string_of_fun f params

let string_of_var x y = x^" is "^y

let print (vars, (fundecs, stmts)) =
  print_endline "variables: ";
  Hashtbl.iter (fun x y -> print_endline (string_of_var x y)) vars;
  print_endline "function arguments: ";
  Hashtbl.iter (fun x y -> print_endline (string_of_fun x y)) fundecs;
  print_endline "instructions: ";
  List.iter (fun x -> print_endline (string_of_stmt x)) stmts
