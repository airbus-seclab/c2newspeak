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

type t = ((string, string) Hashtbl.t * stmt list)

and stmt = (exp * exp)

and exp =
    Const
  | Var of string
  | Deref of exp

let rec string_of_exp e =
  match e with
      Const -> "Cte"
    | Var x -> x
    | Deref e -> "["^(string_of_exp e)^"]"

let string_of_stmt (e1, e2) =
  let s1 = string_of_exp e1 in
  let s2 = string_of_exp e2 in
    s1^" = "^s2

let string_of_var x y = print_endline (x^" is "^y)

let print (vars, stmts) =
  print_endline "variables: ";
  Hashtbl.iter string_of_var vars;
  print_endline "instructions: ";
  List.iter (fun x -> print_endline (string_of_stmt x)) stmts
