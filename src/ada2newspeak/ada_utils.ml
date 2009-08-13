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

  Jasmine Duchon
  email : jasmine . duchon AT free . fr

*)


open Syntax_ada
module Nat = Newspeak.Nat
module  T  = Ada_types

let nat_of_bool b =
  if b then Newspeak.Nat.one
       else Newspeak.Nat.zero

let check_compil_unit_name compil_unit file_name =
  let expected_name = Filename.chop_extension file_name in
  let subprog_name spec = match spec with
    | Function(name,_,_) -> name
    | Procedure(name,_) -> name in
  let (_,lib_item,_) = compil_unit in
  let name =
    match lib_item with
      | Spec(SubProgramSpec(spec))     -> subprog_name spec
      | Body(SubProgramBody(spec,_,_)) -> subprog_name spec
      | Spec(PackageSpec(name,_))   -> name
      | Body(PackageBody(name,_,_)) -> name
  in
    name = expected_name

let with_default (opt:'a option) (def_value:'a):'a = match opt with
    | None   -> def_value
    | Some x -> x

let name_to_string =
  String.concat "."

let make_operator_name op =
  let ada2npk_operator_prefix = "!op_" in
  ada2npk_operator_prefix ^
   (match op with
   | And| AndThen -> "and" | Or | OrElse  -> "or"
   | Xor          -> "xor" | Eq           -> "="
   | Neq          -> "/="  | Lt           -> "<"
   | Le           -> "<="  | Gt           -> ">"
   | Ge           -> ">="  | Plus         -> "+"
   | Minus        -> "-"   | Mult         -> "*"
   | Div          -> "/iv" | Mod          -> "mod"
   | Rem          -> "rem" | Power        -> "**"
   )

let operator_of_string s =
  begin match s with
  | "and" | "or"  | "xor" | "="   
  | "/="  | "<"   | "<="  | ">"   
  | ">="  | "+"   | "-"   | "*"   
  | "/"   | "mod" | "rem" | "**"  -> ()
  |_ -> Npkcontext.report_error "operator_of_string"
         ("\"" ^ s ^ "\" does not name an operator")
  end;
  "!op_"^s

let may f = function
  | None   -> None
  | Some v -> Some (f v)

type progress =
  | Parsing   of string 
  | Semcheck  of string 
  | Translate of string 
  | Post
  | Done of progress

let log_progress p =
  let rec s_prog = function
  | Parsing   f ->   ("Parsing<"^f^">")
  | Semcheck  f ->  ("Semcheck<"^f^">")
  | Translate f -> ("Translate<"^f^">")
  | Post        -> ("Post")
  | Done p      -> ("Done<"^s_prog p^">")
  in
  Npkcontext.print_debug ("###PROGRESS### "^s_prog p)
