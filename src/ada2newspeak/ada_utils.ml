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


open AdaSyntax
module Nat = Newspeak.Nat
module  T  = AdaTypes

let nat_of_bool b =
  if b then Newspeak.Nat.one
       else Newspeak.Nat.zero

let check_compil_unit_name compil_unit file_name =
  let expected_name = Filename.chop_extension file_name in
  let (_,lib_item,_) = compil_unit in
  let name =
    match lib_item with
      | Spec(SubprogramSpec(Subprogram (name,_,_)))     -> name
      | Body(SubprogramBody(Subprogram (name,_,_),_,_)) -> name
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
  let op_str = match op with
   | And| AndThen -> "and" | Or | OrElse  -> "or"
   | Xor          -> "xor" | Eq           -> "="
   | Neq          -> "/="  | Lt           -> "<"
   | Le           -> "<="  | Gt           -> ">"
   | Ge           -> ">="  | Plus         -> "+"
   | Minus        -> "-"   | Mult         -> "*"
   | Div          -> "/"   | Mod          -> "mod"
   | Rem          -> "rem" | Power        -> "**"
  in
  Temps.to_string 0 (Temps.Ada_operator op_str)

let operator_of_string s =
  begin match s with
  | "and" | "or"  | "xor" | "="
  | "/="  | "<"   | "<="  | ">"
  | ">="  | "+"   | "-"   | "*"
  | "/"   | "mod" | "rem" | "**"  -> ()
  |_ -> Npkcontext.report_error "operator_of_string"
         ("\"" ^ s ^ "\" does not name an operator")
  end;
  Temps.to_string 0 (Temps.Ada_operator s)

let may f = function
  | None   -> None
  | Some v -> Some (f v)
