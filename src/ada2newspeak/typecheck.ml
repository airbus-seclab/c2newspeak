(*
  Ada2Newspeak: compiles Ada code into Newspeak. Newspeak is a minimal language
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

  Etienne Millon
  email: etienne.millon AT gmail . com

*)

open Ast

module T = AdaTypes

let error = Npkcontext.report_error "Typecheck"

let expect ?desc t1 t2 =
  if (T.is_unknown t1 || T.is_unknown t2) then
    error "Unknown type";
  if (not (T.is_compatible t1 t2))
  then (* is_compatible ? *)
    error ("incompatible types"
          ^ (match desc with
             | None -> ""
             | Some s -> " in '" ^ s ^ "'"
            )
          ^ "\nL = "
          ^ T.print t1
          ^ "\nR = "
          ^ T.print t2
          )

let t_assert x msg =
  if not x then
    error ("Type assertion failed : " ^ msg)

let type_of_binop op t1 t2 = match op with
  | Eq    -> expect ~desc:"binary =" t1 t2;
             T.boolean
  | Or | And -> expect ~desc:"binary logical op" t1 t2;
                if not (T.is_boolean t1) then
                 error "Logical operator is not defined -- 4.5.1.(2)";
                t1
  | Gt    -> expect ~desc: "binary >" t1 t2;
             if not (T.is_scalar t1) then
               error "Ordering operator is not defined -- 4.5.2.(8)";
             T.boolean
  | Plus | Minus -> expect ~desc:"binary adding" t1 t2;
      if not (T.is_numeric t1) then
        error "Binary adding operator is not defined -- 4.5.3.(1)";
                  (*print_endline "-------------binary minus/  adding ";
		    print_endline (T.print  (T.coerce_types t1 t2)); *)
      T.coerce_types t1 t2
	
  | Mult | Div -> expect ~desc:"binary multiplying" t1 t2;
      if not (T.is_float t1 || T.is_integer t1) then 
        error "Multiplying operator is not defined -- 4.5.5.(2)";
      T.coerce_types t1 t2

  | Rem  | Mod -> expect ~desc:"binary Rem/Mod" t1 t2;
      if not (T.is_integer t1) then
        error "Multiplying operator is not defined -- 4.5.5.(2)";
      t1
  | Power -> expect t2 T.integer;
             if not (T.is_integer t1 || T.is_float t1) then
              error "Highest precedence operator \"**\" is not defined -- 4.5.6";
             t1

let type_of_not t =
  if not (T.is_boolean t) then
    error "Unary operator \"not\" is not defined -- 4.5.6"
  ;
  t

let type_of_xor t1 t2 =
  if not (T.is_boolean t1 && T.is_boolean t2) then
    error "Xor is not defined -- 4.5.1.(2)";
  expect ~desc:"xor" t1 t2;
  t1

let type_of_abs t =
  if not (T.is_numeric t) then
   error "Highest precedence operator 'abs' is not defined -- 4.5.6";
  t

let type_of_uplus t =
  if not (T.is_numeric t) then
   error "Unary '+' is not defined -- 4.5.4";
  t
