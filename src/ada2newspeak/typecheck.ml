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

module T = Ada_types

let tc_verb = Ada_utils.Debug

let error = Ada_utils.mkerror tc_verb "Typecheck"

let expect t t1 =
  if t <> t1 then (* is_compatible ? *)
    error "incompatible types?"

let t_assert x msg =
  if (not x) then
    error ("Type assertion failed : "^msg)

let type_of_binop op t1 t2 = match op with
  | Eq    -> expect t1 t2;
             T.boolean
  | Or | And -> expect t1 t2;
                t_assert (T.is_boolean t1)
                  "Logical operator is not defined -- 4.5.1.(2)";
                t1
  | Gt    -> expect t1 t2;
             t_assert (T.is_scalar t1)
               "Ordering operator is not defined -- 4.5.2.(8)";
             T.boolean
  | Plus | Minus -> expect t1 t2;
                    t_assert (T.is_numeric t1)
                           "Binary adding operator is not defined -- 4.5.3.(1)"
                    ;
                    t1
  | Mult | Div
  | Rem  | Mod -> expect t1 t2;
                  (* and more cases : integer TIMES float, etc *)
                  t_assert (T.is_integer t1) (* and is_specific *)
                    "Multiplying operator is not defined -- 4.5.5.(2)";
                  t1
  | Power -> expect t2 T.natural;
             t_assert (T.is_integer t1) (* or float *)
               "Highest precedence operator \"**\" is not defined -- 4.5.6";
             t1

let type_of_unop op t = match op with
  | UPlus | UMinus | Abs -> t_assert (T.is_numeric t)
                        "Unary adding operator is not defined -- 4.5.4, 4.5.6";
                      t
  | Not            -> t_assert (T.is_boolean t)
                        "Unary adding operator \"not\" is not defined -- 4.5.6"
                      ;
                      t

