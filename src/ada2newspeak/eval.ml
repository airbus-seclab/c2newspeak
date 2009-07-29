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

  Etienne Millon
  email: etienne.millon AT gmail . com

  Jasmine Duchon
  email : jasmine . duchon AT free . fr

*)

open Syntax_ada
open Ada_utils
open Big_int

module Nat = Newspeak.Nat
module  T  = Ada_types

exception NonStaticExpression

let (^%) a b =
  let a = Nat.to_big_int a
  and b = Nat.to_big_int b in
    if (Big_int.sign_big_int b)<0
    then begin
      Npkcontext.report_error "puiss"
        "integer exponent negative"
    end else Nat.of_big_int (Big_int.power_big_int_positive_big_int a b)

let mod_rem_aux ~is_mod na nb =
  let a = Nat.to_big_int na in
  let b = Nat.to_big_int nb in
  let r_mod =  mod_big_int a b in
  Nat.of_big_int (if (sign_big_int (if is_mod then b else a)) > 0
                  then
                    r_mod
                  else
                    sub_big_int r_mod (abs_big_int b)
                 )

let (%%) = mod_rem_aux ~is_mod:false (* rem *)
let (%:) = mod_rem_aux ~is_mod:true  (* mod *)

(** Evaluate (at compile-time) an expression. *)
let eval_static exp tbl =
  let rec eval_static_exp (exp,t) =
    match exp with
    | Ast.CInt   i -> T.IntVal(i)
    | Ast.CFloat f -> T.FloatVal(f)
    | Ast.CChar  c -> T.IntVal (Nat.of_int c)
    | Ast.CBool  b -> T.BoolVal b
    | Ast.Var (s,v,_) -> eval_static_const (s,v) t
    | Ast.Not exp -> begin
                       match (eval_static_exp exp) with
                        | T.BoolVal(b) -> T.BoolVal(not b)
                        | _ -> Npkcontext.report_error "eval_static_exp"
                                 "Unexpected unary operator and argument"
                     end
    | Ast.Binary(op,e1,e2) -> eval_static_binop op e1 e2
    | Ast.CondExp(cond,iftrue,iffalse) ->
        begin
            match eval_static_exp cond with
            | T.BoolVal true  -> eval_static_exp iftrue
            | T.BoolVal false -> eval_static_exp iffalse
            | _ -> Npkcontext.report_error "eval_static.exp"
                   "unexpected type for conditional expression"
        end
    | Ast.ArrayValue   _
    | Ast.FunctionCall _
    | Ast.RecordValue  _
    | Ast.AddressOf    _ -> raise NonStaticExpression

  (**
   * Evaluate statically a binary expression.
   * expected_typ is the expected type of the result, not the operands.
   *)
  and eval_static_binop op e1 e2 =
    let val1 = eval_static_exp e1 in
    let val2 = eval_static_exp e2 in
    match (op,val1,val2) with
      (* operations sur entiers ou flottants *)
    | Ast.Plus , T.IntVal   a, T.IntVal   b -> T.IntVal (Nat.add a b)
    | Ast.Minus, T.IntVal   a, T.IntVal   b -> T.IntVal (Nat.sub a b)
    | Ast.Mult , T.IntVal   a, T.IntVal   b -> T.IntVal (Nat.mul a b)
    | Ast.Div  , T.IntVal   a, T.IntVal   b -> T.IntVal (Nat.div a b)
    | Ast.Power, T.IntVal   a, T.IntVal   b -> T.IntVal   (a ^% b)
    | Ast.Plus , T.FloatVal a, T.FloatVal b -> T.FloatVal (a +. b)
    | Ast.Minus, T.FloatVal a, T.FloatVal b -> T.FloatVal (a -. b)
    | Ast.Mult , T.FloatVal a, T.FloatVal b -> T.FloatVal (a *. b)
    | Ast.Div  , T.FloatVal a, T.FloatVal b -> T.FloatVal (a /. b)
    | Ast.Rem  , T.IntVal   a, T.IntVal   b -> T.IntVal   (a %% b)
    | Ast.Mod  , T.IntVal   a, T.IntVal   b -> T.IntVal   (a %: b)
    | Ast.Eq   ,            a,            b -> T.BoolVal(T.data_eq a b)
    | Ast.Gt   ,            a,            b -> T.BoolVal(T.data_lt b a)
    | Ast.And  , T.BoolVal  a, T.BoolVal  b -> T.BoolVal  (a && b)
    | Ast.Or   , T.BoolVal  a, T.BoolVal  b -> T.BoolVal  (a || b)
    | Ast.Power, T.FloatVal a, T.IntVal   b -> T.FloatVal
                                  (a ** (float_of_int (Nat.to_int b)))
    | _ -> Npkcontext.report_error "eval_static.binop"
                                  "invalid operator and argument"

  and eval_static_const name expected_type =
    try
      let convert_scope = function
        | Symboltbl.Lexical -> None
        | Symboltbl.In_package p -> Some p
      in
      let (package,id) = name in
      let (_,(_,r,_)) = Symboltbl.find_variable_value tbl ~expected_type
                                              (convert_scope package,id) in
      match r with
        | None   -> raise NonStaticExpression
        | Some b -> b
    with
      | Symboltbl.Parameterless_function _ -> raise NonStaticExpression
      | Symboltbl.Variable_no_storage (_,data) -> data

  in
      eval_static_exp exp

(**
 * Evaluate statically an constant number.
 *)
let eval_static_number exp tbl =
     try
         let v = eval_static exp tbl in
             match v with
               | T.BoolVal _ -> Npkcontext.report_error
                     "eval_static.integer_exp"
                     "expected static float or integer constant"
               | T.FloatVal _ | T.IntVal _ -> v
     with
       | NonStaticExpression -> Npkcontext.report_error
          "eval_static.integer_exp"
          "expected static expression"

