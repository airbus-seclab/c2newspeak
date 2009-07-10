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

module Nat = Newspeak.Nat
module  T  = Ada_types

(** Evaluate (at compile-time) an expression. *)
let eval_static (exp:Ast.expression)
                (tbl:Symboltbl.t)
   :T.data_t =

  let rec eval_static_exp (exp,t:Ast.expression)
  :T.data_t =
    match exp with
    | Ast.CInt   i -> T.IntVal(i)            
    | Ast.CFloat f -> T.FloatVal(f)          
    | Ast.CChar  c -> T.IntVal (Nat.of_int c)
    | Ast.CBool  b -> T.BoolVal b            
    | Ast.Var    v -> eval_static_const v t
    | Ast.FunctionCall _  -> raise NonStaticExpression
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

    | Ast.Qualified(_subtyp, exp) ->
        eval_static_exp exp

  (**
   * Evaluate statically a binary expression.
   * expected_typ is the expected type of the result, not the operands.
   *)
  and eval_static_binop (op:Ast.binary_op) (e1:Ast.expression) (e2:Ast.expression)
        :T.data_t =
    let val1 = eval_static_exp e1 in
    let val2 = eval_static_exp e2 in
    match (op,val1,val2) with
      (* operations sur entiers ou flottants *)
    | Ast.Plus , T.IntVal   a, T.IntVal   b -> T.IntVal (Nat.add a b)
    | Ast.Minus, T.IntVal   a, T.IntVal   b -> T.IntVal (Nat.sub a b)
    | Ast.Mult , T.IntVal   a, T.IntVal   b -> T.IntVal (Nat.mul a b)
    | Ast.Div  , T.IntVal   a, T.IntVal   b -> T.IntVal (Nat.div a b)
    | Ast.Power, T.IntVal   a, T.IntVal   b -> T.IntVal (puiss   a b)
    | Ast.Plus , T.FloatVal a, T.FloatVal b -> T.FloatVal  (a  +.  b)
    | Ast.Minus, T.FloatVal a, T.FloatVal b -> T.FloatVal  (a  -.  b)
    | Ast.Mult , T.FloatVal a, T.FloatVal b -> T.FloatVal  (a  *.  b)
    | Ast.Div  , T.FloatVal a, T.FloatVal b -> T.FloatVal  (a  /.  b)
    | Ast.Rem  , T.IntVal   a, T.IntVal   b -> T.IntVal (rem_ada a b)
    | Ast.Mod  , T.IntVal   a, T.IntVal   b -> T.IntVal (mod_ada a b)
    | Ast.Eq   ,            a,            b -> T.BoolVal(T.data_eq a b)
    | Ast.Gt   ,            a,            b -> T.BoolVal(T.data_lt b a)
    | Ast.And  , T.BoolVal  a, T.BoolVal  b -> T.BoolVal(a && b)       
    | Ast.Or   , T.BoolVal  a, T.BoolVal  b -> T.BoolVal(a || b)       
    | Ast.Power, T.FloatVal a, T.IntVal   b -> T.FloatVal(a ** (float_of_int (Nat.to_int b)))
    | _ -> Npkcontext.report_error "eval_static.binop"
                                  "invalid operator and argument"

  and eval_static_const (name:name) (expected_type:T.t)
    :T.data_t =
    try
      match snd (Symboltbl.s_find_variable_value tbl ~expected_type name) with
        | None   -> raise NonStaticExpression
        | Some b -> b
    with Symboltbl.ParameterlessFunction _ -> raise NonStaticExpression

  in
      eval_static_exp exp

(**
 * Evaluate statically an integer expression. FIXME
 *)
let eval_static_integer_exp (exp:Ast.expression)
                            (tbl:Symboltbl.t)
    :nat =
    try
      match (eval_static exp tbl) with
        | T.FloatVal _
        | T.BoolVal  _ -> Npkcontext.report_error
                    "eval_static.integer_exp"
                    "expected static integer constant"
        | T.IntVal i -> i
    with
      | NonStaticExpression -> Npkcontext.report_error
                          "eval_static.integer_exp"
                          "expected static expression"
      | AmbiguousTypeException -> Npkcontext.report_error
                          "eval_static.integer_exp"
                          "uncaught ambiguous type exception"

(**
 * Evaluate statically an constant number.
 *)
let eval_static_number (exp:Ast.expression)
                       (tbl:Symboltbl.t)
    :T.data_t =
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
       | AmbiguousTypeException -> Npkcontext.report_error
         "eval_static.integer_exp"
         "uncaught ambiguous type exception"

