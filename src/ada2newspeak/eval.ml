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

module Nat = Newspeak.Nat
module  T  = AdaTypes

open Ast

exception NonStaticExpression

let (^%) a b =
  let a = Nat.to_big_int a
  and b = Nat.to_big_int b in
    if EBigInt.sign_big_int b < 0
    then begin
      Npkcontext.report_error "Eval"
        "Integer exponents should be strictly positive."
    end else Nat.of_big_int (EBigInt.power_big_int_positive_big_int a b)

let mod_rem_aux ~is_mod na nb =
  let module BI = EBigInt in
  let a = Nat.to_big_int na in
  let b = Nat.to_big_int nb in
  let r_mod = BI.mod_big_int a b in
  Nat.of_big_int (if (BI.sign_big_int (if is_mod then b else a)) > 0
                  then
                    r_mod
                  else
                    BI.sub_big_int r_mod (BI.abs_big_int b)
                 )

let (%%) = mod_rem_aux ~is_mod:false (* rem *)
let (%:) = mod_rem_aux ~is_mod:true  (* mod *)

(** Evaluate (at compile-time) an expression. *)
let eval_static exp tbl =
  let rec eval_static_exp (exp,t) =
    match exp with
    | CInt   i -> T.IntVal(i)
    | CFloat f -> T.FloatVal(f)
    | CChar  c -> T.IntVal (Nat.of_int c)
    | CBool  b -> T.BoolVal b
    | Lval (Var (s,v,_)) -> eval_static_const (s,v) t
    | Not exp -> begin
                   match (eval_static_exp exp) with
                    | T.BoolVal(b) -> T.BoolVal(not b)
                    | _ -> Npkcontext.report_error "eval_static_exp"
                             "Unexpected unary operator and argument"
                 end
    | Binary(op,e1,e2) -> eval_static_binop op e1 e2
    | CondExp(cond,iftrue,iffalse) ->
        begin
            match eval_static_exp cond with
            | T.BoolVal true  -> eval_static_exp iftrue
            | T.BoolVal false -> eval_static_exp iffalse
            | _ -> Npkcontext.report_error "eval_static.exp"
                   "unexpected type for conditional expression"
        end

    | Cast (old_typ , n_typ, e) ->  
	let res = eval_static_exp (e, old_typ) in
	let make_cast r new_type = 
	  match r with 
	      T.IntVal i -> 
		if (T.is_float new_type) then begin
		  Npkcontext.report_warning "Eval eval_static"
		    "UNSOUND static evaluation of cast";
		  T.FloatVal (float (Newspeak.Nat.to_int i))
		end 
		else if (T.is_integer new_type) then T.IntVal  i
		else begin  
		  Npkcontext.report_warning "Eval eval_static"
		    "Unforeseen case of cast ";
		  Npkcontext.report_error "eval_static.exp"
               "unhandled cast case"
		end
	    |_ ->  begin  
		  Npkcontext.report_warning "Eval eval_static"
		    "Casting T.IntVal expected to do... ->  "; 
		 Npkcontext.report_error "eval_static.exp"
               "unhandled cast case" 
	       end
	in
	  make_cast res n_typ 
    | Lval         _
    | FunctionCall _
    | AddressOf    _ 
    | BlkExp  _        -> raise NonStaticExpression

  (**
   * Evaluate statically a binary expression.
   * expected_typ is the expected type of the result, not the operands.
   *)
  and eval_static_binop op e1 e2 =
    let val1 = eval_static_exp e1 in
    let val2 = eval_static_exp e2 in
      match (op,val1,val2) with
      | Plus , T.IntVal   a, T.IntVal   b -> T.IntVal (Nat.add a b)
      | Minus, T.IntVal   a, T.IntVal   b -> T.IntVal (Nat.sub a b)
      | Mult , T.IntVal   a, T.IntVal   b -> T.IntVal (Nat.mul a b)
      | Div  , T.IntVal   a, T.IntVal   b -> T.IntVal (Nat.div a b)
      | Power, T.IntVal   a, T.IntVal   b -> T.IntVal   (a ^% b)
      | Plus , T.FloatVal a, T.FloatVal b -> T.FloatVal (a +. b)
      | Minus, T.FloatVal a, T.FloatVal b -> T.FloatVal (a -. b)
      | Mult , T.FloatVal a, T.FloatVal b -> T.FloatVal (a *. b)
      | Div  , T.FloatVal a, T.FloatVal b -> T.FloatVal (a /. b)
      | Rem  , T.IntVal   a, T.IntVal   b -> T.IntVal   (a %% b)
      | Mod  , T.IntVal   a, T.IntVal   b -> T.IntVal   (a %: b)
      | Eq   ,            a,            b -> T.BoolVal (T.data_compare a b = 0)
      | Gt   ,            a,            b -> T.BoolVal (T.data_compare a b > 0)
      | And  , T.BoolVal  a, T.BoolVal  b -> T.BoolVal  (a && b)
      | Or   , T.BoolVal  a, T.BoolVal  b -> T.BoolVal  (a || b)
      | Power, T.FloatVal a, T.IntVal   b -> T.FloatVal
          (a ** (float_of_int (Nat.to_int b)))
      | _ -> 
	  Npkcontext.report_error "eval_static.binop"
            "invalid operator and argument"
	    
  and eval_static_const name expected_type =
    try
      let convert_scope = function
        | Symboltbl.Lexical -> None
        | Symboltbl.In_package p -> Some p
      in
      let (package,id) = name in
      let (_, (_,_,r,_)) = 
	Symboltbl.find_variable_value tbl ~expected_type 
	  (convert_scope package,id)
      in
      match r with
        | None   -> raise NonStaticExpression
        | Some b -> b
    with
      | Symboltbl.Parameterless_function _ -> raise NonStaticExpression
      | Symboltbl.Variable_no_storage (_,data) -> data

  in
      eval_static_exp exp

