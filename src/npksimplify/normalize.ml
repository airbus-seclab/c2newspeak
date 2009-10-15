(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans
  
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

(* TODO: a more structured syntax for expressions ?
   exp := Bexp bexp
          Aexp aexp

   bexp := !bexp | bexp ^ bexp | bexp v bexp | aexp == aexp | aexp > aexp
   aexp := everything else
*)

open Newspeak

let is_bexp e =
  match e with
      UnOp (Not, _) | BinOp (Eq _, _, _) | BinOp (Gt _, _, _) -> true
    | _ -> false

let process_unop op e = 
  match (op, e) with
      (Not, UnOp (Not, e)) -> e
    | (Not, Const CInt c) ->
	if Nat.compare c Nat.zero = 0 then Newspeak.one else Newspeak.zero
    | _ -> UnOp (op, e)

let rec process_binop op e1 e2 =
  match (op, e1, e2) with
      (Eq Int _, e1, e2) when e1 > e2 -> process_binop op e2 e1
    | (Eq Int _, Const CInt c, e) when is_bexp e ->
	if Nat.compare c Nat.zero = 0 then Newspeak.negate e else e
    | (Gt Int _, Const CInt c1, Const CInt c2) ->
	if Nat.compare c1 c2 > 0 then Newspeak.one else Newspeak.zero
    | (Eq Int _, Const CInt c1, Const CInt c2) ->
	if Nat.compare c1 c2 = 0 then Newspeak.one else Newspeak.zero
    | _ -> BinOp (op, e1, e2)

let rec process_lval lv = 
  match lv with
      Local _ | Global _ -> lv
    | Deref (e, n) -> Deref (process_exp e, n)
    | Shift (lv, e) -> Shift (process_lval lv, process_exp e)

and process_exp e =
  match e with
      Const _ | AddrOfFun _ -> e
    | Lval (lv, t) -> Lval (process_lval lv, t)
    | AddrOf lv -> AddrOf (process_lval lv)
    | UnOp (op, e) -> 
	let e = process_exp e in
	  process_unop op e
    | BinOp (op, e1, e2) ->
	let e1 = process_exp e1 in
	let e2 = process_exp e2 in
	  process_binop op e1 e2
