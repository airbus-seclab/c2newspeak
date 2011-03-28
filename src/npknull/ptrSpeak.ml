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

open Newspeak

(* TODO: maybe should have left value and expressions!!! *)

type exp = 
    Empty
(* TODO: should have just Variable and the translate uses the environment
   information!!! *)
  | LocalVar of string
  | GlobalVar of string
  | Access of exp
  | Shift of exp
  | Join of (exp * exp)

type stmt = 
  | Set of (exp * exp)
  | Guard of exp
  | Select of (blk * blk)
  | InfLoop of blk
  | DoWith of (blk * int)
  | Goto of int
  | Call of (exp list * string * exp list)

and blk = (stmt * Newspeak.location) list

type formula = 
    AreNotEqual of (exp * exp)
  | IsNotNull of exp

let join v1 v2 =
  if (v1 = Empty) then v2
  else if (v2 = Empty) then v1
  else Join (v1, v2)

let rec translate_lval lv =
  match lv with
      Local x -> LocalVar x
    | Global x -> GlobalVar x
    | Deref (e, _) -> Access (translate_exp_under_deref e)
(* TODO: not nice, translation should be in a different file than language 
   definition *)
    | Newspeak.Shift (lv, _) -> Shift (translate_lval lv)

and translate_exp e = 
  match e with
      Const _ | AddrOfFun _ -> Empty
    | Lval (lv, _) -> Access (translate_lval lv)
    | AddrOf lv -> translate_lval lv
    | UnOp (_, e) -> translate_exp e
    | BinOp (PlusPI, e, _) -> Shift (translate_exp e)
    | BinOp (_, e1, e2) -> join (translate_exp e1) (translate_exp e2)

and translate_exp_under_deref e = 
  match e with
      Const _ | AddrOfFun _ -> Empty
    | Lval (lv, _) -> translate_lval lv
    | AddrOf lv -> translate_lval lv (* TODO: this case may be incorrect *)
    | UnOp (_, e) -> translate_exp_under_deref e
    | BinOp (PlusPI, e, _) -> Shift (translate_exp_under_deref e)
    | BinOp (_, e1, e2) -> 
	join (translate_exp_under_deref e1) (translate_exp e2)

let rec to_string e =
  match e with
      Empty -> "{}"
    | LocalVar x -> "local("^x^")"
    | GlobalVar x -> "global("^x^")"
    | Access e -> "*("^to_string e^")"
    | Shift e -> "("^to_string e^" + ?)"
    | Join (e1, e2) -> "("^to_string e1^" | "^to_string e2^")"

let test1 () =
  print_string "PtrSpeak.test1...";
  let var = "p" in
  let e = 
    Lval (Deref (Lval (Local var, Scalar Ptr), 32),
	  Scalar (Int (Signed, 32)))
  in
  let e = translate_exp e in
  let expected = Access (Access (LocalVar var)) in
    if (e <> expected) then invalid_arg "failed";
    print_endline "OK"

let test2 () =
  print_string "PtrSpeak.test2...";
  let lv = Lval (Global "g", Scalar Ptr) in
  let e1 = translate_exp_under_deref lv in
  let e2 = BinOp (PlusPI, lv, exp_of_int 32) in
  let e2 = translate_exp_under_deref e2 in
    if (e1 <> e2) 
    then invalid_arg ("expected: "^to_string e1^", got: "^to_string e2);
    print_endline "OK"

let test3 () =
  print_string "PtrSpeak.test3...";
  let lv = Lval (Global "g", Scalar Ptr) in
  let e1 = Deref (lv, 32) in
  let e1 = translate_lval e1 in
  let e2 = Deref (BinOp (PlusPI, lv, exp_of_int 32), 32) in
  let e2 = translate_lval e2 in
    if (e1 <> e2) 
    then invalid_arg ("expected: "^to_string e1^", got: "^to_string e2);
    print_endline "OK"

let test () =
  test1 ();
  test2 ()
