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

  Olivier Levillain
  email: olivier.levillain@penjili.org
*)

(* Sanity check for Newspeak programs:
   - check that all array sizes are > 0 
     (process_length)
   - check that all shifts are necessarily of the form + int or + (e * int) 
     (process_lval)
   - check that for each assignment lv =_t e, e is of type larger or equal to t
TODO:
   - check that for all belongs, coerce l, u l <= u
   - check that all goto are enclosed within a DoWith
   - check that the size of all types is less than max_int (the sum of all??)
*)

open Newspeak

let inputs = ref []

let anon_fun file = inputs := file::!inputs

let usage_msg = "npkcheck [options] [-help|--help] file.npk"

let speclist = 
  [  ]

let subtyp t1 t2 =
  match (t1, t2) with
      (Int ik1, Int ik2) -> 
	let d1 = Newspeak.domain_of_typ ik1 in
	let d2 = Newspeak.domain_of_typ ik2 in
	  Newspeak.contains d2 d1
    | _ -> t1 = t2

let rec hastype t e =
  match (e, t) with
      (Const (CInt i), Int k) -> belongs i (domain_of_typ k)
    | (Const (CFloat _), Float _) -> true
    | (Const Nil, (Ptr|FunPtr)) -> true
    | (Lval (_, t'), _) -> subtyp t' t
    | (AddrOf _, Ptr) -> true
    | (AddrOfFun _, FunPtr) -> true
    | (UnOp ((Belongs b| Coerce b | BNot b), _), Int k) -> 
	contains (domain_of_typ k) b
    | (UnOp (Not, _), Int _) -> true
    | (UnOp (PtrToInt k', _), Int k) -> k = k'
    | (UnOp (IntToPtr _, _), Ptr) -> true
    | (UnOp (Cast (_, t'), _), _) -> t = t'
    | (BinOp ((PlusF _|MinusF _|MultF _|DivF _), _, _), Float _) -> true
    | (BinOp ((BOr b|BAnd b|BXor b), _, _), Int k) ->
	contains (domain_of_typ k) b
    | (BinOp ((Shiftlt|Shiftrt), _, _), Int _) -> true
    | (BinOp (PlusPI, _, _), Ptr) -> true
    | (BinOp (MinusPP, _, _), Int _) -> true
    | (BinOp ((Gt _|Eq _), _, _), Int _) -> true
    | (BinOp (Mod, _, _), Int _) -> true
    | _ -> false

class checker =
object (self)
  inherit Newspeak.visitor

  method process_length x =
    if x <= 0 
    then self#raise_error "invalid length for array"

  method process_lval x =
    let _ =
      match x with
	  Shift (_, BinOp (MultI, _, Const CInt _)) -> ()
	| Shift (_, Const CInt _) -> ()
	| Shift (_, _) -> self#raise_error "unexpected expression in shift"
	| _ -> ()
    in
      true

  method process_bexp x =
    let rec check x =
      match x with
	  BinOp ((Gt _|Eq _), _, _) 
	| UnOp (Not, BinOp ((Gt _|Eq _), _, _)) -> ()
	| _ -> self#raise_error "unexpected expression as guard"
    in
      check x

  method process_stmt (x, _) =
    let _ = 
      match x with
	  Set (_, e, t) ->
	    if not (hastype t e)
	    then self#raise_error ("expression of type "^(string_of_scalar t)
				    ^" expected in assignment")
	| _ -> ()
    in
      true
end

let check_file fname =
    let (_, prog, _) = Newspeak.read fname in
    let checker = new checker in
      Newspeak.visit (checker :> Newspeak.visitor) prog

let _ = 
  try
    Arg.parse speclist anon_fun usage_msg;
    List.iter check_file !inputs;
    exit 0
  with Invalid_argument s -> 
    print_endline ("Fatal error: "^s);
    exit 1

