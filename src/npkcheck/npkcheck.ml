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
   - check that all array sizes are >= 0 
     (process_length)
   - check that all shifts are necessarily of the form + int or + (e * int) 
     (process_lval)
   - check that for each assignment lv =_t e, e is of type larger or equal to t
TODO:
   - check that for all belongs, coerce l, u l <= u
   - check that all goto are enclosed within a DoWith
   - check that the size of all types is less than max_int (the sum of all??)
*)

open Lowspeak
module N = Newspeak

let inputs = ref []

let anon_fun file = inputs := file::!inputs

let usage_msg = "npkcheck [options] [-help|--help] file.npk"

let speclist = 
  [  ]

let subtyp t1 t2 =
  match (t1, t2) with
      (N.Int ik1, N.Int ik2) -> 
	let d1 = Newspeak.domain_of_typ ik1 in
	let d2 = Newspeak.domain_of_typ ik2 in
	  Newspeak.contains d2 d1
    | _ -> t1 = t2

let rec hastype t e =
  match (e, t) with
      (Const (N.CInt i),   N.Int k) -> N.belongs i (N.domain_of_typ k)
    | (Const (N.CFloat _), N.Float _) -> true
    | (Const N.Nil, (N.Ptr|N.FunPtr)) -> true
    | (Lval (_, t'), _) -> subtyp t' t
    | (AddrOf _, N.Ptr) -> true
    | (AddrOfFun _, N.FunPtr) -> true
    | (UnOp ((N.Belongs b| N.Coerce b | N.BNot b), _), N.Int k) -> 
	N.contains (N.domain_of_typ k) b
    | (UnOp (N.Not, _), N.Int _) -> true
    | (UnOp (N.PtrToInt k', _), N.Int k) -> k = k'
    | (UnOp (N.IntToPtr _, _), N.Ptr) -> true
    | (UnOp (N.Cast (_, t'), _), _) -> t = t'
    | (BinOp ((N.PlusF _|N.MinusF _
              |N.MultF _|N.DivF _), _, _), N.Float _) -> true
    | (BinOp ((N.BOr b|N.BAnd b|N.BXor b), _, _), N.Int k) ->
	N.contains (N.domain_of_typ k) b
    | (BinOp ((N.Shiftlt|N.Shiftrt), _, _), N.Int _) -> true
    | (BinOp (N.PlusPI, _, _), N.Ptr) -> true
    | (BinOp (N.MinusPP, _, _), N.Int _) -> true
    | (BinOp ((N.Gt _|N.Eq _), _, _), N.Int _) -> true
    | (BinOp (N.Mod, _, _), N.Int _) -> true
    | _ -> false

class checker ptr_sz =
object (self)
  inherit Lowspeak.visitor

  method process_length x =
    if x < 0 
    then self#raise_error "invalid length for array"

  method process_lval x =
    let _ =
      match x with
	  Shift (_, BinOp (N.MultI, _, Const N.CInt _)) -> ()
	| Shift (_, Const N.CInt _) -> ()
	| Shift (_, _) -> self#raise_error "unexpected expression in shift"
	| _ -> ()
    in
      true

  method process_bexp x =
    let rec check x =
      match x with
	  BinOp ((N.Gt _|N.Eq _), _, _) 
	| UnOp (N.Not, BinOp ((N.Gt _|N.Eq _), _, _)) -> ()
	| _ -> self#raise_error "unexpected expression as guard"
    in
      check x

  method process_stmt (x, _) =
    let _ = 
      match x with
	  Set (_, e, t) ->
	    if not (hastype t e)
	    then self#raise_error ("expression of type "^(N.string_of_scalar t)
				    ^" expected in assignment")
	| _ -> ()
    in
      true

  method process_typ t =
    let rec check_size t =
      match t with
	  N.Scalar t -> Newspeak.size_of_scalar ptr_sz t
	| N.Array (t, len) -> 
	    let n = check_size t in
	      if len <> 0 && n > max_int/len then begin
		self#raise_error ("size of type not representable as an int")
	      end;
	      n * len
	| N.Region (fields, n) -> 
	    List.iter (fun (_, t) -> ignore (check_size t)) fields;
	    n
    in
    let _ = check_size t in
      ()
end

let check_file fname =
    let prog = Npk2lpk.translate (Newspeak.read fname) in
    let checker = new checker prog.Lowspeak.ptr_sz in
      Lowspeak.visit (checker :> Lowspeak.visitor) prog

let process () = List.iter check_file !inputs

let _ = 
  StandardApplication.launch speclist anon_fun usage_msg process

