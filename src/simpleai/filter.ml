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

module Nat = Newspeak.Nat
module S = Simple

let int_bounds = Newspeak.domain_of_typ (Signed, 32)

let process_scalar_t t =
  match t with
      Int (Signed, 32) -> ()
    | _ -> invalid_arg "Filter.process_scalar_t: int scalar type expected"

let process_typ t =
  match t with
      Scalar t -> process_scalar_t t
    | _ -> invalid_arg "Filter.process_typ: scalar type expected"

let process_ftyp (args, ret) =
  if ret <> None then begin
    invalid_arg "Filter.process_ftyp: function without return value expected"
  end;
  if args <> [] then begin
    invalid_arg "Filter.process_ftyp: function with no parameter expected"
  end

let process_const c =
  match c with
      CInt n -> 
	if not (Newspeak.belongs n int_bounds) then begin
	  invalid_arg ("Filter.process_const: "
		       ^"integer not representable as a 32 bits integer")
	end;
	S.CInt (Int32.of_string (Nat.to_string n))
    | _ -> invalid_arg "Filter.process_const: integer constant expected"

let process_bounds (l, u) = 
  if not (Newspeak.contains int_bounds (l, u)) then begin
    invalid_arg ("Filter.process_bounds: "
		 ^"bounds not representable as 32 bits integer")
  end;
  (Int32.of_string (Nat.to_string l), Int32.of_string (Nat.to_string u))

let process_unop op =
  match op with
      Coerce bounds -> S.Coerce (process_bounds bounds)
    | _ -> invalid_arg "Filter.process_unop: coercion expected"
    
let process_binop op =
  match op with
      PlusI -> S.PlusI
    | MinusI -> S.MinusI
    | MultI -> S.MultI
    | DivI -> S.DivI
    | Mod -> S.Mod
    | Gt t -> process_scalar_t t; S.Gt
    | Eq t -> process_scalar_t t; S.Eq
    | _ -> invalid_arg "Filter.process_binop: integer binary operation expected"

let rec process_lval lv =
  match lv with
      Global x -> S.Global x
    | _ -> invalid_arg "Filter.process_lval: global variable expected"
	
and process_exp e =
  match e with
      Const c -> S.Const (process_const c)
    | UnOp (op, e) -> 
	let e = process_exp e in
	let op = process_unop op in
	  S.UnOp (op, e)
    | BinOp (op, e1, e2) -> 
	let e1 = process_exp e1 in
	let e2 = process_exp e2 in
	let op = process_binop op in
	  S.BinOp (op, e1, e2)
    | Lval (lv, t) -> 
	process_scalar_t t;
	S.Lval (process_lval lv)
    | _ -> invalid_arg "Filter.process_exp: not implemented yet"
  
let process_funexp f =
  match f with
      FunId f -> S.FunId f
    | _ -> invalid_arg "Filter.process_funexp: known function call expected"

let rec process_blk x = List.map process_stmt x 
  
and process_stmt (x, loc) = 
  Context.set_loc loc;
  (process_stmtkind x, loc)
    
and process_stmtkind x =
  match x with
      Set (lv, e, t) -> 
	process_scalar_t t;
	S.Set (process_lval lv, process_exp e)
    | Call f -> S.Call (process_funexp f)
    | _ -> invalid_arg "Filter.process_stmtkind: not implemented yet"
	
	
let process prog = 
  let globals = Hashtbl.create 100 in
  let fundecs = Hashtbl.create 100 in
    
  let process_global x (t, loc) = 
    process_typ t;
    Hashtbl.add globals x loc
  in

  let process_fundec f (t, body) = 
    process_ftyp t;
    let body = process_blk body in
      Hashtbl.add fundecs f body
  in

  let init = process_blk prog.init in
    Hashtbl.iter process_global prog.globals;
    Hashtbl.iter process_fundec prog.fundecs;
    {
      S.fnames = prog.fnames;
      S.globals = globals;
      S.init = init;
      S.fundecs = fundecs;
      S.src_lang = prog.src_lang;
    }
