(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007, 2011  Charles Hymans, Sarah Zennou
  
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
  
  Sarah Zennou
  email: sarah(dot)zennou(at)eads(dot)net
*)

open Newspeak

module S = Simple

let int_bounds = Newspeak.domain_of_typ (Signed, 32)

let globals = ref []

let process_scalar_t t =
  match t with
      Int (Signed, 32) -> ()
    | _ -> invalid_arg "Filter.process_scalar_t: int scalar type expected"

let process_typ t =
  match t with
      Scalar t -> process_scalar_t t
    | _ -> invalid_arg "Filter.process_typ: scalar type expected"

let process_ftyp args ret =
  if ret <> [] then begin
    invalid_arg "Filter.process_ftyp: function without return value expected"
  end;
  if args <> [] then begin
    invalid_arg "Filter.process_ftyp: function with no parameter expected"
  end

let process_nat n =
  if not (Newspeak.belongs n int_bounds) then begin
    invalid_arg ("Filter.process_const: "
		 ^"integer not representable as a 32 bits integer")
  end;
  Int32.of_string (Nat.to_string n)

let process_const c =
  match c with
      CInt n -> S.CInt (process_nat n)
    | _ -> invalid_arg "Filter.process_const: integer constant expected"

let process_bounds (l, u) = 
  if not (Newspeak.contains int_bounds (l, u)) then begin
    invalid_arg ("Filter.process_bounds: "
		 ^"bounds not representable as 32 bits integer")
  end
    
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
    | UnOp (Not, e) -> S.UnOp (S.Not, process_exp e)
    | UnOp (Coerce bounds, e) -> 
	process_bounds bounds;
	process_exp e
    | BinOp (op, e1, e2) -> 
	let e1 = process_exp e1 in
	let e2 = process_exp e2 in
	let op = process_binop op in
	  S.BinOp (op, e1, e2)
    | Lval x -> S.Lval (process_typed_lval x)
    | _ -> invalid_arg "Filter.process_exp: integer expression expected"

and process_typed_lval (lv, t) = 
  process_typ t;
  process_lval lv
  
let process_funexp (args, f, rets) =
  begin
    match (args, rets) with
      | ([], []) -> ()
      | _ -> 
	  invalid_arg "Filter.process_funexp: only void -> void functions are allowed" 
  end;
  match f with
      FunId f -> S.FunId f
    | _ -> invalid_arg "Filter.process_funexp: known function call expected"

let process_guard e not_e =
  match (e, not_e) with
      (e1, UnOp (Not, e2)) | (UnOp (Not, e1), e2) when e1 = e2 -> 
	process_exp e
    | _ -> 
	invalid_arg "Filter.process_guard: unexpected guard, case not handled"

let process_loop_guard lbl_exit x =
  match x with
      ((Guard e, _)::[], (Guard not_e, _)::(Goto lbl, _)::[]) 
	when lbl = lbl_exit -> 
	  process_guard e not_e
    | _ -> 
	invalid_arg ("Filter.process_loop_guard: "
		     ^"unexpected loop guard, case not handled")

let process_assertion x =
  match x with
      LvalToken (lv,t)::SymbolToken c::SymbolToken '='::CstToken (CInt n)::[] -> 
	let lv' = process_typed_lval (lv, t) in
	let n = S.CInt (process_nat n) in
	let cmp =
	  match c with
	      '=' -> S.Equals
	    | '<' -> S.IsLess
	    | _ -> 
		invalid_arg ("Filter.process_assertion: "
			     ^"unexpected operator in assertion")
	in
	  (lv', cmp, n)
    | _ -> 
	invalid_arg "Filter.process_assertion: unexpected syntax for assertion"

let rec process_blk x = List.map process_stmt x 
  
and process_stmt (x, loc) = 
  Context.set_loc loc;
  (process_stmtkind x, loc)
    
and process_stmtkind x =
  match x with
      Set (lv, e, t) -> 
	process_scalar_t t;
	S.Set (process_lval lv, process_exp e)
    | Select ((Guard e, _)::br1, (Guard not_e, _)::br2) ->
	let e = process_guard e not_e in
	let br1 = process_blk br1 in
	let br2 = process_blk br2 in
	  S.If (e, br1, br2)
    | Call f -> S.Call (process_funexp f)
    | DoWith ((InfLoop ((Select loop_guard, _)::body), _)::[], lbl) -> 
	let e = process_loop_guard lbl loop_guard in
	let body = process_blk body in
	  S.While (e, body)
    | UserSpec (IdentToken "assert"::x) -> S.Assert (process_assertion x)
    | UserSpec (LvalToken (lv, t)::IdentToken "between"
		  ::CstToken (CInt l)::IdentToken "and"::CstToken (CInt u)::
		  []) -> 
	process_typ t;
	S.Set (process_lval lv, S.Random (process_nat l, process_nat u))
    | _ -> 
	invalid_arg ("Filter.process_stmtkind: "
		     ^"unexpected statement, case not handled yet")
	
	
let process prog = 
  let fundecs = Hashtbl.create 100 in
    
  let process_global x t = 
    process_typ t;
    globals := x::!globals
  in

  let process_fundec f fd = 
    process_ftyp fd.args fd.rets;
    let body = process_blk fd.body in
      Hashtbl.add fundecs f body
  in

  let init = process_blk prog.init in
    Hashtbl.iter process_global prog.globals;
    Hashtbl.iter process_fundec prog.fundecs;
    let prog =
      {
	S.globals = !globals;
	S.init = init;
	S.fundecs = fundecs;
	S.src_lang = prog.src_lang;
      }
    in
      Context.print_verbose (Simple.to_string prog);
      prog
