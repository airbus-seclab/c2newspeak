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

(* Put all variables declarations at function scope 
   Maybe, C2Newspeak's syntax should be restricted that way actually *)

open Newspeak

let rec shift_vars_lval i x =
  match x with
      Local v when v >= i -> Local (v + 1) 
    | Local _ | Global _ -> x
    | Deref (ptr, n) -> Deref (shift_vars_exp i ptr, n)
    | Shift (lv, e) -> Shift (shift_vars_lval i lv, shift_vars_exp i e)

and shift_vars_exp i x =
  match x with
      Const _ -> x
    | Lval (lv, n) -> Lval (shift_vars_lval i lv, n)
    | AddrOf (lv, n) -> AddrOf (shift_vars_lval i lv, n)
    | AddrOfFun _ -> x
    | UnOp (op, e) -> UnOp (op, shift_vars_exp i e)
    | BinOp (op, e1, e2) -> 
	BinOp (op, shift_vars_exp i e1, shift_vars_exp i e2)

let shift_vars_fn i x =
  match x with
      FunId f -> FunId f
    | FunDeref (ptr, t) -> FunDeref (shift_vars_exp i ptr, t)

let rec shift_vars_blk i x = 
  List.map (fun (x, loc) -> (shift_vars_stmtkind i x, loc)) x
    
and shift_vars_stmtkind i x =
  match x with
      Set (lv, e, t) -> Set (shift_vars_lval i lv, shift_vars_exp i e, t)
    | Copy (lv1, lv2, n) -> 
	Copy (shift_vars_lval i lv1, shift_vars_lval i lv2, n)
    | Guard b -> Guard (shift_vars_exp i b)
    | Decl (v, t, body) -> Decl (v, t, shift_vars_blk (i + 1) body)
    | Goto _ -> x
    | Call fn -> Call (shift_vars_fn i fn)
    | Select (blk1, blk2) -> 
	Select (shift_vars_blk i blk1, shift_vars_blk i blk2)
    | InfLoop body -> InfLoop (shift_vars_blk i body)
    | DoWith (body, lbl, action) ->
	DoWith (shift_vars_blk i body, lbl, shift_vars_blk i action)

let process prog =
  let rec process_blk x = 
    match x with
	(Decl (v, t, body), loc)::tl ->
	  let tl = shift_vars_blk 0 tl in
	  let body = body@tl in
	  let body = process_blk body in
	    [Decl (v, t, body), loc]
      | (hd, loc)::(Decl (v, t, body), loc')::tl ->
	  let hd = shift_vars_stmtkind 0 hd in
	    process_blk ((Decl (v, t, (hd, loc)::body), loc')::tl)
      | (x, loc)::tl -> (process_stmtkind x, loc)::(process_blk tl)
      | [] -> []
 
  and process_stmtkind x =
    match x with
      | Select (blk1, blk2) -> Select (process_blk blk1, process_blk blk2)
      | InfLoop body -> InfLoop (process_blk body)
      | DoWith (body, lbl, action) ->
	  DoWith (process_blk body, lbl, process_blk action)
      | Set _ | Copy _ | Goto _ | Call _ | Guard _ -> x
      | Decl _ -> 
	  invalid_arg ("Variable_hoist.process.process_stmtkind: "
			^"Unexpected declaration")
  in

  let res = Hashtbl.create 100 in
  let process_fun fid (t, body) =
    let body = process_blk body in
      Hashtbl.add res fid (t, body)
  in

    Hashtbl.iter process_fun prog.fundecs;
    { prog with fundecs = res }
