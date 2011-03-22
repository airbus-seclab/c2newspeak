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

(* TODO: try to factor the bottom up iterator which is found both in 
   directAccess and here
*)
let compute entry_points prog =
  let result = Hashtbl.create 100 in

  let rec process_lval lv =
    match lv with
	Global x -> VarSet.singleton x
      | Local _ -> VarSet.empty
      | Deref (e, _) -> process_exp e
      | Shift (lv, e) -> VarSet.union (process_lval lv) (process_exp e)

  and process_exp e =
    match e with
	Const _ | AddrOfFun _ -> VarSet.empty
      | Lval (lv, _) -> process_lval lv
      | AddrOf lv -> process_lval lv
      | UnOp (_, e) -> process_exp e
      | BinOp (_, e1, e2) -> VarSet.union (process_exp e1) (process_exp e2)
  in

  let rec process_fun f =
    try Hashtbl.find result f
    with Not_found -> 
      let variables =
	try
	  let fundec = Hashtbl.find prog.fundecs f in
	    process_blk fundec.body
	with Not_found ->
	  Context.print_err ("Function '"^f
			     ^"' not found. Omitted from analysis.");
	  VarSet.empty
      in
	Hashtbl.add result f variables;
	variables

  and process_blk x =
    match x with
	[] -> VarSet.empty
      | (stmt, loc)::blk -> 
	  Context.set_current_loc loc;
	  VarSet.union (process_stmt stmt) (process_blk blk)

  and process_stmt x =
    match x with
	Set (lv, e, _) -> VarSet.union (process_lval lv) (process_exp e)
      | Copy (lv1, lv2, _) -> VarSet.union (process_lval lv1) (process_lval lv2)
      | Guard e -> process_exp e
      | Decl (_, _, blk) | InfLoop blk | DoWith (blk, _) -> process_blk blk
      | Call (args, FunId f, rets) -> 
	  let arg_globals = process_args args in
	  let ret_globals = process_rets rets in
	  let function_globals = process_fun f in
	  let used_globals = VarSet.union arg_globals ret_globals in 
	    VarSet.union used_globals function_globals
      | Select (blk1, blk2) -> 
	  VarSet.union (process_blk blk1) (process_blk blk2)
      | Goto _ -> VarSet.empty
      | _ -> 
	  invalid_arg ("UsedGlobals.compute_used_globals.process_stmt:"
		       ^" not implemented yet")
  and process_args x =
    match x with
	[] -> VarSet.empty
      | (e, _)::args -> VarSet.union (process_exp e) (process_args args)

  and process_rets x =
    match x with
	[] -> VarSet.empty
      | (lv, _)::args -> VarSet.union (process_lval lv) (process_rets args)
  in

  let handle_fun x =
    let _ = process_fun x in
      ()
  in
    List.iter handle_fun entry_points;
    result
