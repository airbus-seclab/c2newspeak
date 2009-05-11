(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2009  Charles Hymans
 
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

(* TODO: maybe simplify DoWith in Newspeak:
   1) no action
   2) no lbl (Goto n where n is the number of labeled blocks to cross before
   getting there)
*)
open Newspeak
module F = Equations

let pos_of_lbl j lbl = 
  let rec pos_of_lbl n j =
    match j with
	lbl'::_ when lbl = lbl' -> n
      | _::tl -> pos_of_lbl (n+1) tl
      | [] -> invalid_arg "Factory.pos_of_lbl: unreachable code"
  in
    pos_of_lbl 0 j

let build prog =
  let globals = ref [] in
  let fundecs = Hashtbl.create 100 in
    
  let translate_global x _ = globals := x::!globals in

  let translate_binop op e1 e2 =
    match op with
	PlusI | MinusI | MultI | DivI | Mod | BOr _ | BAnd _ | Shiftlt -> 
	  F.BinOp (e1, e2)
      | PlusPI -> e1
      | _ -> invalid_arg ("Factory.translate_binop: not implemented yet: "
			  ^(Newspeak.string_of_binop op))
  in

  let rec translate_lval x =
    match x with
	Local x -> F.Local x
      | Global x -> F.Global x
      | Deref (e, _) -> F.Deref (translate_exp e)
      | Shift (lv, _) -> translate_lval lv
  
  and translate_exp x = 
    match x with
	Const _ -> F.Const
      | Lval (lv, _) -> F.Deref (translate_lval lv)
      | AddrOf (lv, _) -> translate_lval lv
      | AddrOfFun (f, _) -> F.Global f
      | UnOp (_, e) -> translate_exp e
      | BinOp (op, e1, e2) -> 
	  let e1 = translate_exp e1 in
	  let e2 = translate_exp e2 in
	    translate_binop op e1 e2
  in

  let translate_fn x =
    match x with
	FunId f -> f
      | _ -> invalid_arg "Factory.translate_fn: not implemented yet"
  in

  let rec translate_blk j x =
    match x with
	hd::tl -> 
	  let hd = translate_stmt j hd in
	  let tl = translate_blk j tl in
	    hd@tl
      | [] -> []

  and translate_stmt j (x, loc) = 
    match x with
	Set (lv, e, _) ->
	  let lv = translate_lval lv in
	  let e = translate_exp e in
	    (F.Set (lv, e), loc)::[]
(*
      | Copy (lv1, lv2, _) -> 
*)
      | Guard _ -> []
      | Decl (_, _, body) -> (F.Decl (translate_blk j body), loc)::[]
      | Select (br1, br2) -> 
	  let br1 = translate_blk j br1 in
	  let br2 = translate_blk j br2 in
	    (F.Select (br1, br2), loc)::[]
      | InfLoop body -> (F.InfLoop (translate_blk j body), loc)::[]
      | DoWith (body, lbl, []) -> 
	  let body = translate_blk (lbl::j) body in
	    (F.BlkLbl body, loc)::[]
      | Goto lbl -> (F.Goto (pos_of_lbl j lbl), loc)::[]
      | Call f -> (F.Call (translate_fn f), loc)::[]
      | _ -> 
	  invalid_arg ("Factory.translate_stmt: statement not handled yet: "
		       ^(Newspeak.string_of_stmt (x, loc)))
  in

  let translate_fundec f (_, body) =
    let body = translate_blk [] body in
      Hashtbl.add fundecs f body
  in

  let build_entry () =
    let main_tainted = "!main_tainted!" in
    let tainted_exp = F.Global main_tainted in
    let loc = Newspeak.unknown_loc in
    let ((args, ret), _) = Hashtbl.find prog.fundecs "main" in
    let call = ref ((F.Call "main", Newspeak.unknown_loc)::[]) in
    let rec append_args args =
      match args with
	  _::tl -> 
	    let taint = (F.Taint (F.Local 0), loc) in
	    let set = (F.Set (F.Local 0, tainted_exp), loc) in
	      call := (F.Decl (taint::set::(!call)), loc)::[];
	      append_args tl
	| [] -> ()
    in
      globals := main_tainted::!globals;
      append_args args;
      call := (F.Set (tainted_exp, tainted_exp), loc)::!call;
      call := (F.Taint tainted_exp, loc)::!call;
      match ret with
	  Some _ -> (F.Decl (!call), loc)::[]
	| None -> !call
  in

  let entry = build_entry () in
    Hashtbl.iter translate_global prog.globals;
    Hashtbl.iter translate_fundec prog.fundecs;
    (!globals, fundecs, entry)
