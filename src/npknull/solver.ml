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

let universe () = ()

let emptyset = ()

let prepare_call _ _ = ()

let apply _ _ = ()

let join _ _ = ()

type t = unit

let process prog = 
  let current_loc = ref Newspeak.unknown_loc in
  let errors = ref StrSet.empty in
(* 
   list of functions to analyze
   when this list is empty, analysis ends
*)
(* TODO: could try a queue instead?? *)
  let todo = ref [] in
  let funtbl = Hashtbl.create 100 in
  let lbl_tbl = ref [] in

  let push lbl = lbl_tbl := (lbl, emptyset)::!lbl_tbl in
  let pop () = 
    match !lbl_tbl with
	(_, s)::tl -> 
	  lbl_tbl := tl;
	  s
      | [] -> invalid_arg "Solver.pop: unreachable code"
  in
  let goto lbl s = 
    let rec goto tbl =
      match tbl with
	  (lbl', s')::tl when lbl = lbl' -> 
	    let s = join s s' in
	      (lbl, s)::tl
	| _::tl -> goto tl
	| [] -> invalid_arg "Solver.goto: unreachable code"
    in
      lbl_tbl := goto !lbl_tbl
  in

  let print_err msg = 
    let msg = Newspeak.string_of_loc !current_loc^": "^msg in
      if not (StrSet.mem msg !errors) then begin
	errors := StrSet.add msg !errors;
	print_endline msg
      end
  in

  let warn_deref () = print_err "potential null pointer deref" in

(* TODO: maybe find a way to remove emptyset???!!! 
   use an exception instead
*)
  let init_fun f _ = Hashtbl.add funtbl f (emptyset, emptyset) in

  let rec check_lval x = 
    match x with
	Global _ -> ()
      | Local _ -> ()
      | Shift (lv, e) -> 
	  check_lval lv;
	  check_exp e
      | Deref _ -> warn_deref ()

  and check_exp x = 
    match x with
	Const _ -> ()
      | AddrOf (lv, _) -> check_lval lv
      | Lval (lv, _) -> check_lval lv
      | UnOp (_, e) -> check_exp e
      | BinOp (_, e1, e2) -> 
	  check_exp e1;
	  check_exp e2
      | _ -> warn_deref ()
  in

  let rec process_blk x s =
    match x with
	[] -> s
      | (x, loc)::tl -> 
	  current_loc := loc;
	  let s = process_stmtkind x s in
	    process_blk tl s
	      
  and process_stmtkind x s =
    match x with
	Set (lv, e, _) -> 
	  check_lval lv;
	  check_exp e;
	  s
      | Decl (_, _, body) -> process_blk body s
      | Call FunId f -> begin
	  try
	    let (init, rel) = Hashtbl.find funtbl f in
	    let init = prepare_call init s in
	    let s = apply rel s in
	      Hashtbl.replace funtbl f (init, rel);
	      todo := f::!todo;
	      s
	  with Not_found -> 
	    print_err ("missing function: "^f
		       ^", call ignored, analysis may be unsound")
	end
      | Select (br1, br2) -> join (process_blk br1 s) (process_blk br2 s)
      | Guard e -> 
	  check_exp e; 
	  s
(* TODO: change labels?? with the number of DoWith to traverse, 
   but harder to manipulate? *)
      | DoWith (body, lbl, action) -> 
	  push lbl;
	  let s1 = process_blk body s in
	  let s2 = pop () in
	  let s2 = process_blk action s2 in
	    join s1 s2
      | Goto lbl -> 
	  goto lbl s;
	  emptyset
      | _ -> invalid_arg "Analysis.process_stmtkind: case not implemented"
  in

    (* initialization *)
    Hashtbl.iter init_fun prog.fundecs;
    let s = universe () in
    let s = process_blk prog.init s in
    let _ = process_stmtkind (Call (FunId "main")) s in

    (* fixpoint computation *)
    try
      while true do
	match !todo with
	    f::tl -> 
	      todo := tl;
	      let (_, body) = Hashtbl.find prog.fundecs f in
	      let (s, _) = Hashtbl.find funtbl f in
		process_blk body s
	 | [] -> raise Exit
      done;
      ()
    with Exit -> ()
