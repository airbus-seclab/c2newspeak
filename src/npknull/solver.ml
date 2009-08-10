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

let env_of_ftyp (args, ret) = 
  let n = List.length args in
    match ret with
	None -> n - 1
      | Some _ -> n

let process prog = 
  let warn_cnt = ref 0 in
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

  let live_funs = ref StrSet.empty in

  let memloc_cnt = ref (-1) in
  let gen_memloc () = 
    incr memloc_cnt;
    "H."^string_of_int !memloc_cnt
  in

  let push lbl = lbl_tbl := (lbl, Store.emptyset)::!lbl_tbl in
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
	    let s = Store.join s s' in
	      (lbl, s)::tl
	| hd::tl -> hd::(goto tl)
	| [] -> invalid_arg "Solver.goto: unreachable code"
    in
      lbl_tbl := goto !lbl_tbl
  in

  let print_err msg = 
    let msg = Newspeak.string_of_loc !current_loc^": "^msg in
      if not (StrSet.mem msg !errors) then begin
	errors := StrSet.add msg !errors;
	prerr_endline msg
      end
  in

  let warn_deref () = 
    let msg = "potential null pointer deref" in
    let msg = Newspeak.string_of_loc !current_loc^": "^msg in
      if not (StrSet.mem msg !errors) then begin
	incr warn_cnt;
	errors := StrSet.add msg !errors;
	print_endline msg
      end
  in

  let exp_is_valid env s x =
    try
      match x with
	  Lval (lv, Ptr) -> 
	    let a = Store.lval_to_abaddr env s lv in
	    let a = Store.abaddr_to_addr a in
	      Store.addr_is_valid s a
	| _ -> raise Store.Unknown
    with Store.Unknown -> false
  in

  let rec check_lval env s x = 
    match x with
	Global _ -> ()
      | Local _ -> ()
      | Shift (lv, e) -> 
	  check_lval env s lv;
	  check_exp env s e
      | Deref (e, _) -> if not (exp_is_valid env s e) then warn_deref ()

  and check_exp env s x = 
    match x with
	Const _ -> ()
      | AddrOf (lv, _) -> check_lval env s lv
      | AddrOfFun _ -> ()
      | Lval (lv, _) -> check_lval env s lv
      | UnOp (_, e) -> check_exp env s e
      | BinOp (_, e1, e2) -> 
	  check_exp env s e1;
	  check_exp env s e2
  in

  let rec process_blk x env s =
    match x with
	[] -> s
      | (x, loc)::tl -> 
	  current_loc := loc;
	  let s = process_stmtkind x env s in
	    process_blk tl env s
	      
  and process_stmtkind x env s =
    match x with
	Set (lv, e, t) -> 
	  check_lval env s lv;
	  check_exp env s e;
	  Store.assign (lv, e, t) env s
      | Decl (_, _, body) -> 
	  let env = env + 1 in
	  let s = process_blk body env s in
	    Store.remove_local env s
      | Call FunId f -> begin
	  try
	    let rel = Hashtbl.find funtbl f in
	      (* TODO: think about this, rewrite *)
	    let (is_new, init) = Store.prepare_call rel s in
	      if is_new then todo := (f, init)::!todo;
	      Store.apply rel s
	  with Not_found -> 
	    print_err ("missing function: "^f
		       ^", call ignored, analysis may be unsound");
	    s
	end
      | Select (br1, br2) -> 
	  let s1 = process_blk br1 env s in
	  let s2 = process_blk br2 env s in
	    Store.join s1 s2
      | Guard e -> 
	  check_exp env s e;
	  s
(* TODO: change labels?? with the number of DoWith to traverse, 
   but harder to manipulate? *)
      | DoWith (body, lbl, action) -> 
	  push lbl;
	  let s1 = process_blk body env s in
	  let s2 = pop () in
	  let s2 = process_blk action env s2 in
	    Store.join s1 s2
      | Goto lbl -> 
	  goto lbl s;
	  Store.emptyset
      | InfLoop body -> 
	  let rec fixpoint x =
	    let x' = process_blk body env x in
	      if not (Store.contains x x') then fixpoint (Store.join x x')
	  in
	    fixpoint s;
	    Store.emptyset
      | _ -> invalid_arg "Analysis.process_stmtkind: case not implemented"
  in

    (* initialization *)
    Hashtbl.iter (fun f _ -> Hashtbl.add funtbl f Store.emptyset) prog.fundecs;
    let s = Store.universe () in
    let s = process_blk prog.init 0 s in
    let s = Store.set_pointsto ("L.2", 0) (gen_memloc ()) s in
      todo := ("main", s)::[];
      
      (* fixpoint computation *)
      begin try
	while true do
	  match !todo with
	      (f, s)::tl -> 
		live_funs := StrSet.add f !live_funs;
		todo := tl;
		let (ft, body) = Hashtbl.find prog.fundecs f in
		let env = env_of_ftyp ft in
		let _ = process_blk body env s in
		  ()
	    | [] -> raise Exit
	done
      with Exit -> ()
      end;
      (!live_funs, !warn_cnt)
