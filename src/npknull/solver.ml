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
  let warnings = ref StrSet.empty in
(* 
   list of functions to analyze
   when this list is empty, analysis ends
*)
(* TODO: could try a queue instead?? *)
  let todo = ref [] in
  let fun_tbl = Hashtbl.create 100 in
  let lbl_tbl = ref [] in
  let init_tbl = Hashtbl.create 100 in
  let keep_fun = ref false in
  let pred_tbl = Hashtbl.create 100 in
  let current_fun = ref "" in

  let live_funs = ref StrSet.empty in

  let push lbl = lbl_tbl := (lbl, State.emptyset)::!lbl_tbl in
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
	    let s = State.join s s' in
	      (lbl, s)::tl
	| hd::tl -> hd::(goto tl)
	| [] -> invalid_arg "Solver.goto: unreachable code"
    in
      lbl_tbl := goto !lbl_tbl
  in

  let warn_deref () = 
    let msg = "potential null pointer deref" in
    let msg = Context.get_current_loc ()^": "^msg in
      if not (StrSet.mem msg !warnings) then begin
	incr warn_cnt;
	warnings := StrSet.add msg !warnings;
	print_endline msg
      end
  in

  let exp_is_valid env s x =
    try
      match x with
	  Lval (lv, Ptr) -> 
	    let a = State.lval_to_abaddr env s lv in
	    let a = State.abaddr_to_addr a in
	      State.addr_is_valid s a
	| _ -> raise Exceptions.Unknown
    with Exceptions.Unknown -> false
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
	  Context.set_current_loc loc;
	  let s = process_stmtkind x env s in
	    process_blk tl env s
	      
  and process_stmtkind x env s =
    match x with
	_ when State.is_empty s -> s
      | Set (lv, e, t) -> 
	  check_lval env s lv;
	  check_exp env s e;
	  State.assign (lv, e, t) env s
      | Copy (lv1, lv2, _) -> 
	  check_lval env s lv1;
	  check_lval env s lv2;
	  State.forget_lval lv1 env s
      | Decl (_, _, body) -> 
	  let env = env + 1 in
	  let s = process_blk body env s in
	    State.remove_local env s
      | Call FunId f -> begin
	  try
	    let rel = Hashtbl.find fun_tbl f in
	      (* TODO: think about this, rewrite *)
	    let (is_new, init) = State.prepare_call s rel in
	      if is_new then begin
		let pred = Hashtbl.find pred_tbl f in
		let init = 
		  try State.join (Hashtbl.find init_tbl f) init
		  with Not_found -> init
		in
		  if not (List.mem !current_fun pred) 
		  then Hashtbl.replace pred_tbl f (!current_fun::pred);
		  Hashtbl.replace init_tbl f init;
		  if not (List.mem f !todo) then todo := f::!todo;
		  keep_fun := true
	      end;
	      State.apply s rel
	  with Not_found -> 
	    try 
	      let s = Stubs.process f env s in
		(* TODO: factor with print_err!! *)
		Context.report_stub_used ("missing function: "^f
					  ^", stub used, "
					  ^"use option --use-stubs to "^
					  "skip this message");
		s
  	    with Not_found -> 
	      Context.print_err ("missing function: "^f
				 ^", call ignored, analysis may be unsound");
	      s
	end
      | Select (br1, br2) -> 
	  let s1 = process_blk br1 env s in
	  let s2 = process_blk br2 env s in
	    State.join s1 s2
      | Guard e -> 
	  check_exp env s e;
	  State.guard e env s
(* TODO: change labels?? with the number of DoWith to traverse, 
   but harder to manipulate? *)
      | DoWith (body, lbl, action) -> 
	  push lbl;
	  let s1 = process_blk body env s in
	  let s2 = pop () in
	  let s2 = process_blk action env s2 in
	    State.join s1 s2
      | Goto lbl -> 
	  goto lbl s;
	  State.emptyset
      | InfLoop body -> 
	  let rec fixpoint x =
	    let x' = process_blk body env x in
	      if not (State.contains x x') then fixpoint (State.join x x')
	  in
	    fixpoint s;
	    State.emptyset
      | UserSpec ((IdentToken "__npknull_display")::_) -> 
	  print_endline (State.to_string s);
	  s
      | _ -> invalid_arg "Analysis.process_stmtkind: case not implemented"
  in

  let init_fun f _ =
    Hashtbl.add fun_tbl f State.emptyset;
    Hashtbl.add pred_tbl f []
  in

    (* initialization *)
    Hashtbl.iter init_fun prog.fundecs;
    let s = State.universe in
    let s = process_blk prog.init 0 s in
    let s = State.set_pointsto ("L.2", 0) (Memloc.gen ()) s in
      Hashtbl.add init_tbl "main" s;
      todo := "main"::[];
      
      (* fixpoint computation *)
      begin try
	while true do
	  match !todo with
	      f::tl -> 
		Context.print_verbose ("Analyzing: "^f);
		current_fun := f;
		live_funs := StrSet.add f !live_funs;
		todo := tl;
		let s = Hashtbl.find init_tbl f in
		let (ft, body) = Hashtbl.find prog.fundecs f in
		let env = env_of_ftyp ft in
		let s = process_blk body env s in
		  if !keep_fun then keep_fun := false
		  else Hashtbl.remove init_tbl f;
		  keep_fun := false;
		  let rel = Hashtbl.find fun_tbl f in
		    if not (State.contains rel s) then begin
		      let s = State.join rel s in
			Hashtbl.replace fun_tbl f s;
			let pred = Hashtbl.find pred_tbl f in
			  todo := pred@(!todo)
		    end;
		    ()
	    | [] -> raise Exit
	done
      with Exit -> ()
      end;
      (!live_funs, !warn_cnt)
