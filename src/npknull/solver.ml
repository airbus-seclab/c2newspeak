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

let rec create_locals env n =
  let rec create n =
    if n < 0 then [] else (Memloc.of_local (env-n))::(create (n-1))
  in
    create n

let build_main_info ft s =
  match ft with
      (_::_::[], Some _) -> 
	State.set_pointsto (Memloc.of_local 2, 0) (Memloc.gen ()) s
    | (_::_::[], None) -> 
	State.set_pointsto (Memloc.of_local 1, 0) (Memloc.gen ()) s
    | ([], _) -> s
    | _ -> invalid_arg "Solver.add_main_info: unexpected type for main"

let process glb_tbl prog = 
(* results of the analysis *)
  let warnings = ref StrSet.empty in
  let warn_cnt = ref 0 in
(* set of reachable functions *)
  let live_funs = ref StrSet.empty in

(*
  worklist: 
  list of functions to analyze
  when this list is empty, analysis ends
*)
(* TODO: could try a queue instead?? *)
  let todo = ref [] in
(* mapping from function to pre/post relation *)
  let fun_tbl = Hashtbl.create 100 in

(* inverse call graph *)
  let pred_tbl = Hashtbl.create 100 in


(* used during the analysis of a function *)
  let current_fun = ref "" in (* name of the function currently analysed *)
  let env = ref 0 in          (* number of local variables *)
  let lbl_tbl = ref [] in     (* table of states at each jump label *)


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

  let exp_is_valid s x =
    try
      match x with
	  Lval (lv, Ptr) -> 
	    let a = State.lval_to_abaddr !env s lv in
	    let a = State.abaddr_to_addr a in
	      State.addr_is_valid s a
	| _ -> raise Exceptions.Unknown
    with Exceptions.Unknown -> false
  in

  let rec check_lval s x = 
    match x with
	Global _ -> ()
      | Local _ -> ()
      | Shift (lv, e) -> 
	  check_lval s lv;
	  check_exp s e
      | Deref (e, _) -> if not (exp_is_valid s e) then warn_deref ()

  and check_exp s x = 
    match x with
	Const _ -> ()
      | AddrOf (lv, _) -> check_lval s lv
      | AddrOfFun _ -> ()
      | Lval (lv, _) -> check_lval s lv
      | UnOp (_, e) -> check_exp s e
      | BinOp (_, e1, e2) -> 
	  check_exp s e1;
	  check_exp s e2
  in

  let rec process_blk x s =
    match x with
	[] -> s
      | (x, loc)::tl -> 
	  Context.set_current_loc loc;
	  let s = 
	    try process_stmtkind x s 
	    with Exceptions.NotImplemented msg -> 
	      Context.print_err ("Not implemented yet: "^msg);
	      s
	  in
	    process_blk tl s
	      
  and process_stmtkind x s =
    match x with
	_ when State.is_empty s -> s
      | Set (lv, e, t) -> 
	  check_lval s lv;
	  check_exp s e;
	  State.assign (lv, e, t) !env s
      | Copy (lv1, lv2, _) -> 
	  check_lval s lv1;
	  check_lval s lv2;
	  State.forget_lval lv1 !env s
      | Decl (_, _, body) -> 
	  incr env;
	  let s = process_blk body s in
	  let s = State.remove_local !env s in
	    decr env;
	    s
	      
(* TODO: treatment for function call could be more precise:
   - list of pre/post in the table
   - differentiate between read and write
   - 
*)
      | Call FunId f -> begin
	  try
	    let (pre, post) = Hashtbl.find fun_tbl f in
	    let (ft, _) = Hashtbl.find prog.fundecs f in
	    let locals_nb = env_of_ftyp ft in
	    let locals = create_locals !env locals_nb in
	    let globals = Hashtbl.find glb_tbl f in
	    let globals = List.map Memloc.of_global globals in
	    let memlocs = locals@globals in
	    let (unreach, reach, tr0) = State.split memlocs s in
	    let tr1 = State.build_param_map !env locals_nb in
	    let tr1 = State.compose tr0 tr1 in
	    let reach = State.transport tr1 reach in
	      try
		let locals = create_locals locals_nb locals_nb in
		let memlocs = locals@globals in
		let tr2 = State.build_transport reach memlocs pre in
		let reach = State.transport tr2 reach in
		let tr = State.invert (State.compose tr1 tr2) in
		  if not (State.contains pre reach) then begin
		    let pre = State.join reach pre in
		    let pred = Hashtbl.find pred_tbl f in
		      if not (List.mem !current_fun pred)
		      then Hashtbl.replace pred_tbl f (!current_fun::pred);
		      Hashtbl.replace fun_tbl f (pre, post);
		      if not (List.mem f !todo) then todo := f::!todo
		  end;
		  let post = State.transport tr post in
		    State.glue unreach post
	      with Exceptions.Unknown ->
		print_endline (State.to_string reach);
		print_endline (State.to_string pre);
		invalid_arg "Solver.call: not implemented yet"
	  with Not_found -> 
	    try 
	      let s = Stubs.process f !env s in
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
	    let s1 = process_blk br1 s in
	    let s2 = process_blk br2 s in
	      State.join s1 s2
      | Guard e -> 
	  check_exp s e;
	  State.guard e !env s
	    (* TODO: change labels?? with the number of DoWith to traverse, 
	       but harder to manipulate? *)
      | DoWith (body, lbl, action) -> 
	  push lbl;
	  let s1 = process_blk body s in
	  let s2 = pop () in
	  let s2 = process_blk action s2 in
	    State.join s1 s2
      | Goto lbl -> 
	  goto lbl s;
	  State.emptyset
      | InfLoop body -> 
	  let rec fixpoint x =
	    let x' = process_blk body x in
	      if not (State.contains x x') then fixpoint (State.join x x')
	  in
	    fixpoint s;
	    State.emptyset
      | UserSpec ((IdentToken "display")::_) -> 
	  print_endline (State.to_string s);
	  s
      | _ -> raise (Exceptions.NotImplemented "Analysis.process_stmtkind")
  in

  let init_fun f _ =
    Hashtbl.add fun_tbl f (State.emptyset, State.emptyset);
    Hashtbl.add pred_tbl f []
  in
    
    (* initialization *)
    Hashtbl.iter init_fun prog.fundecs;
    let s = State.universe in
    let s = process_blk prog.init s in
    let (ft, _) = 
      try Hashtbl.find prog.fundecs "main"
      with Not_found -> invalid_arg "Solver.process: missing main function"
    in
    let s = build_main_info ft s in
      todo := "main"::[];
      Hashtbl.add fun_tbl "main" (s, State.emptyset);
      
      (* fixpoint computation *)
      begin try
	while true do
	  match !todo with
	      f::tl -> begin
		todo := tl;
		Context.print_verbose ("Analyzing: "^f);
		current_fun := f;
		live_funs := StrSet.add f !live_funs;
		let (pre, post) = Hashtbl.find fun_tbl f in
		let (ft, body) = Hashtbl.find prog.fundecs f in
		  env := env_of_ftyp ft;
		  let new_post = process_blk body pre in
		    if not (State.contains post new_post) then begin
		      Hashtbl.replace fun_tbl f (pre, new_post);
		      let pred = Hashtbl.find pred_tbl f in
			todo := pred@(!todo)
		    end
	      end
	    | [] -> raise Exit
	done
      with Exit -> ()
      end;
      (!live_funs, !warn_cnt)
