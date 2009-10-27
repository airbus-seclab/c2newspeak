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

(* TODO: important note, 
   functions whose arguments differ after a given depth are going to be 
   analyzed multiple times EVEN if the values after some depth are never
   used by the function
   !!!!!!!!!!!!
   f(int *x );
   that uses only *x
   
   called on g
   and called on y that point to z that point to u
   is going to be analyzed twice, instead of once!!!

   think about memcpy
*)

open Newspeak

type env = {
  cur_fun: string;
  mutable height: int;
  mutable lbl_tbl: (Newspeak.lbl * State.t) list;
}

let push_var env = env.height <- env.height + 1

let pop_var env = env.height <- env.height - 1

let push env lbl = env.lbl_tbl <- (lbl, State.emptyset)::env.lbl_tbl

let pop env = 
  match env.lbl_tbl with
      (_, s)::tl -> 
	env.lbl_tbl <- tl;
	s
    | [] -> invalid_arg "Solver.pop: unreachable code"

let goto env lbl s = 
  let rec goto tbl =
    match tbl with
	(lbl', s')::tl when lbl = lbl' -> 
	  let s = State.join s s' in
	    (lbl, s)::tl
      | hd::tl -> hd::(goto tl)
      | [] -> invalid_arg "Solver.goto: unreachable code"
  in
    env.lbl_tbl <- goto env.lbl_tbl
  
let height_of_ftyp (args, ret) = 
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

let string_of_stats fun_tbl cache_miss cache_hit =
  let stats = ref [] in
  let count f data =
    let x = List.length data in
    let c = 
      try
	let c = List.assoc x !stats in
	  stats := List.remove_assoc x !stats;
	  StrSet.add f c
      with Not_found -> StrSet.singleton f
    in
      stats := (x, c)::!stats
  in
    Hashtbl.iter count fun_tbl;
    stats := List.sort (fun (x, _) (y, _) -> compare x y) !stats;
    let res = ref "Function distribution: " in
    let string_of_elem (nb_infos, funs) =
      let nb_funs = StrSet.cardinal funs in
	res := !res^"\nNumber of function with ";
	if nb_infos = 1 then res := !res^"a hoare triple"
	else res := !res^string_of_int nb_infos^" hoare triples";
	res := !res^": "^string_of_int nb_funs;
	if nb_funs <= 3 then res := !res^" ("^StrSet.to_string funs^")"
    in
      List.iter string_of_elem !stats;
      "Cache misses: "^(string_of_int !cache_miss)^"/"
      ^(string_of_int (!cache_miss + !cache_hit))^"\n"
      ^(!res)

let process glb_tbl prog = 
(* set of reachable functions *)
  let live_funs = ref StrSet.empty in

(* TODO: put these together as one data-structure?? *)
(* mapping from function to pre/post relation *)
  let fun_tbl = Hashtbl.create 100 in
(* stats for function cache reuse *)
  let cache_miss = ref 0 in
  let cache_hit = ref 0 in

(* results of the analysis *)
  let warnings = ref StrSet.empty in
  let warn_cnt = ref 0 in

  let init_fun f _ = Hashtbl.add fun_tbl f [] in

  let warn_deref () = 
    let msg = "potential null pointer deref" in
     let msg = Context.get_current_loc ()^": "^msg in
      if not (StrSet.mem msg !warnings) then begin
	incr warn_cnt;
	warnings := StrSet.add msg !warnings;
	print_endline msg
      end
  in

(* TODO: not so nice to transmit this env all the time, think about it!!! *)
(* TODO: put check_lval and check_exp in State?? *)
  let rec check_lval env s x = 
    match x with
	Global _ -> ()
      | Local _ -> ()
      | Shift (lv, e) -> 
	  check_lval env s lv;
	  check_exp env s e
      | Deref (e, _) -> 
	  check_exp env s e;
	  if not (State.exp_is_valid env s e) then warn_deref ()

  and check_exp env s x = 
    match x with
	Const _ -> ()
      | AddrOf lv -> check_lval env s lv
      | AddrOfFun _ -> ()
      | Lval (lv, _) -> check_lval env s lv
      | UnOp (_, e) -> check_exp env s e
      | BinOp (_, e1, e2) -> 
	  check_exp env s e1;
	  check_exp env s e2
  in

  let rec process_blk env x s =
    match x with
	[] -> s
      | (x, loc)::tl -> 
	  Context.set_current_loc loc;
	  let s = process_stmtkind env x s in
	    process_blk env tl s
	      
  and process_stmtkind env x s =
    match x with
	_ when State.is_empty s -> s
      | Set (lv, e, t) -> 
	  check_lval env.height s lv;
	  check_exp env.height s e;
	  State.assign (lv, e, t) env.height s
      | Copy (lv1, lv2, _) -> 
	  check_lval env.height s lv1;
	  check_lval env.height s lv2;
	  State.copy (lv1, lv2) env.height s
      | Decl (_, _, body) -> 
	  push_var env;
	  let s = process_blk env body s in
	  let s = State.remove_local env.height s in
	    pop_var env;
	    s

(* TODO: treatment for function call could be more precise:
   - differentiate between read and write
*)
      | Call f -> 
	  let f = process_funexp env f s in
	  let res = ref State.emptyset in
	  let add_call f = res := State.join !res (process_call env f s) in
	    List.iter add_call f;
	    !res

      | Select (br1, br2) -> 
	    let s1 = process_blk env br1 s in
	    let s2 = process_blk env br2 s in
	      State.join s1 s2
      | Guard e -> 
	  check_exp env.height s e;
	  State.guard e env.height s
	    (* TODO: change labels?? with the number of DoWith to traverse, 
	       but harder to manipulate? *)
      | DoWith (body, lbl, action) -> 
	  push env lbl;
	  let s1 = process_blk env body s in
	  let s2 = pop env in
	  let s2 = process_blk env action s2 in
	    State.join s1 s2
      | Goto lbl -> 
	  goto env lbl s;
	  State.emptyset
      | InfLoop body -> 
	  let rec fixpoint x =
	    let x' = process_blk env body x in
	      if not (State.contains x x') then fixpoint (State.join x x')
	  in
	    fixpoint s;
	    State.emptyset
      | UserSpec ((IdentToken "display")::_) -> 
	  print_endline (State.to_string s);
	  s
      | UserSpec _ -> 
	  raise (Exceptions.NotImplemented "Analysis.process_stmtkind")

  and process_funexp env e s =
    match e with
	FunId f -> f::[]
      | FunDeref (e, _) -> 
	  try State.exp_to_fun env.height s e
	  with Exceptions.Unknown -> 
	    raise (Exceptions.NotImplemented "Analysis.process_funexp")

  and process_call env f s =
    Context.print_graph ("edge: "^env.cur_fun^", "^f);
    try
(* TODO: maybe could look for the function's semantics after having prepared
   the current state?? *)
      let rel_list = Hashtbl.find fun_tbl f in

      let (ft, _) = Hashtbl.find prog.fundecs f in
      let locals_nb = height_of_ftyp ft in
      let locals = create_locals env.height locals_nb in
      let globals = Hashtbl.find glb_tbl f in
      let globals = List.map Memloc.of_global globals in
      let memlocs = locals@globals in
      let (unreach, reach) = State.split memlocs s in
      let tr1 = Subst.build_param_map env.height locals_nb in
      let reach = State.transport tr1 reach in
      let locals = create_locals locals_nb locals_nb in
      let memlocs = locals@globals in

	apply_rel f memlocs unreach tr1 reach rel_list

    with Not_found -> 
      try 
	let s = Stubs.process f env.height s in
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

  and process_fun f pre =
    let rel_list = Hashtbl.find fun_tbl f in
    let sz = string_of_int (List.length rel_list) in
(* TODO: could merge these two prints?? *)
      Context.print_verbose ("Analyzing: "^f^" ("^sz^")");
      Context.print_graph ("node: "^f);
      live_funs := StrSet.add f !live_funs;
      let (ft, body) = Hashtbl.find prog.fundecs f in
      let env = { cur_fun = f; height = height_of_ftyp ft; lbl_tbl = [] } in
	process_blk env body pre
	  
  and apply_rel f memlocs unreach tr1 reach rel_list =
    let rec apply rel_list =
      match rel_list with
	  [] -> 
	    incr cache_miss;
	    let post = process_fun f reach in
(* TODO: could this code be factored with the other case?? *)
	    let s = State.transport (Subst.invert tr1) post in
	    let s = State.glue unreach s in
	      (s, (reach, post)::[])
	| (pre, post)::tl -> 
	    try
	      let tr2 = State.build_transport reach memlocs pre in
	      let reach = State.transport tr2 reach in
	      let (pre, post) = 
		if State.contains pre reach then begin
		  incr cache_hit;
		  (pre, post)
		end else begin
		  incr cache_miss;
		  let pre = State.join reach pre in
		  let post = process_fun f pre in
		    (pre, post)
		end
	      in
	      let rel_list = (pre, post)::tl in
(* TODO: push this code back up!!! *)
	      let tr = Subst.invert (Subst.compose tr1 tr2) in
	      let s = State.transport tr post in
		(State.glue unreach s, rel_list)
	    with Exceptions.Unknown -> 
	      let (s, tl) = apply tl in
		(s, (pre, post)::tl)
    in
    let (s, rel_list) = apply rel_list in
      Hashtbl.replace fun_tbl f rel_list;
      s
  in
    
    (* initialization *)
    Hashtbl.iter init_fun prog.fundecs;
    let s = State.universe in
    let env = { cur_fun = ""; height = 0; lbl_tbl = [] } in
    let s = process_blk env prog.init s in
    let (ft, _) = 
      try Hashtbl.find prog.fundecs "main"
      with Not_found -> invalid_arg "Solver.process: missing main function"
    in
    let pre = build_main_info ft s in
    let post = process_fun "main" pre in
      Hashtbl.replace fun_tbl "main" [(pre, post)];

      Context.print_verbose (string_of_stats fun_tbl cache_miss cache_hit);
      (!live_funs, !warn_cnt)
      

(*
(* results of the analysis *)
  let warnings = ref StrSet.empty in
  let warn_cnt = ref 0 in
(* set of reachable functions *)
  let live_funs = ref StrSet.empty in

(*
  worklist: 
  list of functions to analyze, index of hoare triple to update
  when this list is empty, analysis ends
*)
(* TODO: could try a queue instead?? *)
  let todo = ref [] in
(* mapping from function to pre/post relation *)
  let fun_tbl = Hashtbl.create 100 in
  let cache_miss = ref 0 in
  let cache_hit = ref 0 in

(* inverse call graph *)
  let pred_tbl = Hashtbl.create 100 in
  let depth_tbl = Hashtbl.create 100 in

(* used during the analysis of a function *)
  let current_fun = ref ("", 0) in (* name of the function currently analysed *)
  let current_depth = ref 0 in
  let env = ref 0 in          (* number of local variables *)
  let lbl_tbl = ref [] in     (* table of states at each jump label *)


  let find_preds f = try Hashtbl.find pred_tbl f with Not_found -> [] in

  let string_of_fun_tbl () =
    let stats = ref [] in
    let count f data =
      let x = List.length data in
      let c = 
	try
	  let c = List.assoc x !stats in
	    stats := List.remove_assoc x !stats;
	    StrSet.add f c
	with Not_found -> StrSet.singleton f
      in
	stats := (x, c)::!stats
    in
      Hashtbl.iter count fun_tbl;
      stats := List.sort (fun (x, _) (y, _) -> compare x y) !stats;
      let res = ref "Function distribution: " in
      let string_of_elem (nb_infos, funs) =
	let nb_funs = StrSet.cardinal funs in
	  res := !res^"\nNumber of function with ";
	  if nb_infos = 1 then res := !res^"a hoare triple"
	  else res := !res^string_of_int nb_infos^" hoare triples";
	  res := !res^": "^string_of_int nb_funs;
	  if nb_funs <= 3 then res := !res^" ("^StrSet.to_string funs^")"
      in
	List.iter string_of_elem !stats;
	"Cache misses: "^(string_of_int !cache_miss)^"/"
	^(string_of_int (!cache_miss + !cache_hit))^"\n"
	^(!res)
  in

  let update_pred_tbl f =
    let pred = find_preds f in
      if not (List.mem !current_fun pred)
      then begin
	let (fname, _) = f in
	let d = Hashtbl.find depth_tbl fname in
	let d = max d (!current_depth+1) in
	  Hashtbl.replace depth_tbl fname d;
	  Hashtbl.replace pred_tbl f (!current_fun::pred)
      end
  in

  let schedule f = if not (List.mem f !todo) then todo := f::!todo in


  let deepest_todo () = 
    let rec deepest_todo l =
      match l with
	| (f, i)::[] -> 
	    let depth = Hashtbl.find depth_tbl f in
	      ((f, i, depth), [])
	| (f1, i1)::tl -> 
	    let depth1 = Hashtbl.find depth_tbl f1 in
	    let ((f2, i2, depth2), tl) = deepest_todo tl in
	      if depth2 > depth1 then ((f2, i2, depth2), (f1, i1)::tl) 
	      else ((f1, i1, depth1), (f2, i2)::tl)
	| [] -> raise Exit
    in
    let (res, new_todo) = deepest_todo !todo in
      todo := new_todo;
      res
  in

  let apply_rel f memlocs unreach tr1 reach rel_list =
    let rec apply i rel_list =
      match rel_list with
	  [] -> 
	    incr cache_miss;
	    schedule (f, i);
	    update_pred_tbl (f, i);
	    (State.emptyset, (reach, State.emptyset)::[])
	| (pre, post)::tl -> 
	    try
	      let tr2 = State.build_transport reach memlocs pre in
	      let reach = State.transport tr2 reach in
	      let pre = 
		if State.contains pre reach then pre
		else begin
		  schedule (f, i);
		  State.join reach pre
		end
	      in
(* TODO: push this code back up!!! *)
	      let tr = Subst.invert (Subst.compose tr1 tr2) in
	      let rel_list = (pre, post)::tl in
	      let post = State.transport tr post in
		incr cache_hit;
		update_pred_tbl (f, i);
		(State.glue unreach post, rel_list)
	    with Exceptions.Unknown -> 
	      let (s, tl) = apply (i+1) tl in
		(s, (pre, post)::tl)
    in
    let (s, rel_list) = apply 0 rel_list in
      Hashtbl.replace fun_tbl f rel_list;
      s
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
(*
  let exp_is_valid s x =
    try
(* TODO: should do something like exp_to_ptr
   then check_deref???
*)
      match x with
	  Lval (lv, Ptr) -> 
	    let a = State.lval_to_abaddr !env s lv in
	    let a = State.abaddr_to_addr a in
	      State.addr_is_valid s a
	| _ -> 
	    print_endline (Newspeak.string_of_exp x);
	    raise Exceptions.Unknown
    with Exceptions.Unknown -> false
  in
*)

  let rec process_blk x s =
    match x with
	[] -> s
      | (x, loc)::tl -> 
	  Context.set_current_loc loc;
	  let s = process_stmtkind x s in
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
	  State.copy (lv1, lv2) !env s
      | Decl (_, _, body) -> 
	  incr env;
	  let s = process_blk body s in
	  let s = State.remove_local !env s in
	    decr env;
	    s
	      
(* TODO: treatment for function call could be more precise:
   - differentiate between read and write
*)
      | Call f -> 
	  let f = process_funexp f s in
	  let res = ref State.emptyset in
	  let add_call f = res := State.join !res (process_call f s) in
	    List.iter add_call f;
	    !res
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
      | UserSpec _ -> 
	  raise (Exceptions.NotImplemented "Analysis.process_stmtkind")

  and process_funexp e s =
    match e with
	FunId f -> f::[]
      | FunDeref (e, _) -> 
	  try State.exp_to_fun !env s e
	  with Exceptions.Unknown -> 
	    raise (Exceptions.NotImplemented "Analysis.process_funexp")

  and process_call f s =
    Context.print_graph ("edge: "^(fst !current_fun)^", "^f);
    try
(* TODO: maybe could look for the function's semantics after having prepared
   the current state?? *)
      let rel_list = Hashtbl.find fun_tbl f in

      let (ft, _) = Hashtbl.find prog.fundecs f in
      let locals_nb = env_of_ftyp ft in
      let locals = create_locals !env locals_nb in
      let globals = Hashtbl.find glb_tbl f in
      let globals = List.map Memloc.of_global globals in
      let memlocs = locals@globals in
      let (unreach, reach) = State.split memlocs s in
      let tr1 = Subst.build_param_map !env locals_nb in
      let reach = State.transport tr1 reach in
      let locals = create_locals locals_nb locals_nb in
      let memlocs = locals@globals in

	apply_rel f memlocs unreach tr1 reach rel_list

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
  in
    
  let process_fun (f, i) body rel_list = 
    let changed = ref false in
    let rec process_pre i rel_list =
      match rel_list with
	  (pre, post)::tl when i = 0 -> 
	    let new_post = process_blk body pre in
	    let hd = 
	      if State.contains post new_post then (pre, post)
	      else begin
		changed := true;
		(pre, new_post)
	      end
	    in
	      hd::tl
	| hd::tl -> hd::(process_pre (i-1) tl)
	| [] -> []
    in
    let rel_list = process_pre i rel_list in
      Hashtbl.replace fun_tbl f rel_list;
      if !changed then begin
	let pred = find_preds (f, i) in
	  List.iter schedule pred
      end
  in
  
  let init_fun f _ = 
    Hashtbl.add fun_tbl f [];
    Hashtbl.add depth_tbl f 0 
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
      schedule ("main", 0);
      Hashtbl.replace fun_tbl "main" ((s, State.emptyset)::[]);
      
      (* fixpoint computation *)
      begin try
	while true do
	  let (f, i, depth) = deepest_todo () in
	    current_fun := (f, i);
	    current_depth := depth;
	    live_funs := StrSet.add f !live_funs;
	    (* TODO: could be optimized and analysed only the pre/post
	       conditions which are not yet complete!! *)
	    let rel_list = Hashtbl.find fun_tbl f in
	    let sz = string_of_int (List.length rel_list) in
	    let (ft, body) = Hashtbl.find prog.fundecs f in
	      env := env_of_ftyp ft;
	      Context.print_verbose ("Analyzing: "^f^" ("^sz^")");
	      Context.print_graph ("node: "^f);
	      process_fun (f, i) body rel_list
	done
      with Exit -> ()
      end;
      Context.print_verbose (string_of_fun_tbl ());
      (!live_funs, !warn_cnt)
*)
