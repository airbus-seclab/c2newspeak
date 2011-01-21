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

open Lowspeak

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
    n + List.length ret - 1

let rec create_locals env n =
  let rec create n =
    if n < 0 then [] else (Memloc.of_local (env-n))::(create (n-1))
  in
    create n

let build_main_info ft s =
  match ft with
      (_::_::[], _::[]) -> 
	State.set_pointsto (Memloc.of_local 2, 0) (Memloc.gen ()) s
    | (_::_::[], []) -> 
	State.set_pointsto (Memloc.of_local 1, 0) (Memloc.gen ()) s
    | ([], _) -> s
    | _ -> invalid_arg "Solver.add_main_info: unexpected type for main"

let string_of_triple (pre, post) = 
  State.to_string pre^" --> "^State.to_string post

let string_of_info x =
  ListUtils.to_string string_of_triple "\n" x

let string_of_stats fun_tbl cache_miss cache_hit =
  let stats = ref [] in
  let max = ref 0 in
  let count f data =
    let x = List.length data in
    let c = 
      try
	let c = List.assoc x !stats in
	  stats := List.remove_assoc x !stats;
	  StrSet.add f c
      with Not_found -> StrSet.singleton f
    in
      if x > !max then max := x;
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
	if nb_funs <= 3 then res := !res^" ("^StrSet.to_string funs^")";
	if nb_infos = !max then begin
	  let show_fun f =
	    res := !res^("\nFunction: "^f^"\n");
	    res := !res^(string_of_info (Hashtbl.find fun_tbl f))
	  in
	    StrSet.iter show_fun funs
	end
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
      | DoWith (body, lbl) -> 
	  push env lbl;
	  let s = process_blk env body s in
	    (* TODO: the code for pop could be simplified *)
	  let _ = pop env in
	    s
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
	  invalid_arg "Analysis.process_stmtkind: not implemented yet"

  and process_funexp env e s =
    match e with
	FunId f -> f::[]
      | FunDeref (e, _) -> 
	  try State.exp_to_fun env.height s e
	  with Exceptions.Unknown -> 
	    invalid_arg "Analysis.process_funexp: not implemented yet"

  and process_call env f s =
    Context.print_graph ("edge: "^env.cur_fun^", "^f);
    try
(* TODO: maybe could look for the function's semantics after having prepared
   the current state?? *)
      let rel_list = Hashtbl.find fun_tbl f in

      let declaration = Hashtbl.find prog.fundecs f in
      let locals_nb = height_of_ftyp declaration.ftyp in
      let locals = create_locals env.height locals_nb in
      let globals = Hashtbl.find glb_tbl f in
      let globals = List.map Memloc.of_global globals in
      let memlocs = locals@globals in
(* splits the store into reachable and unreachable 
   (from globals and function arguments) portions
*)
      let (unreach, reach) = State.split memlocs s in

(* do parameter passing (could/should? do this before??) that way locals 
   are built only once?? *)
      let tr1 = Subst.build_param_map env.height locals_nb in
      let reach = State.transport tr1 reach in

      let locals = create_locals locals_nb locals_nb in
      let memlocs = locals@globals in

      let tr2 = State.normalize memlocs reach in
(* TODO: could this transport be made once with the previous transport?? *)
      let reach = State.transport tr2 reach in

      let post = get_post f memlocs reach rel_list in

(* TODO: use transport and invert instead here, remove this code *)
      let post = State.transport (Subst.invert tr2) post in
(* TODO: right now unsound need to implement and add this too *)
(* { \sigma \mid \exists \sigma1 \in reach and \sigma2 \in post
   \forall V \in partition tr2, 
   \exists x \sigma(x) = \sigma2(x) 
    \sigma(V\x) = \sigma1(V\x) } 
   in other words, for all elements in the partition one of them is updated by
   post, the other ones keep their value in reach
*)
      let post = State.compose reach (Subst.domain tr2) post in

      let post = State.transport (Subst.invert tr1) post in
	State.glue unreach post

    with Not_found -> 
      try 
	let s = Stubs.process f env.height s in
	  (* TODO: factor with print_err!! *)
	  Context.print_err_with_advice Context.UseStubs 
	    ("missing function: "^f^", stub used");
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
      let declaration = Hashtbl.find prog.fundecs f in
      let env = 
	{ cur_fun = f; height = height_of_ftyp declaration.ftyp; lbl_tbl = [] }
      in
	process_blk env declaration.body pre
	  
  and get_post f memlocs reach rel_list =
    let rec apply rel_list =
      match rel_list with
	  [] -> 
	    incr cache_miss;
	    let post = process_fun f reach in
(* TODO: could this code be factored with the other case?? *)
	      (post, (reach, post)::[])
	| (pre, post)::tl -> 
	    try
	      let tr = State.build_transport reach memlocs pre in
	      let reach = State.transport tr reach in
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
	      let s = State.transport (Subst.invert tr) post in
		(s, rel_list)
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
    let declaration = 
      try Hashtbl.find prog.fundecs "main"
      with Not_found -> invalid_arg "Solver.process: missing main function"
    in
    let pre = build_main_info declaration.ftyp s in
    let post = process_fun "main" pre in
      Hashtbl.replace fun_tbl "main" [(pre, post)];

      Context.print_verbose (string_of_stats fun_tbl cache_miss cache_hit);
      (!live_funs, !warn_cnt)
      

