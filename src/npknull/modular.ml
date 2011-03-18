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


open PtrSpeak

module type State =
sig
  type t
    
  val emptyset: unit -> t
    
  (* TODO: try to remove universe *)
  val universe: unit -> t
    
  val init: string list -> t
    
  val join: t -> t -> t
    
  val assign: PtrSpeak.exp -> PtrSpeak.exp -> t -> t
    
  val guard: PtrSpeak.exp -> t -> t
    
  val compose: t -> t -> t

  val substitute: Subst2.t -> t -> t

  val remove_variables: string list -> t -> t
    
   
  val print: t -> unit
    
  val size_of: t -> int
    
  val is_subset: t -> t -> bool

  val split: string list -> t -> (t * t)

  val transport: string list -> t -> t -> Subst2.t

  val glue: t -> t -> t

  val normalize: string list -> t -> (t * Subst2.t)

  val list_nodes: t -> VarSet.t
  val restrict: VarSet.t -> t -> t

  val satisfies: t -> PtrSpeak.formula -> bool
end

module Make(State: State) =
struct
  module LblStack = LblStack.Make(State)
    
  (* TODO: try to factor code with state.ml *)
  let fixpoint f state =
    let rec fixpoint state =
      let state' = f state in
	if (State.is_subset state' state) then state
	else begin
	  let state = State.join state state' in
	    fixpoint state
	end
    in
      fixpoint state
	
  (* TODO: remove *)
  let prepare_call args rets state =
    let result = ref state in
    let param_cnt = ref (-1) in
    let gen_param () =
      incr param_cnt;
      string_of_int !param_cnt
    in
    let pass_argument e =
      let param = gen_param () in
      let actual = "!actual"^param in
      let formal = "!formal"^param in
      let lv = LocalVar actual in
	result := State.assign lv e !result;
	(formal, actual)
    in
    let add_return _ =
      let param = gen_param () in
      let actual = "!actual"^param in
      let formal = "!formal"^param in
	(formal, actual)
    in
    let subst = List.map add_return rets in
    let subst = subst @ (List.map pass_argument args) in
      (subst, !result)
	
  (* TODO: remove *)
  let return_from_call actuals rets state =
    let result = ref state in
    let assign_return_value lv =
      (* TODO: think about it, but there is a dangerous disconnect between this
	 number 0 and the one in pass_arguments and also the name of the argument!! *)
      let e = PtrSpeak.Access (PtrSpeak.LocalVar "!actual0") in
	result := State.assign lv e !result
    in
      List.iter assign_return_value rets;
      State.remove_variables actuals !result
	
  let assign_return_value rets state =
    let result = ref state in
    let assign lv =
      let e = PtrSpeak.Access (PtrSpeak.LocalVar "!actual0") in
	result := State.assign lv e !result
    in
      List.iter assign rets;
      !result
	
  (* adds formal arguments and return variables to state *)
  let add_formals (args, rets) state = 
    let result = ref state in
    let param_cnt = ref (-1) in
    let gen_param () =
      incr param_cnt;
      let param = string_of_int !param_cnt in
      let actual = "!actual"^param in
      let formal = "!formal"^param in
	(actual, formal)
    in
    let pass_argument e =
      let (actual, formal) = gen_param () in
      let lv = LocalVar actual in
	result := State.assign lv e !result;
	(actual, formal)
  in
    let add_return _ = gen_param () in
    let subst = List.map add_return rets in
    let subst = List.map pass_argument args @ subst in
      (!result, subst)
	
	
  let rename_formals rename_subst state = State.substitute rename_subst state
    
  let remove_formals formals state = State.remove_variables formals state
    
  (* TODO: think about it, but here the function table is used for one thread
     only, so it is discarded after the analysis of each thread
     it would be more efficient to reuse it for all threads analyses
  *)
  let process_thread global_tbl fundecs init state =
    let fun_tbl = Hashtbl.create 100 in
      
    let check_lval state e = 
      match e with
	  Access e -> 
	    (* TODO: do more in preprocessor *)
	    if not (State.satisfies state (PtrSpeak.IsNotNull e))
	    then Context.print_err "potential null pointer deref"
	| _ -> ()
    in

    let check_exp state e = 
      match e with
	  Access lv -> check_lval state lv
	| _ -> ()
    in
      
    let rec process_fun f input = 
      let labels = LblStack.create () in
      let push_label lbl = LblStack.push labels lbl in
      let pop_label () = LblStack.pop labels in
      let goto_label lbl state = LblStack.add labels lbl state in
      let rec  process_blk x state =
	match x with
	    [] -> state
	  | (stmt, loc)::blk -> 
	      Context.set_current_loc loc;
	      let state = process_stmt stmt state in
		process_blk blk state
		  
      and process_stmt x state =
	match x with
	    Set (lv, e) -> 
	      check_lval state lv;
	      check_exp state e;
	      State.assign lv e state
	  | Call ([], "__display", []) -> 
	      (* TODO: do this in preprocessor *)
	      State.print state;
	      state
	  | Call (e1::e2::[], "__assert_are_not_equal", []) -> 
	      (* TODO: do this in preprocessor *)
	      if not (State.satisfies state (PtrSpeak.AreNotEqual (e1, e2))) 
	      then print_endline "Assert failed";
	      state
	  | Call (args, f, rets) -> 
	      let (state, subst) = add_formals (args, rets) state in
	      let (actuals, formals) = List.split subst in
	      let rename_subst = Subst2.of_list subst in
		(* TODO: cleanup: could try to avoid this VarSet.elements *)
	      let globals = VarSet.elements (Hashtbl.find global_tbl f) in
		(* TODO: put this comment down into State.mli
		   return two states, the first one contains all that is reachable from:
		   function argument + global variable used
		   the rest in the second state
		*)
	      let roots = actuals@globals in
		
	      let (state, unreachable) = State.split roots state in
	      (* TODO: a bit clumsy, think about how to simplify this... *)
	      let (state, normalize_subst) = State.normalize roots state in
		(*	      print_endline "After normalize";
			      State.print state;*)
	      let roots = formals@globals in
	      let state = rename_formals rename_subst state in 
	      let (input, output) = 
		try Hashtbl.find fun_tbl f 
		with Not_found -> (State.emptyset (), State.emptyset ())
	      in

	      (* TODO: is this a hack, shouldn't it rather be done in the transport 
		 structure? I don't know 
		 yes I think it would be better, but not so easy to do... *)
	      let nodes = State.list_nodes state in
	      let nodes = VarSet.union nodes (VarSet.of_list roots) in
	      let transport_subst = State.transport roots state input in
	      let state = State.substitute transport_subst state in
	      let state = 
		if State.is_subset state input then output
		else begin
		  let state = State.join state input in
		  let (state, subst) = State.normalize roots state in
		    (* has side-effect of updating table 
		       if necessary *)
		  let state = process_fun f state in
		    State.substitute (Subst2.inverse subst) state
		end
	      in
	      let state = 
	      State.substitute (Subst2.inverse transport_subst) state 
	      in
		(* TODO: is this a hack, shouldn't it rather be done in the transport 
		   structure? yes *)
	      let state = State.restrict nodes state in
	      let state = State.substitute (Subst2.inverse rename_subst) state in
	      let state = 
		State.substitute (Subst2.inverse normalize_subst) state 
	      in
	      let state = State.glue unreachable state in
	      let state = assign_return_value rets state in
	      let state = remove_formals actuals state in
		state
		  
	  | InfLoop blk -> fixpoint (fun x -> process_blk blk x) state
	  | Guard e -> 
	      check_exp state e;
	      State.guard e state
	  | Select (blk1, blk2) ->
	      let state1 = process_blk blk1 state in
	      let state2 = process_blk blk2 state in
		State.join state1 state2
	  | DoWith (blk, lbl) -> 
	      push_label lbl;
	      let state = process_blk blk state in
	      let label_state = pop_label () in
		State.join state label_state
	  | Goto lbl -> 
	      goto_label lbl state;
	      State.emptyset ()
      in
      let call (local_variables, body) () =
	let output = process_blk body input in
	let output = State.remove_variables local_variables output in
	  Hashtbl.replace fun_tbl f (input, output);
	  output
      in
	try
	  let info = Hashtbl.find fundecs f in
	    Pretty.execute_call_and_print f (call info)
	with Not_found -> 
	  print_endline ("Function '"^f
		       ^"' not found. Omitted from analysis.");
	  input
    in
      process_fun init state
	
  (* TODO: should reread the whole analysis and make tests for all bugs that 
     I foresee *)
	
  let process (global_tbl, fundecs) entry_point =
    let state = State.universe () in
    let _ = process_thread global_tbl fundecs entry_point state in
      ()
end
