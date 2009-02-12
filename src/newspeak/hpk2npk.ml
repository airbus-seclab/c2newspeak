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

open Highspeak

module N = Newspeak

let tmp_var = -1

let scalar_of_typ t =
  match t with
      N.Scalar t -> t
    | _ -> 
	Npkcontext.report_error "Hpk2npk.scalar_of_typ" "scalar type expected"

let translate prog = 
  let fundecs = Hashtbl.create 100 in
  let globals = Hashtbl.create 100 in

  let env = Hashtbl.create 100 in
  let stack_height = ref 0 in
  let push id =
    incr stack_height;
    Hashtbl.add env id !stack_height
  in
  let pop id =
    decr stack_height;
    Hashtbl.remove env id
  in

  let rec translate_exp e =
    match e with
	Const c -> N.Const c
      | Lval (lv, t) -> N.Lval (translate_lval lv, scalar_of_typ t)
      | AddrOf (lv, sz) -> N.AddrOf (translate_lval lv, sz)
      | AddrOfFun (f, ft) -> N.AddrOfFun (f, ft)
      | UnOp (op, e) -> N.UnOp (op, translate_exp e)
      | BinOp (op, e1, e2) -> N.BinOp (op, translate_exp e1, translate_exp e2)

  and translate_lval lv =
    match lv with
	Local v -> 
	  let x = Hashtbl.find env v in
	    N.Local (!stack_height - x)
      | Global x -> N.Global x
      | Deref (e, sz) -> N.Deref (translate_exp e, sz)
      | Shift (lv, e) -> N.Shift (translate_lval lv, translate_exp e)
  in

  let translate_set (lv, e, t) =
    match (t, e) with
	(N.Scalar t, _) -> N.Set (translate_lval lv, translate_exp e, t)
      | (N.Region (_, n), Lval (lv', _)) -> 
	  N.Copy (translate_lval lv, translate_lval lv', n)
      | _ -> 
	  Npkcontext.report_error "Hpk2npk.translate_set" 
	    "translate_set not implemented yet"
  in

  let translate_fn x =
    match x with
	FunId f -> N.FunId f
      | FunDeref (e, ft) -> N.FunDeref (translate_exp e, ft)
  in
    
  let translate_token x =
    match x with
	SymbolToken c -> N.SymbolToken c
      | IdentToken s -> N.IdentToken s
      | LvalToken lv -> N.LvalToken (translate_lval lv)
      | CstToken c -> N.CstToken c
  in

  let translate_assertion x = List.map translate_token x in
  (* TOOD: find a way to factor prefix_args and suffix_rets!! *)
  let prefix_args fid loc f args =
    let rec add i args =
      match args with
	  (e::args, t::args_t) -> 
	    push tmp_var;
	    let set = translate_set (Local tmp_var, e, t) in
	    let call = add (i+1) (args, args_t) in
	    let x = fid^".arg"^(string_of_int i) in
	      pop tmp_var;
	      N.Decl (x, t, (set, loc)::(call, loc)::[])
	| _ -> N.Call (translate_fn f)
    in
      add 1 args
  in

  let suffix_rets fid loc f args rets =
    let rec add rets =
      match rets with
	  (* TODO: should have one list instead of two here!!! *)
	  (lv::rets, t::rets_t) -> 
	    push tmp_var;
	    let e = Lval (Local tmp_var, t) in
	    let set = translate_set (lv, e, t) in
	    let call = add (rets, rets_t) in
	    let x = "value_of_"^fid in
	      pop tmp_var;
	      N.Decl (x, t, (call, loc)::(set, loc)::[])
	| _ -> prefix_args fid loc f args
    in
    let rec add_fst rets =
      match rets with
	  ((Local v)::rets, _::rets_t) 
	    when Hashtbl.find env v = !stack_height -> 
	      add_fst (rets, rets_t)
	| _ -> add rets
    in
      add_fst rets
  in

  let rec translate_blk x = List.map translate_stmt x 
  
  and translate_stmt (x, loc) = (translate_stmtkind loc x, loc)

  and translate_stmtkind loc x = 
    match x with
	(* Here: hypothesis that arguments are well typed 
	   and that 
	*)
	(* TODO: instead of having a ftyp, have the list of typed and named args
	   and the list of typed and named rets?? *)
	Call (args, (args_t, rets_t), f, rets) -> 
	  let fid = 
	    match f with
		FunId fid -> fid
	      | FunDeref _ -> "fptr_call"
	  in
	    (* TODO: change ret_typ in ftyp so that it is a list *)
	  let rets_t =
	    match rets_t with
		None -> []
	      | Some t -> t::[]
	  in
	    suffix_rets fid loc f (args, args_t) (rets, rets_t)
      | DoWith (body, lbl, action) -> 
	  let body = translate_blk body in
	  let action = translate_blk action in
	    N.DoWith (body, lbl, action)
      | Goto lbl -> N.Goto lbl
      | Decl (x, t, vid, body) -> 
	  push vid;
	  let body = translate_blk body in
	    pop vid;
	    N.Decl (x, t, body)
      | Set (lv, e, t) -> translate_set (lv, e, t)
      | Select (body1, body2) -> 
	  let body1 = translate_blk body1 in
	  let body2 = translate_blk body2 in
	    N.Select (body1, body2)
      | Guard e -> N.Guard (translate_exp e)
      | InfLoop body -> 
	  let body = translate_blk body in
	    N.InfLoop body
      | UserSpec x -> N.UserSpec (translate_assertion x) 
  in

  let translate_init_cell (sz, t, e) = (sz, t, translate_exp e) in

  let translate_fundec f (ret_ids, arg_ids, ft, body) =
    List.iter push ret_ids;
    List.iter push arg_ids;
    let body = translate_blk body in
      List.iter pop arg_ids;
      List.iter pop ret_ids;
      Hashtbl.add fundecs f (ft, body)
  in

  let translate_global x (t, init, loc) =
    let init =
      match init with
	  Zero -> N.Zero
	| Init cells -> N.Init (List.map translate_init_cell cells)
    in
      Hashtbl.add globals x (t, init, loc)
  in
    
  let specs = List.map translate_assertion prog.specs in
    Hashtbl.iter translate_fundec prog.fundecs;
    Hashtbl.iter translate_global prog.globals;

    { 
      N.fnames = prog.fnames; 
      N.globals = globals;
      N.fundecs = fundecs;
      N.specs = specs;
      N.ptr_sz = prog.ptr_sz;
      N.mem_zones = prog.mem_zones;
    }

(* TODO: push back to linker and then to High2New
  and append_args loc args fid f =
    let rec append x args =
      match args with
	  (e::args, t::args_t) ->
	    let id = fresh_id () in
	      push id;
	      let lv = Var id in
	      let set = translate_set (lv, t, e) in
	      let t = translate_typ t in
	      let call = append (x+1) (args, args_t) in
	      let arg = fid^".arg"^(string_of_int x) in
		pop id;
		(K.Decl (arg, t, (set, loc)::call::[]), loc)
		  
	| _ -> 
	    let fn = translate_fn f in
	      (K.Call fn, loc)
    in
      append 1 args
*)
(*
  and translate_call loc ret ((args_t, ret_t), f, args) =
    let fid =
      match f with
	  Fname f -> f
	| _ -> "fptr_call"
    in
    let args = (args, args_t) in
      match (ret_t, ret) with
	  (Void, _) -> append_args loc args fid f
	| (_, Some (Var id)) when Hashtbl.find env id = !stack_height ->
	    append_args loc args fid f
	| _ ->
	    let t = translate_typ ret_t in
	    let id = fresh_id () in
	      push id;
	      let post = 
		match ret with
		    Some lv -> 
		      let e = Lval (Var id, ret_t) in
		      let set = translate_set (lv, ret_t, e) in
			(set, loc)::[]
		  | None -> []
	      in
	      let call = append_args loc args fid f in
		pop id;
		(K.Decl ("value_of_"^fid, t, call::post), loc)

		*)

(* TODO: then, dump to disk and if necessary: print 
      Newspeak.write output_file npk;
	
      Npkcontext.print_debug "File linked.";
      
      if !Npkcontext.verb_newspeak then begin
	print_endline "Newspeak output";
	print_endline "---------------";
	let npk = Newspeak.read output_file in
	  Newspeak.dump npk;
	  print_newline ()
      end
*)

(* TODO???
  let e = 
    if !Npkcontext.no_opt then e 
    else Newspeak.simplify_exp !Npkcontext.opt_checks e 
  in
*)
