(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2009  Sarah Zennou
  
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

  Sarah Zennou
  EADS Innovation Works - SE/IA
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: sarah (dot) zennou (at) eads (dot) net
*)

open Csyntax
  
let fresh_loc loc = loc
  
let fresh_lbl lbl = "goto."^lbl
  
let zero = Cst (Cir.CInt Newspeak.Nat.zero, uint_typ)
let one = Cst (Cir.CInt (Newspeak.Nat.of_int 1), uint_typ) 
  
let dummy_cond = one
  
let rec var_and_stmt_addition stmts =
  match stmts with 
      [] -> [], []
    | (s, l)::stmts -> 
	match s with 
	    Label lbl -> 
	      let l' = fresh_loc l in
	      let lbl' = fresh_lbl lbl in
	      let init = Data zero in
	      let vdecl = VDecl (lbl', uint_typ, false, false, Some init) in
	      let s' = Exp (Set (Var lbl', None, zero)) in
	      let vdecls', stmts' = var_and_stmt_addition stmts in
		vdecl::vdecls', (s, l)::(s', l')::stmts'
		  
	  | If (exp, if_stmts, else_stmts) ->
	      let if_vdecls', if_stmts' = var_and_stmt_addition if_stmts in
	      let else_vdecls', else_stmts' = var_and_stmt_addition else_stmts in
	      let vdecls', stmts' = var_and_stmt_addition stmts in
	      let s' = If (exp, if_stmts', else_stmts') in
		if_vdecls'@else_vdecls'@vdecls', (s', l)::stmts'
		  
	  | CSwitch (e, c_stmts, d_stmts) ->
	      let c_vdecls' = ref [] in
	      let c_stmts' = ref [] in
	      let add_cases (e, stmts, l) =
		let vdecls', stmts' = var_and_stmt_addition stmts in
		  c_vdecls' := !c_vdecls'@vdecls';
		  c_stmts' := (e, stmts', l)::!c_stmts'
	      in
		List.iter add_cases (List.rev c_stmts);
		let d_vdecls', d_stmts' = var_and_stmt_addition d_stmts in
		let switch = CSwitch (e, !c_stmts', d_stmts') in
		let vdecls', stmts' = var_and_stmt_addition stmts in
		  !c_vdecls'@d_vdecls'@vdecls', (switch, l)::stmts'
		    
	  | For (stmts1, e, stmts2, stmts3) ->
	      let vdecls2', stmts2' = var_and_stmt_addition stmts2 in
	      let vdecls', stmts' = var_and_stmt_addition stmts in
	      let new_for = For (stmts1, e, stmts2', stmts3) in
	      let vdecls' = vdecls2'@vdecls' in
		vdecls', (new_for, l)::stmts'
		  
	  | Block b_stmts ->
	      let b_vdecls', b_stmts' = var_and_stmt_addition b_stmts in
	      let vdecls', stmts' = var_and_stmt_addition stmts in
	      let block = Block b_stmts' in
		b_vdecls'@vdecls', (block, l)::stmts'
	  | _ -> 
	      let vdecls', stmts' = var_and_stmt_addition stmts in
		vdecls', (s, l)::stmts'
		  
		  
let conditional_goto_addition lo_tbl stmts =
  let rec add stmts =
    match stmts with
	[] -> []
      | (s, l)::stmts -> 
	  match s with
	      Goto lbl ->
		let _, (_, o) = Hashtbl.find lo_tbl lbl in
		  if o < l then 
		    (* goto is backward: conditional stmt is added *)
		    let e = dummy_cond in
		    let stmts' = add stmts in
		    let l' = fresh_loc l in 
		      (If (e, [s, l], []), l')::stmts'
		  else
		    (* goto is forward. Not stored *)
		    (s, l)::stmts
		      
	    | Block b_stmts ->
		let b_stmts' = add b_stmts in
		let stmts' = add stmts in
		let block' = Block b_stmts' in
		  (block', l)::stmts'
		    
	    | For (stmts1, e, stmts2, stmts3) ->
		let stmts2' = add stmts2 in
		let stmts' = add stmts in
		let for' = For(stmts1, e, stmts2', stmts3) in
		  (for', l)::stmts'
	    | _ -> 
		(s, l)::(add stmts)
  in add stmts
       
let level_and_offset_processing lbls stmts =
  (* fills the hashtable lbls with the levels and offset of the
     label and goto stmts of the statement list stmts *)
  let rec processing n stmts =
    match stmts with
	[] -> ()
      | (s, loc)::stmts ->
	  match s with
	      If (_, if_stmts, else_stmts) -> 
		processing (n+1) if_stmts;
		processing (n+1) else_stmts;
		processing n stmts
		  
	    | CSwitch (_, c_stmts, d_stmts) ->
		List.iter (fun (_, stmts, _) -> processing (n+1) stmts) c_stmts;
		processing (n+1) d_stmts;
		processing n stmts
		  
	    | For (_, _, stmts2, _) ->
		processing (n+1) stmts2;
		processing n stmts;
		  
	    | Block b_stmts -> 
		processing n b_stmts; processing n stmts
		
	    | Goto lbl -> begin 
		try 
		  let (gotos, (l', o')) = Hashtbl.find lbls lbl in
		    (* only backward goto (ie whose offset is less
		       than the one of the label are added. If the ast
		       is explored in the order of the CFG then this
		       check is useless: goto can only be backward *)
		    if o' = Newspeak.unknown_loc or loc > o' then
		      Hashtbl.replace lbls lbl ((n, loc)::gotos, (l', o'))
		    else
		      ()
		with
		    Not_found -> 
		      Hashtbl.add lbls lbl ([n, loc], (0, Newspeak.unknown_loc))
	      end;
		processing n stmts
		
	    | Label lbl -> begin
		try 
		  let (gotos, _) = Hashtbl.find lbls lbl in
		    Hashtbl.replace lbls lbl (gotos, (n, loc))
		with
		    Not_found ->
		      Hashtbl.add lbls lbl ([], (n, loc))
	      end;
		processing n stmts
	    
	    | _ -> processing n stmts
  in processing 0 stmts
       
       
       
let has_label stmts lbl =
  (* returns true if one of the statements is Label lbl *)
  let rec has stmts =
    match stmts with
	[] -> false
      | (stmt, _)::stmts -> 
	  match stmt with
	      Label lbl' -> if lbl' = lbl then true else has stmts
	    | _ -> has stmts
  in
    has stmts
      
let has_goto stmts lbl g_offset =
  (* returns true if one of the statement is goto lbl with g_offset as
     location *)
  let rec has stmts = 
    match stmts with
	[] -> false 
      | (stmt, l)::stmts ->
	  match stmt with
	      If (_, [Goto lbl', _], []) -> 
		if lbl' = lbl && l = g_offset then true else has stmts
	    | _ -> has stmts
  in
    has stmts
      
let search_lbl stmts lbl =
  (* returns true if one of the statement is the label lbl or
     contains a stmt where the label is nested *)
  let rec search stmts =
    match stmts with
	[] -> false
      | (stmt, _)::stmts ->
	  match stmt with 
	      Label lbl' when lbl' = lbl -> true
	    | If(_, if_blk, else_blk) -> (search if_blk) or (search else_blk) or (search stmts)
	    | For(_, _, blk, _) -> (search blk) or (search stmts)
	    | Block blk -> (search blk) or (search stmts)
	    | CSwitch (_, cases, blk) -> 
		(List.exists (fun (_, blk, _) -> search blk) cases) or (search blk) or (search stmts) 
	    | _ -> search stmts
  in
    search stmts
      
let related d stmts lbl g_offset =
  let rec direct stmts =
    if has_goto stmts lbl g_offset then 
      not (has_label stmts lbl)
    else
      if has_label stmts lbl then true
      else
	related direct stmts
	  
  and indirect stmts = 
    if (has_goto stmts lbl g_offset) or (has_label stmts lbl) then false 
    else related indirect stmts 
      
  and related p stmts = 
    match stmts with
	[] -> true
      | (stmt, _)::stmts ->
	  match stmt with
	      If(_, if_blk, else_blk) -> (p if_blk) or (p else_blk) 
		
	    | CSwitch (_, l, blk) ->
		(List.exists (fun (_, blk, _) -> p blk) l) or (p blk)
		  
	    | For(_, _, blk2, _) -> p blk2 
		
	    | _ -> p stmts
  in
  let p = if d then direct else indirect in related p stmts 
					      
let directly_related stmts lbl g_offset =
  (*returns true if the label and goto levels are directly related
    (see Def 4) *)
  related true stmts lbl g_offset
    
let indirectly_related stmts lbl g_offset =
  (* returns true if the goto and label stmts satisfy the indirectly
     relation (see Def 5) *)
  related false stmts lbl g_offset
    
let if_else_out stmts lbl level offset =
  (* returns the stmt list whose 'goto lbl' stmt has been deleted and
     the condition if (fresh_lbl lbl) goto this fresh lbl has been
     added at the right position (see fig 5) *)
  let rec out stmts =
    match stmts with
	[] -> [], [], 0
	  
      | (stmt, l)::stmts ->
	  match stmt with
	      If (e, [Goto lbl', _], []) ->
		if lbl' = lbl && l = !offset then begin
		  let lbl' = fresh_lbl lbl in 
		  let cond = Var lbl' in
		  let n_cond = Unop(Neg, cond) in
		  let stmt = Exp (Set (cond, None, e)) in
		  let if_goto = If(cond, [Goto lbl', l], []) in
		    level := !level-1; 
		    offset := l;
		    (stmt, l)::((If(n_cond, stmts, []), l)::stmts), [if_goto, l], 2
		end
		else
		  let stmts', cond, n = out stmts in
		    (stmt, l)::stmts', cond, n
		      
	    | Label lbl' -> 
		let n = if lbl' = lbl then 1 else 0 in
		let stmts', cond, n' = out stmts in
		  (stmt, l)::stmts', cond, (n+n')
		    
	    | _ -> let stmts', cond, n = out stmts in (stmt, l)::stmts', cond, n
  in
    out stmts
      
      
let rec out_switch_loop stmts lbl level offset =
  (* returns the stmt list whose 'goto lbl' stmt has been deleted
     and replaced by some stmt according to the algo of Figure 4
     (Moving a goto out of a switch) *)
  let rec out stmts =
    match stmts with 
	[] -> [], []
      | (stmt, l)::stmts ->
	  match stmt with
	      If (e, [Goto lbl', _], []) ->
		if lbl' = lbl && l = !offset then begin
		  let lbl' = fresh_lbl lbl in 
		  let lbl' = Var lbl' in
		  let stmt = Exp (Set (lbl', None, e)) in
		  let if_goto_in = If(lbl', [Break, l], []) in
		  let if_goto_out = If(lbl', [Goto lbl, l], []) in
		  let stmts', cond = out stmts in
		    level := !level-1;
		    offset := l;
		    (stmt, l)::((if_goto_in, l)::stmts'), (if_goto_out, l)::cond
		end
		else
		  let stmts', cond = out stmts in
		    (stmt, l)::stmts', cond
		    		      
	    | _ -> let stmts', cond = out stmts in (stmt, l)::stmts', cond
  in 
      out stmts
	
	
let outward stmts lbl level offset =
  (* moves the goto stmt with label lbl at location o: 
     - either until it becomes undireclty related, if goto and label stmts are in
     different branches of the same if stmt 
     - or until the goto becomes
     directly related to an if or switch containing label lbl otherwise*)
  let achieved n_if n_else =
    (n_if = 2 && n_else = 1) or (n_if = 1 && n_else = 2)
  in
  let rec search stmt l =
    match stmt with
	
	If (_, [Goto lbl', _], []) -> 
	  let n = if lbl = lbl' && !offset = l then 2 else 0 in
	    [stmt, l], false, n
	      
      | Label lbl' -> 
	  let n = if lbl = lbl' then 1 else 0 in
	    [stmt, l], false, n
	      
      | If (e, if_stmts, else_stmts) ->
	  let if_stmts', b, n_if = move if_stmts in
	    if b then 
	      (* movement is achieved *)
	      let _, _, n_else = move else_stmts in
		[If (e, if_stmts', else_stmts), l], true, (n_if+n_else)
	    else
	      let else_stmts', b, n_else = move stmts in
		if b then 
		  (* movement is achieved *)
		  let _, _, n_if = move if_stmts in
		    [If (e, if_stmts, else_stmts'), l], true, (n_if+n_else)
		else
		  let b = achieved n_if n_else in
		  let n = n_if+n_else in
		    if n_if >= 2 then
		      let if_stmts', cond', _ = if_else_out if_stmts' lbl level offset in
			(If(e, if_stmts', else_stmts'), l)::cond', b, n
		    else
		      let else_stmts', cond', _ = if_else_out else_stmts' lbl level offset in
			(If(e, if_stmts', else_stmts'), l)::cond', b, n
			  
      | Block stmts ->
	  let stmts', b, n = move stmts in [Block stmts', l], b, n
	      
      | CSwitch (e, cases, d_case) -> 
	  let rec iter cases =
	    match cases with
		[] -> [], [], true, 0
		  
	      | (e, stmts, l')::cases ->
		  let stmts1, b1, n1 = move stmts in
		  let stmts', cond1 = out_switch_loop stmts1 lbl level offset in
		  let t = (e, stmts', l') in
		  let cases', cond2, b2, n2 = iter cases in
		    t::cases', cond1@cond2, (b1 or b2), (n1+n2)
	  in
	  let cases', cond', b, c_n = iter cases in
	    if c_n >= 2 then
	      (* goto label is in one of the cases *)
	      let _, _, d_n = move d_case in
	      let switch' = CSwitch (e, cases', d_case), l in
		switch'::cond', b, (c_n+d_n)
	    else
	      (* is goto stmt in the default case ? *)
	      let d_case', b', d_n = move d_case in
	      let d_case', cond = out_switch_loop d_case' lbl level offset in
	      let switch' = CSwitch(e, cases, d_case'), l in
		switch'::(cond@cond'), b', (c_n+d_n)
		  
      | For(stmts1, e, stmts2, stmts3) ->
	  let stmts2', b, n = move stmts2 in
	  let stmts2', cond = out_switch_loop stmts2' lbl level offset in
	  let for' = For(stmts1, e, stmts2', stmts3) in
	    (for', l)::cond, b, n
	      
      | _ -> [stmt, l], false, 0

  and move stmts =
    match stmts with
	[] -> [], true, 0
      | (stmt, l)::stmts ->
	  let stmts', b, n = search stmt l in
	    (* stops as soon as the block containing the expected goto
	       is found and transformed *)
	    if b then 
	      let stmts', b, n = move stmts in 
		(stmt, l)::stmts', b, n
	    else 
	      (stmt, l)::stmts', b, n
  in 
  let stmts', _, _ = move stmts in stmts'
				     
				     
let avoid_break_capture stmts lwhile l = 
  let break_var = "break."^(Newspeak.string_of_loc l) in
  let rec search_break stmts =
    match stmts with 
	[] -> [], false
      | (stmt, l')::stmts ->
	  match stmt with
	      Break -> 
		let stmts', _ = search_break stmts in
		let set = Exp (Set( (Var break_var), None, one)) in
		  (set, l')::((stmt, l')::stmts'), true
		    
	    | If (e, if_blk, else_blk) ->
		let if_blk', b_if = search_break if_blk in
		let else_blk', b_else = search_break else_blk in
		  (If(e, if_blk', else_blk'), l')::stmts, (b_if or b_else)
		    
	    | _ -> 
		let stmts', b = search_break stmts in 
		  (stmt, l)::stmts', b
  in
  let stmts', b = search_break stmts in
    if not b then [], stmts', []
    else 
      let init = Data zero in
      let vdecl = VDecl (break_var, uint_typ, false, false, Some init) in
      let set = Exp (Set (Var break_var, None, zero)) in
      let if_blk = (set, lwhile)::[Break, lwhile] in
      let if' = If(Var break_var, if_blk, []) in
	[vdecl, lwhile], stmts', [if', lwhile]
	  
	  
let rec if_else_in lbl l e before cond if_blk else_blk g_offset =
  let lbl' = Var (fresh_lbl lbl) in
  let before' = If(Unop(Not, lbl'), before, []) in
  let lb = try snd (List.hd before) with Failure "hd" -> l in
  let set = Exp (Set (lbl', None, e)) in
    if has_label if_blk lbl then 
      begin
	let l' = try snd (List.hd if_blk) with Failure "hd" -> l in
	let if' = If(lbl', [Goto lbl, l'], []) in
	let if_blk' = (if', l')::if_blk in
	  g_offset := l';
	  let if_blk' = inward lbl g_offset if_blk' in
	  let if' = If(cond, if_blk', else_blk) in
	    [(set, lb); (before', lb); (if', l')]
      end
    else 
      begin
	let e = IfExp (Unop(Not, lbl'), e, zero) in
	let l' = try snd (List.hd else_blk) with Failure "hd" -> l in
	let if' = If(lbl', [Goto lbl, l'], []) in
	let else_blk' = (if', l')::else_blk in
	  g_offset := l';
	  let else_blk' = inward lbl g_offset else_blk' in
	  let if' = If(e, if_blk, else_blk') in
	    [(set, lb); (before', lb); (if', l')]     
      end
	
and loop_in lbl l e before cond blk1 blk2 blk3 g_offset =
  let lbl' = Var (fresh_lbl lbl) in
  let rec search_and_add stmts =
    match stmts with
	[] -> []
      | (stmt, l)::stmts ->
	  match stmt with
	      Label lb when lb = lbl -> 
		let set = Exp (Set(lbl', None, zero)) in
		  (stmt, l)::((set, l)::stmts)
	    | _ -> (stmt, l)::(search_and_add stmts)
  in
  let set = Exp (Set (lbl', None, e)) in
  let lb = try snd (List.hd before) with Failure "hd" -> l in
  let before' = If(Unop(Not, lbl'), before, []) in
  let e = IfExp(lbl', one, cond) in
  let if' = If(lbl', [Goto lbl, !g_offset], []) in
  let blk2' = search_and_add blk2 in
  let l' = try snd (List.hd blk2') with Failure "hd" -> l in
    g_offset := l';
    let blk2' = (if', l')::blk2' in 
    let blk2' = inward lbl g_offset blk2' in
    let for'= For(blk1, e, blk2', blk3) in
      [(set, lb) ; (before', lb) ; (for', l)]
	
and cswitch_in lbl l e before cond cases default g_offset =
  let lbl' = Var (fresh_lbl lbl) in
  let tswitch = "switch."^(Newspeak.string_of_loc l) in
  let declr = VDecl (tswitch, uint_typ, false, false, None) in
  let tswitch = Var tswitch in
  let set = Exp (Set (lbl', None, e)) in
  let lb = try snd (List.hd before) with Failure "hd" -> l in
  let rec search_lbl cases =
    match cases with 
	[] -> raise Not_found
      | (e, stmts, l)::cases -> 
	  if has_label stmts lbl then 
	    begin
	      let l' = snd (List.hd stmts) in
	      let if' = If(lbl', [Goto lbl, l'], []) in
	      let stmts' = (if', l)::stmts in
		g_offset := l;
		e, (e, stmts', l)::cases
	    end
	  else let exp, cases' = search_lbl cases in exp, (e, stmts, l)::cases'
  in
    try 
      let e_case, cases' = search_lbl cases in
      let last = try (snd (List.hd (List.rev before))) with Failure "hd" -> l in
      let set_if = Exp (Set(tswitch, None, cond)) in
      let set_else = Exp (Set(tswitch, None, e_case)) in
      let before' = before @ [set_if, last] in
      let if' = If(Unop(Not, lbl'), before', [set_else, l]) in
      let switch' = CSwitch(tswitch, cases', default) in
	[(set, lb) ; (declr, lb) ; (if', lb) ; (switch', l)]
    with Not_found ->
      invalid_arg "Goto_elimination.cswitch_in: label is in the default case"
	
and inward lbl g_offset stmts =
  let rec search_goto_cond stmts =
    match stmts with
	[] -> invalid_arg "Goto_elimination.search_goto_cond: goto has to be in that stmt list"
      | (stmt, l)::stmts ->
	  match stmt with
	      If(e, [Goto lbl', _], []) when lbl = lbl' && !g_offset = l -> e, stmts
	    | _ -> search_goto_cond stmts
  in
  let rec inward before stmts = 
    match stmts with
	[] -> invalid_arg "Goto_elimination.inward: label has to be in that stmt list"
      | (stmt, l)::stmts ->
	  if search_lbl [stmt, l] lbl then
	    let e, before' = search_goto_cond before in
	    let stmts' =
	      match stmt with
		  If (ie, if_blk, else_blk) -> 
		    if_else_in lbl l e before' ie if_blk else_blk g_offset 
		      
		| For (blk1, cond, blk2, blk3) -> 
		    loop_in lbl l e before' cond blk1 blk2 blk3 g_offset
		      
		| CSwitch (ce, cases, default) -> 
		    cswitch_in lbl l e before' ce cases default g_offset
		      
		| _ -> (stmt, l)::stmts (* goto and label are sibling *)
	    in
	      before'@stmts'@stmts
	  else 
	    let before' = (stmt, l)::before in inward before' stmts
  in inward [] stmts
       
       
let lifting_and_inward stmts lbl l_level g_level g_offset =
  let rec split_goto stmts =
    match stmts with
	[] -> [], []
      | (stmt, l)::stmts ->
	  match stmt with
	      If(_, [Goto lbl', _], _) when lbl' = lbl && !g_offset = l ->
		[], (stmt, l)::stmts
	    | _ ->
		  let blk, after = split_goto stmts in
		    (stmt, l)::blk, after

  in
  let rec search stmts =
    (* returns the triple of stmts (before, blk, after) where 
       -blk is the chunk of statements in stmts from the label to the
       stmt just before the desired goto; 
       -before is the chunk from the beginning to the stmt just
       before the label;
       -after is the chunk from the goto to the end*)
    match stmts with
	[] -> [], [], []
      | (stmt, l)::stmts ->
	  if search_lbl [stmt, l] lbl then
	    let blk, after = split_goto stmts in
	      [], (stmt, l)::blk, after
	  else 
	    let before, blk, after = search stmts in
	      (stmt, l)::before, blk, after
  in
  let first_if_cond stmts =
    match stmts with
	(If(e, _, _), _)::_ -> e
      | _ -> invalid_arg "Goto_elimination.first_if_cond: stmt list could not be of that form"
  in
  let rec lifting_and_inward  stmts = 
    if has_goto stmts lbl !g_offset then
      begin
	g_level := !g_level + 1;
	l_level := !l_level + 1;
	(* looking for the block between the label and the goto (label
	   is the first as it is a backward one) *)
	let before, blk, after = search stmts in
	  (* building additional stmts *)
	  (* assign the goto_lbl to false and add it to the beginning of
	     before *)
	let lbl' = Var (fresh_lbl lbl) in
	let stmt = Exp (Set (lbl', None, zero)) in
	let _, l_init = 
	  try List.hd before
	  with Failure "hd" -> List.hd blk
	in
	let before' = before@[stmt, l_init] in
	  
	(* add if (goto_lbl) goto lbl; at the beginning of blk *)
	let e = first_if_cond after in
	let _, l_if = List.hd blk (*never raises Failure as we
				    know the label is in the current stmts *) in
	let if' = If(lbl', [Goto lbl, l_if], []) in
	let blk' = (if', l_if)::blk in
	  g_offset := l_if;
	  (* add goto_lbl = cond to the end of blk and wrap blk with a loop *)
	  let set = Exp (Set (lbl', None, e)) in
	  let _, l_set = 
	    try List.hd after
	    with Failure "hd" -> List.hd (List.rev blk)
	  in
	  let blk' = blk'@[set, l_set] in
	  
	  (* the goto stmt is removed from the stmts in the chunk after *)
	  let after' = List.tl after in
	    
	  (* inward transformations on the blk chunk. We know that the
	     first stmt is the goto stmt and the second one contains
	     the label stmt *)
	  let blk' = inward lbl g_offset blk' in
	    
	  (* do-while loop *)
	  let blk' = [DoWhile(blk', lbl'), l_set] in
	    (before'@blk'@after'), true
      end
    else
      match stmts with
	  [] -> [], false
	| (stmt, l)::stmts -> 
	    match stmt with
		If(e, if_blk, else_blk) -> 
		  let if_blk', b = lifting_and_inward if_blk in
		  let else_blk', b' = 
		    if b then else_blk, b else lifting_and_inward else_blk 
		  in
		  let if' = If(e, if_blk', else_blk') in
		    (if', l)::stmts, (b or b')
		      
	      | CSwitch (e, cases, blk) ->
		  let rec iter cases =
		    match cases with 
			[] -> [], false
		      | (e, blk, l)::cases ->
			  let blk', b = lifting_and_inward blk in 
			  let c = (e, blk', l) in
			  let cases', b' = if b then cases, b else iter cases in
			    c::cases', (b or b')
		  in 
		  let cases', b = iter cases in
		  let blk', b' = if b then blk, b else lifting_and_inward blk in
		    [CSwitch (e, cases', blk'), l], (b or b')
		      
	      | For(blk1, e, blk2, blk3) ->
		  (* if the goto is in blk1 then it has been managed
		     in the if branch of this function *)
		  let blk2', b' = lifting_and_inward blk2 in
		  let l' = try snd (List.hd (List.rev blk2')) with Failure "hd" -> l in
		  let before, blk2', after = avoid_break_capture blk2' l l' in
		  let stmts' = before @ ((For(blk1, e, blk2', blk3), l)::after) in
		    stmts', b'   
		      
	      | _ -> let stmts', b = lifting_and_inward stmts in (stmt, l)::stmts', b
  in
    fst (lifting_and_inward stmts)
      
let sibling_elimination stmts lbl g_offset =
  let rec add_loop_delete_goto stmts =
    match stmts with
	[] -> 
	  invalid_arg ("Goto_elimination.sibling_elimination: implementation bug." ^ 
			 "Goto has to be in this stmts list")
      | (stmt, l)::stmts ->
	  match stmt with
	      If(e, [Goto lbl', _], []) when lbl' = lbl && l = g_offset -> e, [], stmts 
	    | _ -> 
		let e, blk, after = add_loop_delete_goto stmts in
		  e, (stmt, l)::blk, after
  in
  let rec search_lbl stmts =
    match stmts with
	[] -> []
      | (stmt, l)::stmts ->
	  match stmt with
	      Label lbl' when lbl' = lbl -> 
		let e, blk', after = add_loop_delete_goto stmts in
		let l' = try snd (List.hd (List.rev blk')) with Failure "hd" -> l in
		let before, blk', after' = avoid_break_capture blk' l l' in
		  before @ ( (DoWhile (blk', e), l)::(after'@after)) 
		    
	    | _ -> (stmt, l)::(search_lbl stmts)
  in
    search_lbl stmts
      
  
let elimination stmts lbl (gotos, lo) =
  (* moves all gotos of the given label lbl. lo is the pair
     (level, offset) of the label statement *)
  let l_level, l_offset = lo in
  let stmts = ref stmts in	
  let move goto =
    let l, o = goto in
    let g_offset = ref o in
    let g_level = ref l in
      
      (* force goto and label to be directly related *)
      if indirectly_related !stmts lbl !g_offset then
	  stmts := outward !stmts lbl g_level g_offset;
      
      (* force goto and label to be siblings *)
      if directly_related !stmts lbl !g_offset then
	if !g_level > l_level then
	  stmts := outward !stmts lbl g_level g_offset
	else
	  begin
	    let l_level = ref l_level in
	      if !g_offset > l_offset then
		stmts := lifting_and_inward !stmts lbl l_level g_level g_offset
	  end;
      (* goto and label are sibling; eliminate goto and label *)
      stmts := sibling_elimination !stmts lbl !g_offset;
  in
    List.iter move gotos;
    !stmts
        
let run prog =
  let elimination lbls stmts =
    (* goto elimination *)
    let stmts = ref stmts in
    let move lbl g = 
      stmts := elimination !stmts lbl g
    in
      Hashtbl.iter move lbls;
      !stmts
  in
  let in_fun_elimination lbls stmts =
    (* adding a fresh boolean variable to each label stmt *)
    let vdecls', stmts' = var_and_stmt_addition stmts in
      (* computing offset and level for each pair of goto/label statement *)
      level_and_offset_processing lbls stmts;
      (* making all goto stmt conditional *)
    let stmts' = conditional_goto_addition lbls stmts' in
      if vdecls' = [] then 
	(* no label found *)
	stmts' 
      else 
	let (_, loc) = List.hd stmts' in 
	let vdecls' = List.map (fun vdecl -> (vdecl, loc)) vdecls' in
	let stmts' = vdecls'@stmts' in
	  (* processing goto elimination *)
	  elimination lbls stmts'
  in
  let lbls = Hashtbl.create 30 in
  let rec run prog = 
    match prog with
	[] -> []
      | (g, l)::prog -> 
	  match g with
	      FunctionDef (s, t, b, stmts) -> 
		let stmts' = in_fun_elimination lbls stmts in
		let g' = FunctionDef (s, t, b, stmts') in
		  Hashtbl.clear lbls;
		  (g', l)::(run prog)
	    | _ -> (g, l)::(run prog)
  in 
    run prog
