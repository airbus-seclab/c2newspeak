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
  
let fresh_lbl lbl = "goto."^lbl

let goto_lbl lbl g_offset = lbl ^ "." ^ g_offset

let del_goto_suffix lbl = 
  try 
    let i = String.index lbl '.' in String.sub lbl 0 i
  with Not_found -> lbl
 
let zero = Cst (Cir.CInt Newspeak.Nat.zero, uint_typ)
let one = Cst (Cir.CInt (Newspeak.Nat.of_int 1), uint_typ) 
  
let dummy_cond = one
 
let nested_gotos stmt = 
  (* returns the label list of all nested gotos in the stmtkind stmt *)
  let rec concat blk =
    let blk' = List.map (fun (stmt, _) -> nested_gotos stmt) blk in
    List.concat blk'
  and nested_gotos stmt =
    match stmt with 
	Goto lbl -> [lbl]
      | For(_, _, blk2, _) -> concat blk2
      | DoWhile(blk, _) -> concat blk
      | If(_, if_blk, else_blk) -> concat (if_blk @ else_blk)
      | Block blk -> concat blk
      | CSwitch(_, cases, default) ->
	let cases = List.concat (List.map (fun (_, blk, _) -> blk) cases) in
	  concat (cases @ default)
    | _ -> []
  in nested_gotos stmt
	
let preprocessing lbls stmts =
  (* For every forward goto stmt:
     - adds conditional goto statement
     - adds the additional boolean variables 
     - fills the level/offset table. Goto label are slightly
     modified to make them unique 

A goto is forward if there exists a statement sequence
stmt_1...stmt_i ... stmt_j stmt_n s.t. 
- i < j 
- stmt_i contains a nested stmt 'goto lbl'
- stmt_j is the label stmt 'Label lbl'*)
  let nth = ref 0 in
  let marking stmt lbls =
    let rec fold blk =
      let f blk (stmt, l) =
	let stmt' = mark stmt in (stmt', l)::blk
      in
	List.rev (List.fold_left f [] blk)
	  
    and mark stmt = 
      match stmt with
	  Goto lbl ->
	    if List.exists (fun lbl' -> lbl = lbl') lbls then
	      let lbl' = goto_lbl lbl "forward" in Goto lbl' 
	    else stmt

	| DoWhile(blk, e) ->
	    let blk' = fold blk in DoWhile(blk', e)
				     
	| For(blk1, e, blk2, blk3) ->
	    let blk2' = fold blk2 in For(blk1, e, blk2', blk3)
				      			       
	| CSwitch(e, cases, default) ->
	    let add cases (e, blk, l) =
	      let blk' = fold blk in (e, blk', l)::cases
	    in 
	    let cases' = List.rev (List.fold_left add [] cases) in
	    let default' = fold default in 
	      CSwitch(e, cases', default')
		
	| If(e, if_blk, else_blk) ->
	    let if_blk' = fold if_blk in
	    let else_blk' = fold else_blk in
	      If(e, if_blk', else_blk')
 
	| Block blk -> let blk' = fold blk in Block blk'
	| _ -> stmt

    in mark stmt
  in
  let rec forward_marking stmts =
    match stmts with
	[] -> []
      | (stmt, l)::stmts ->
	  let n_lbls = nested_gotos stmt in
	  let rec add stmts =
	    match stmts with
		[] -> []
	      | (stmt, _)::stmts ->
		  match stmt with 
		      Label lbl ->
			if List.exists (fun n_lbl -> lbl = n_lbl) n_lbls then 
			    lbl::(add stmts)
			else add stmts
		    | Block blk -> 
			(add blk)@(add stmts)
		    | _ -> add stmts
	  in
	  let lbls = add stmts in
	  let stmts' = forward_marking stmts in
	  let stmt' = if lbls = [] then stmt else marking stmt lbls in
	    (stmt', l)::stmts'
  in
  let cond_addition stmts =
    let rec add level stmts =
    match stmts with
	[] -> []
      | (stmt, l)::stmts ->
	  match stmt with
	      Goto lbl -> begin
		try
		  let len = String.index lbl '.' in
		    (* goto is forward. Its label is set back to its
		       original value *)
		  let lbl = String.sub lbl 0 len in
		    (Goto lbl, l)::(add level stmts)
		with
		    Not_found ->
		      let o = string_of_int !nth in
		      let lbl' = goto_lbl lbl o in
			begin
			  try 
			    let (gotos, (l', o')) = Hashtbl.find lbls lbl in
			      Hashtbl.replace lbls lbl ((level, o, l)::gotos, (l', o'));
			  with Not_found ->
			    Hashtbl.add lbls lbl ([level, o, l], (0, Newspeak.unknown_loc))
			end;
			nth := !nth + 1;
			let if' = If(dummy_cond, [Goto lbl', l], []) in
			  (if', l)::(add level stmts)
	      end

	    | Label lbl -> begin
		try
		  let (gotos, _) = Hashtbl.find lbls lbl in
		    Hashtbl.replace lbls lbl (gotos, (level, l))
		with
		    Not_found -> Hashtbl.add lbls lbl ([], (level, l))
	      end;
		(stmt, l)::(add level stmts)

	    | If(e, if_blk, else_blk) ->
		let level' = level+1 in
		let if_blk' = add level' if_blk in
		let else_blk' = add level' else_blk in
		let stmts' = add level stmts in
		  (If(e, if_blk', else_blk'), l)::stmts'
		    
	    | DoWhile(blk, e) ->
		let blk' = add (level+1) blk in
		  (DoWhile(blk', e), l)::(add level stmts)
		    
	    | Block blk -> 
		let blk' = add level blk in
		  (Block blk', l)::(add level stmts)
		    
	    | For(blk1, e, blk2, blk3) ->
		(* we suppose that only blk2 may contain goto stmts *)
		let blk2' = add (level+1) blk2 in
		  (For (blk1, e, blk2', blk3), l)::(add level stmts)
		    
	    | CSwitch(e, cases, default) ->
		let level' = level+1 in
		let add_cases cases (e, blk, l) =
		  let blk' = add level' blk in
		    (e, blk', l)::cases
		in
		let cases' = List.rev (List.fold_left add_cases [] cases) in
		let default' = add level' default in
		  (CSwitch(e, cases', default'), l)::(add level stmts)
		    
	    | _ -> (stmt, l)::(add level stmts)
    in
      add 0 stmts
  in
    if stmts = [] then [], []
    else
      let (_, l) = List.hd stmts in
      let decl lbl =
	let lbl' = fresh_lbl lbl in
	let init = Data zero in
	let vdecl = VDecl (lbl', uint_typ, false, false, Some init) in
	  vdecl, l
      in   
      let stmts' = forward_marking stmts in
      let stmts' = cond_addition stmts' in
      let lbl' = ref [] in
	Hashtbl.iter (fun lbl (gotos, _) -> 
			if gotos = [] then Hashtbl.remove lbls lbl 
			else lbl' := lbl::!lbl') lbls;
	let lbl' = List.rev !lbl' in
	let vdecls = List.map decl lbl' in
	  vdecls, stmts'
      
		         
let goto_equal lbl lbl' g_offset = compare lbl' (lbl^"."^ g_offset) = 0
    
let has_label stmts lbl =
  (* returns true if one of the statements is Label lbl *)
  let rec has stmts =
    match stmts with
	[] -> false
      | (stmt, _)::stmts -> 
	  match stmt with
	      Label lbl' when lbl' = lbl -> true
	    | Block blk -> (has blk) or (has stmts)
	    | _ -> has stmts
  in
    has stmts

let has_goto stmts lbl g_offset =
  (* returns true if one of the statement is the desired goto *)
  let rec has stmts = 
    match stmts with
	[] -> false 
      | (stmt, _)::stmts ->
	  match stmt with
	      If (_, [Goto lbl', _], []) when goto_equal lbl lbl' g_offset -> true
	    | Block blk -> (has blk) or (has stmts)
	    | _ -> has stmts
  in has stmts
      
type lblPos = No | In | Nested

let search_lbl stmts lbl =
 (* returns true if one of the statement is the label lbl or
     contains a stmt where the label is nested *)
  let rec search stmts =
  match stmts with
      [] -> false
    | (stmt, _)::stmts -> 
	match stmt with 
	    Label lbl' when lbl = lbl' -> true
	  | If(_, if_blk, else_blk) -> 
	      (search if_blk) or (search else_blk) or (search stmts)
		
	  | For(_,_,blk,_) -> (search blk) or (search stmts)
	
	  | DoWhile(blk, _) -> (search blk) or (search stmts)
	
	  | Block blk -> (search blk) or (search stmts)
	      
	  | CSwitch (_, cases, default) ->
	      (List.exists (fun (_, blk, _) -> search blk) cases) 
	      or (search default) or (search stmts)
		
	  | _ -> search stmts
  in search stmts
		
exception Indirect
exception Direct
exception Sibling

let rec related stmts lbl g_offset =
  let rec goto_or_label previous =
    match previous with
	In -> raise Sibling
      | Nested -> raise Direct
      | No -> In
    and loops previous p stmts =
    match previous, p with
	Nested, In -> raise Indirect
      | Nested, Nested -> raise Indirect
      | Nested, No -> related previous stmts
      | In, In -> raise Direct
      | In, Nested -> raise Direct
      | In, No -> related previous stmts
      | No, p ->
	  let p = if p = In then Nested else p in 
	    related p stmts
    and related previous stmts =
    match stmts with
	[] -> previous
      | (stmt, _)::stmts -> 
	  match stmt with
	      If(_, [Goto lbl', _], []) when goto_equal lbl lbl' g_offset ->
		let p = goto_or_label previous in related p stmts
						    
	    | Label lbl' when lbl = lbl' -> 
		let p = goto_or_label previous in related p stmts
						    
	    | For(_, _, blk, _) -> 
		let p = related No blk in 
		  loops previous p stmts
					    
	    | DoWhile(blk, _) ->
		let p = related No blk in loops previous p stmts
					    
	    | Block blk -> 
		let p = related previous blk in related p stmts
	
	    | If(_, if_blk, else_blk) ->
		let p = related No if_blk in
		let p' = related No else_blk in
		if p = No then loops previous p' stmts
		else 
		  if p' = No then loops previous p stmts else raise Indirect
		  
	    | CSwitch (_, cases, default) ->
		let rec rel cases =
		  match cases with
		      [] -> 
			let p = related No default in loops previous p stmts

		    | (_, blk, _)::cases -> 
			let p = related No blk in 
			if p = No then rel cases else loops previous p stmts
		in rel cases

	    | _ -> related previous stmts
  in related No stmts 
       
let directly_related stmts lbl g_offset =
  (*returns true if the label and goto levels are directly related *)
  try 
    let _ = related stmts lbl g_offset in 
      invalid_arg "Goto_elimination.directly_related: goto and label not found"
  with 
      Direct -> true 
    | Indirect | Sibling -> false 
  
    
let indirectly_related stmts lbl g_offset =
  (* returns true if the goto and label stmts are indirectly related *)
  try 
    let _ = related stmts lbl g_offset in
      invalid_arg "Goto_elimination.indirectly_related: goto and label not found"
  with
      Indirect -> true
    | Direct | Sibling -> false

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
		let stmts', b = search_break stmts in (stmt, l)::stmts', b
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

let optimizable before blk lbl =
  if before = [] then
    let (stmt, _) = List.hd blk in
      match stmt with
	  Label lbl' when lbl' = lbl -> true
	| _ -> false
  else false

let sibling_elimination stmts lbl g_offset =
  let rec add_loop_delete_goto stmts =
    match stmts with
	[] ->
	  invalid_arg ("Goto_elimination.sibling_elimination: implementation bug. " ^
			 "Goto has to be in this stmts list")
      | (stmt, l)::stmts ->
	  match stmt with
	      If(e, [Goto lbl', _], []) when goto_equal lbl lbl' g_offset -> e, [], stmts
	    | _ ->
		let e, blk, after = add_loop_delete_goto stmts in
		  e, (stmt, l)::blk, after
  in
  let rec backward stmts =
    match stmts with
	[] -> []
      | (stmt, l)::stmts ->
	  match stmt with
	      Label lbl' when lbl' = lbl ->
		let e, blk', after = add_loop_delete_goto ((stmt,l)::stmts) in
		let l' = try snd (List.hd (List.rev blk')) with Failure "hd" -> l in
		let before, blk', after' = avoid_break_capture blk' l l' in
		  before @ ( (DoWhile (blk', e), l)::(after'@after))
		    
	    | _ -> (stmt, l)::(backward stmts)
  in
  let rec split_lbl stmts =
    match stmts with
	[] -> [], []
      | (stmt, l)::stmts ->
	  if has_label [stmt, l] lbl then [], (stmt, l)::stmts
	  else
	    let before, blk = split_lbl stmts in
	      (stmt, l)::before, blk
  in
  let rec forward stmts =
    match stmts with
	[] -> []
      | (stmt, l)::stmts ->
	  match stmt with
	      If(e, [Goto lbl', _], []) when goto_equal lbl lbl' g_offset ->
		let before, blk = split_lbl stmts in
		  if optimizable before blk lbl then 
		    (*optimization: nothing between the goto and the
		      label. It is simply deleted *) 
		    stmts 
		  else 
		    if before = [] then blk
		    else
		    let if' = If (Unop(Not, e), before, []) in
		      (if', l)::blk
	    | _ -> (stmt, l)::(forward stmts)
  in
  let rec choose stmts =
    match stmts with
	[] -> [], false
      | (stmt, l)::stmts ->
	  match stmt with
	      If(_, [Goto lbl', _], []) when goto_equal lbl lbl' g_offset ->
		let stmts' = (stmt, l)::stmts in forward stmts', true

	    | Label lbl' when lbl' = lbl -> 
	      let stmts' = (stmt, l)::stmts in backward stmts', true

	    | Block blk -> 
		let blk', b = choose blk in 
		if b then 
		  (Block blk', l)::stmts, true
		else 
		  let stmts', b' = choose stmts in (stmt, l)::stmts', b'

	    | If(e, if_blk, else_blk) -> 
		let if_blk', b = choose if_blk in 
		  if b then
		    (If(e, if_blk', else_blk), l)::stmts, true
		  else
		    let else_blk', b' = choose else_blk in
		      if b' then 
			(If(e, if_blk, else_blk'), l)::stmts, true
		      else
			let stmts', b' = choose stmts in (stmt, l)::stmts', b'

	    | For(blk1, e, blk2, blk3) ->
		let blk2', b = choose blk2 in 
		  if b then 
		    (For(blk1, e, blk2', blk3), l)::stmts, true
		  else 
		    let stmts', b' = choose stmts in (stmt, l)::stmts', b'

	    | DoWhile(blk, e) ->
		let blk', b = choose blk in 
		  if b then 
		    (DoWhile(blk', e), l)::stmts, true
		  else 
		    let stmts', b' = choose stmts in (stmt, l)::stmts', b'

	    | CSwitch(e, cases, default) ->
		let rec iter cases =
		  match cases with
		      [] -> [], false
		    | (e, stmts, l)::cases -> 
			let stmts', b = choose stmts in 
			  if b then (e, stmts', l)::cases, true
			  else
			    let cases', b' = iter cases in (e, stmts', l)::cases', b'
		in
		let cases', b' = iter cases in
		  if b' then (CSwitch(e, cases', default), l)::stmts, b'
		  else
		    let default', b' = choose default in 
		      if b' then (CSwitch(e, cases, default'), l)::stmts, b' 
		      else 
			let stmts', b' = choose stmts in (stmt, l)::stmts', b'

	    | _ -> let stmts', b' = choose stmts in (stmt, l)::stmts', b'
  in
    fst (choose stmts)

let out_if_else stmts lbl level g_offset =
  (* returns the stmt list whose 'goto lbl' stmt has been deleted and
     the condition if (fresh_lbl lbl) goto this fresh lbl has been
     added at the right position (see fig 5) *)
  let rec out stmts =
    match stmts with
	[] -> [], []	  
      | (stmt, l)::stmts ->
	  match stmt with
	      If (e, [Goto lbl', l'], []) when goto_equal lbl lbl' g_offset ->
		  let lbl = fresh_lbl lbl in 
		  let cond = Var lbl in
		  let n_cond = Unop(Neg, cond) in
		  let stmt = Exp (Set (cond, None, e)) in
		  let if_goto = If(cond, [Goto lbl', l'], []) in
		    level := !level-1; 
		    if stmts = [] then
		      [stmt, l], [if_goto, l]
		    else
		      [(stmt, l) ; (If(n_cond, stmts, []), l)], [if_goto, l]

	    | Block blk -> let blk', cond = out blk in 
		if cond = [] then 
		  let stmts', cond = out stmts in
		    (Block blk, l)::stmts', cond
		else
		  (Block blk', l)::stmts, cond

	    | _ -> let stmts', cond = out stmts in (stmt, l)::stmts', cond
  in out stmts
      
      
let rec out_switch_loop stmts lbl level g_offset =
  (* returns the stmt list whose 'goto lbl' stmt has been deleted
     and replaced by some stmt according to the algo of Figure 4
     (Moving a goto out of a switch) *)
  let rec out stmts =
    match stmts with 
	[] -> [], []
      | (stmt, l)::stmts ->
	  match stmt with
	      If (e, [Goto lbl', l'], []) ->
		if goto_equal lbl lbl' g_offset then begin
		  let f_lbl = fresh_lbl lbl in 
		  let f_lbl = Var f_lbl in
		  let stmt = Exp (Set (f_lbl, None, e)) in
		  let if_goto_in = If(f_lbl, [Break, l'], []) in
		  let if_goto_out = If(f_lbl, [Goto lbl', l'], []) in
		  let stmts', cond = out stmts in
		    level := !level-1;
		    (stmt, l)::((if_goto_in, l)::stmts'), (if_goto_out, l)::cond
		end
		else
		  let stmts', cond = out stmts in (stmt, l)::stmts', cond
	    | Block blk ->
		let blk', cond = out blk in
		  if cond = [] then 
		    let stmts', cond = out stmts in
		      (Block blk, l)::stmts', cond
		  else
		    (Block blk', l)::stmts, cond    		      
	    | _ -> let stmts', cond = out stmts in (stmt, l)::stmts', cond
  in out stmts



let outward stmts lbl g_level g_offset =
  (* moves the goto stmt with label lbl at location o: 
     - either until the goto becomes direclty related to athe label, 
     if they are in different stmts
     - or until the goto becomes directly related to an if or switch
     containing label lbl otherwise*)
let rec loop_out builder e blk stmts l =
  if has_goto blk lbl g_offset then 
    let blk', after = out_switch_loop blk lbl g_level g_offset in
    let stmt' = builder e blk' in
		    (stmt', l)::(after @ stmts), 1
  else
    let blk', r = outward blk in
      if r = 1 then 
	let blk', after = out_switch_loop blk' lbl g_level g_offset in
	let stmt' = builder e blk' in 
	  (stmt', l)::(after @ stmts), r
      else
	if r = 2 then
	  let stmt' = builder e blk' in (stmt', l)::stmts, r
	else
	  let stmt = builder e blk in
	  let stmts', r = outward stmts in (stmt, l)::stmts', r

and outward stmts =
    match stmts with
	[] -> [], 0 
      | (stmt, l)::stmts ->
	  match stmt with
	      For (blk1, e, blk2, blk3) ->
		let builder e blk = For(blk1, e, blk, blk3) in loop_out builder e blk2 stmts l 

	    | DoWhile (blk, e) -> 
		let builder e blk = DoWhile(blk, e) in loop_out builder e blk stmts l 

	    | If (e, if_blk, else_blk) ->
		if has_goto if_blk lbl g_offset then
		  let if_blk', after = out_if_else if_blk lbl g_level g_offset in
		    (* are goto and label stmts in different branches of this If ? *)
		  let r = if search_lbl else_blk lbl then 2 else 1 in
		  let if' = If(e, if_blk', else_blk), l in
		    if'::(after @ stmts), r
		else
		  (* same process for the else branch *)
		  if has_goto else_blk lbl g_offset then
		    let else_blk', after = out_if_else else_blk lbl g_level g_offset in
		    let r = if search_lbl if_blk lbl then 2 else 1 in 
		    let if' = If(e, if_blk, else_blk'), l in 
		      if'::(after @ stmts), r
		  else
		    (* neither if_blk nor else_blk branches contains
		       the goto stmt ; calls recursively outward on the
		       branches. If either of the two branches returns a
		       modified stmt list (ret code = 1) then the
		       if_else_out is performed on this branch *)
		    let if_blk', r = outward if_blk in
		      if r = 1 then
			let if_blk', after = out_if_else if_blk' lbl g_level g_offset in
			let if' = If(e, if_blk', else_blk), l in
			  if'::(after @ stmts), r
		      else
			if r = 2 then
			  let if' = If(e, if_blk', else_blk), l in 
			    if'::stmts, r
			else
			  let else_blk', r = outward else_blk in
			    if r = 1 then
			      let else_blk', after = out_if_else else_blk' lbl g_level g_offset in
			      let else' = If(e, if_blk, else_blk'), l in
				else'::(after @ stmts), r
			    else
			      if r = 2 then
				let if' = If(e, if_blk, else_blk'), l in
				  if'::stmts, r
			      else
				(* goto is neither in the if-branch
				   nor in the else-branch. outward is
				   applied on the rest of the stmt
				   list *)
				let stmts', r = outward stmts in
				  (stmt, l)::stmts', r

	    			    
	    | CSwitch (e, cases, default) ->
		let rec iter cases =
		  match cases with
		      [] -> [], [], 0
		    | (e, blk, l)::cases ->
			if has_goto blk lbl g_offset then
			  let blk', after = out_switch_loop blk lbl g_level g_offset in
			    (e, blk', l)::cases, after, 1
			else
			  let blk', r = outward blk in
			    if r = 0 then
			      let cases', after, r = iter cases in
				(e, blk, l)::cases', after, r
			    else
			      let blk', after = out_switch_loop blk' lbl g_level g_offset in
			      (e, blk', l)::cases, after, r
		in
		let cases', after, r = iter cases in
		  if r = 0 then
		    if has_goto default lbl g_offset then
		      let default', after = out_switch_loop default lbl g_level g_offset in
		      let cs = CSwitch(e, cases, default'), l in
			cs::(after @ stmts), 1
		    else
		      let default', r = outward default in
			if r = 0 then
			  let stmts', r = outward stmts in
			    (stmt, l)::stmts', r
			else
			  let cs = CSwitch(e, cases, default'), l in
			    cs::stmts, r
		  else
		    let cs = CSwitch(e, cases', default), l in 
		      cs::(after @ stmts), 1
	  	
	    | Block blk ->
		let blk', r = outward blk in 
		let stmts', r = if r = 0 then outward stmts else stmts, r in
		  (Block blk', l)::stmts', r

	    | _ -> let stmts', r = outward stmts in (stmt, l)::stmts', r	

 in fst (outward stmts)
      
      
let rec if_else_in lbl l e before cond if_blk else_blk g_offset g_loc =
  let lbl' = Var (fresh_lbl lbl) in
  let lb = try snd (List.hd before) with Failure "hd" -> l in
  let set = Exp (Set (lbl', None, e)) in
    if has_label if_blk lbl then 
      begin
	let cond = IfExp (lbl', cond, zero) in
	let l' = try snd (List.hd if_blk) with Failure "hd" -> l in
	let g_lbl = goto_lbl lbl g_offset in
	let if' = If(lbl', [Goto g_lbl, g_loc], []) in
	let if_blk' = (if', l')::if_blk in
	let if_blk' = inward lbl g_offset g_loc if_blk' in
	let if' = If(cond, if_blk', else_blk) in
	  if before = [] then 
	    [(set, lb); (if', l')]
	  else
	    let before' = If(Unop(Not, lbl'), before, []) in
	      [(set, lb); (before', lb); (if', l')]
      end
    else 
      begin
	let cond = IfExp (Unop(Not, lbl'), cond, zero) in
	let l' = try snd (List.hd else_blk) with Failure "hd" -> l in
	let g_lbl = goto_lbl lbl g_offset in
	let if' = If(lbl', [Goto g_lbl, g_loc], []) in
	let else_blk' = (if', l')::else_blk in
	let else_blk' = inward lbl g_offset g_loc else_blk' in
	let if' = If(cond, if_blk, else_blk') in
	  if before = [] then 
	    [(set, lb); (if', l')]
	  else
	    let before' = If(Unop(Not, lbl'), before, []) in
	      [(set, lb); (before', lb); (if', l')]     
      end

and loop_in lbl l e before cond blk g_offset g_loc b =
  (* b is boolean param true when the expression to generate is for a
     While loop and false for a DoWhile loop *)
  let lbl' = Var (fresh_lbl lbl) in
  let rec search_and_add stmts =
    match stmts with
	[] -> []
      | (stmt, l)::stmts ->
	  match stmt with
	      Label lb when lb = lbl -> 
		let set = Exp (Set(lbl', None, zero)) in
		  (stmt, l)::((set, l)::stmts)

	    | Block blk -> 
		let blk' = search_and_add blk in 
		  if blk = blk' then (stmt, l)::(search_and_add stmts) 
		  else (Block blk', l)::stmts

	    | _ -> (stmt, l)::(search_and_add stmts)
  in
  let set = Exp (Set (lbl', None, e)) in
  let lb = try snd (List.hd before) with Failure "hd" -> l in
  let e = 
    if b then (* expression for While *) IfExp(lbl', one, cond) 
    else (* expression for DoWhile *)  cond 
  in
  let g_lbl = goto_lbl lbl g_offset in
  let if' = If(lbl', [Goto g_lbl, g_loc], []) in
  let blk' = search_and_add blk in
  let l' = try snd (List.hd blk') with Failure "hd" -> l in
  let blk' = (if', l')::blk' in
  let blk' = inward lbl g_offset g_loc blk' in
    if before = [] then 
      (* optimisation when there are no stmts before the loop *)
	[(set, lb)], blk', e
    else 
      let before' = If(Unop(Not, lbl'), before, []) in
	  [(set, lb) ; (before', lb)], blk', e
  

and while_in lbl l e before cond blk1 blk2 blk3 g_offset g_loc=
  let stmts', blk2', e = loop_in lbl l e before cond blk2 g_offset g_loc true in
    stmts'@[For(blk1, e, blk2', blk3), l]

and dowhile_in lbl l e before cond blk g_offset g_loc =
  let stmts', blk', e = loop_in lbl l e before cond blk g_offset g_loc false in
    stmts'@[DoWhile(blk', e), l]

and cswitch_in lbl l e before cond cases default g_offset g_loc =
  let lbl' = Var (fresh_lbl lbl) in
  let tswitch = "switch."^(Newspeak.string_of_loc l) in
  let declr = VDecl (tswitch, uint_typ, false, false, None) in
  let tswitch = Var tswitch in
  let set = Exp (Set (lbl', None, e)) in
  let lb = try snd (List.hd before) with Failure "hd" -> l in
  let set_if = Exp (Set(tswitch, None, cond)) in
  let last = try (snd (List.hd (List.rev before))) with Failure "hd" -> l in
  let before' = before @ [set_if, last] in
  let g_lbl = goto_lbl lbl g_offset in
  let if' = If(lbl', [Goto g_lbl, g_loc], []) in
  let rec search_lbl cases =
    match cases with 
	[] -> raise Not_found
      | (e, stmts, l)::cases -> 
	  if has_label stmts lbl then 
	      let stmts' = (if', l)::stmts in
	      let stmts' = inward lbl g_offset g_loc stmts' in
		e, (e, stmts', l)::cases
	  else let exp, cases' = search_lbl cases in exp, (e, stmts, l)::cases'
  in
    try 
      let e_case, cases' = search_lbl cases in
      
      let set_else = Exp (Set(tswitch, None, e_case)) in
      
      let if' = If(Unop(Not, lbl'), before', [set_else, l]) in
      let switch' = CSwitch(tswitch, cases', default) in
	[(set, lb) ; (declr, lb) ; (if', lb) ; (switch', l)]
    with Not_found ->
      let conds = List.map (fun (e, _, _) -> e) cases in
      let build e e' = IfExp(e, one, e') in
      let e_default = List.fold_left build zero conds in
      let e_default = Unop(Not, e_default) in
      let set_else = Exp (Set(tswitch, None, e_default)) in
      let default' = (if', l)::default in
      let default' = inward lbl g_offset g_loc default' in
      let if' = If(Unop(Not, lbl'), before', [set_else, l]) in
      let switch' = CSwitch(tswitch, cases, default') in
	[(set, lb) ; (declr, lb) ; (if', lb) ; (switch', l)]



	
and inward lbl g_offset g_loc stmts =
  let rec search_goto_cond stmts =
    match stmts with
	[] -> invalid_arg "Goto_elimination.search_goto_cond: goto has to be in that stmt list"
      | (stmt, _)::stmts ->
	  match stmt with
	      If(e, [Goto lbl', _], []) when goto_equal lbl lbl' g_offset -> e, stmts
	    | _ -> search_goto_cond stmts
  in
  let rec inward before stmts = 
    match stmts with
	[] -> invalid_arg "Goto_elimination.inward: label has to be in that stmt list"
      | (stmt, l)::stmts ->
	  if has_label [stmt, l] lbl then 
	    (* goto and label are sibling *) 
	    sibling_elimination (before@((stmt, l)::stmts)) lbl g_offset
	  else
	    if search_lbl [stmt, l] lbl then
	      let e, before' = search_goto_cond before in
	      let stmts' =
		match stmt with
		    If (ie, if_blk, else_blk) -> 
		      if_else_in lbl l e before' ie if_blk else_blk g_offset g_loc
			
		  | For (blk1, cond, blk2, blk3) -> 
		      while_in lbl l e before' cond blk1 blk2 blk3 g_offset g_loc
			
		  | DoWhile(blk, cond) ->
		      dowhile_in lbl l e before' cond blk g_offset g_loc

		  | CSwitch (ce, cases, default) -> 
		      cswitch_in lbl l e before' ce cases default g_offset g_loc

		  | Block blk -> 
		      let blk' = inward before blk in  [Block blk', l]

		  | _ -> (stmt, l)::stmts 
	      in
		before'@stmts'@stmts
	    else 
	      let before' = (stmt, l)::before in inward before' stmts
  in inward [] stmts
       
       
let rec lifting_and_inward stmts lbl l_level g_level g_offset g_loc =
  let rec split_goto stmts =
    match stmts with
	[] -> [], []
      | (stmt, l)::stmts ->
	  match stmt with
	      If(_, [Goto lbl', _], _) when goto_equal lbl lbl' g_offset ->
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
       -after is the chunk from the goto to the end *)
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
    if has_goto stmts lbl g_offset then
      begin
	g_level := !g_level + 1;
	l_level := !l_level + 1;
	(* looking for the block between the label and the goto (label
	   is the first as it is backward one *)
	let before, blk, after = search stmts in
	let lbl' = Var (fresh_lbl lbl) in
	  
	(* add if (goto_lbl) goto lbl; at the beginning of blk *)
	let e = first_if_cond after in
	let _, l_if = List.hd blk (*never raises Failure as we
				    know the label is in the current stmts *) in
	let g_lbl = goto_lbl lbl g_offset in
	let if' = If(lbl', [Goto g_lbl, g_loc], []) in
	let blk' = (if', l_if)::blk in
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
	  let blk' = inward lbl g_offset g_loc blk' in
	    
	  (* do-while loop *)
	  let blk' = [DoWhile(blk', lbl'), l_set] in
	    (before@blk'@after'), true
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
		  let blk2', b' = lifting_and_inward blk2 in
		    [For(blk1, e, blk2', blk3), l], b' 
		      
	      | DoWhile(blk, e) -> 
		  let blk', b' = lifting_and_inward blk in
		    [DoWhile(blk', e), l], b'

	      | Block blk ->
		  let blk', b = lifting_and_inward blk in
		  let stmt' = Block blk' in
		    if b then (stmt', l)::stmts, b
		    else
		      let stmts', b' = lifting_and_inward stmts in
			(stmt', l)::stmts', b'

	      | _ -> let stmts', b = lifting_and_inward stmts in (stmt, l)::stmts', b
  in
    fst (lifting_and_inward stmts)

  
let elimination stmts lbl (gotos, lo) =
  (* moves all gotos of the given label lbl. lo is the pair
     (level, offset) of the label statement *)
  let l_level, l_offset = lo in
  let stmts = ref stmts in	
  let move goto =
    let l, id, o = goto in
    let l = ref l in
      (* force goto and label to be directly related *)
      if indirectly_related !stmts lbl id then
	  stmts := outward !stmts lbl l id;

      (* force goto and label to be siblings *)
      if directly_related !stmts lbl id then begin
	if !l > l_level then begin
	  stmts := outward !stmts lbl l id;
	  stmts := sibling_elimination !stmts lbl id
	end
	else begin
	  let l_level = ref l_level in
	    if o > l_offset then
	      stmts := lifting_and_inward !stmts lbl l_level l id o
	end
      end
      else
	(* goto and label are sibling; eliminate goto and label *) 
	stmts := sibling_elimination !stmts lbl id
  in
    List.iter move gotos;
    !stmts

let rec deleting_goto_ids stmts =
  match stmts with
      [] -> []
    | (stmt, l)::stmts ->
	match stmt with
	    Goto lbl -> 
	      let lbl' = del_goto_suffix lbl in 
		(Goto lbl', l)::(deleting_goto_ids stmts) 
	  
	  | If(e, if_blk, else_blk) ->
	      let if_blk' = deleting_goto_ids if_blk in
	      let else_blk' = deleting_goto_ids else_blk in
		(If(e, if_blk', else_blk'), l)::(deleting_goto_ids stmts)

	  | Block blk -> 
	      let blk' = deleting_goto_ids blk in
		(Block blk', l)::(deleting_goto_ids stmts)

	  | For(blk1, e, blk2, blk3) ->
	      let blk1' = deleting_goto_ids blk1 in
	      let blk2' = deleting_goto_ids blk2 in
	      let blk3' = deleting_goto_ids blk3 in
		(For(blk1', e, blk2', blk3'), l)::(deleting_goto_ids stmts)

	  | CSwitch(e, cases, default) ->
	      let cases' = ref [] in
	      let add (e, blk, l) = 
		cases':= (e, deleting_goto_ids blk, l)::!cases'
	      in
		List.iter add cases; 
		let cases' = List.rev !cases' in
		let default' = deleting_goto_ids default in
		  (CSwitch(e, cases', default'), l)::(deleting_goto_ids stmts)

	  | DoWhile(blk, e) ->
	      let blk' = deleting_goto_ids blk in 
		(DoWhile(blk', e), l)::(deleting_goto_ids stmts)

	  | _ -> (stmt, l)::(deleting_goto_ids stmts)

let run prog =
  let elimination lbls stmts =
    (* goto elimination *)
    let stmts = ref stmts in
    let move lbl g = 
      stmts := elimination !stmts lbl g
    in
      Hashtbl.iter move lbls;
      deleting_goto_ids !stmts
      
  in
  let in_fun_elimination lbls stmts =
    (* adding a fresh boolean variable to each label stmt *)
    (* computing offset and level for each pair of goto/label statement *)
    (* making all goto stmt conditional *)
    let vdecls', stmts' = preprocessing lbls stmts in
      if vdecls' = [] then 
	(* no backward goto found *)
	stmts' 
      else 
	(* processing goto elimination *)
	let stmts' = vdecls'@stmts' in
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
