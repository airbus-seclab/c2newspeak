(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2009  Sarah Zennou, Charles Hymans
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
  EADS Innovation Works - SE/IS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: sarah (dot) zennou (at) eads (dot) net

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org
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

let preprocessing lbls stmts =
  (* For every goto stmt:
     - adds conditional goto statement
     - adds the additional boolean variables 
     - fills the level/offset table. Goto label are slightly
     modified to make them unique 
*)
  let nth = ref 0 in
  let cond_addition stmts =
    let rec add level stmts =
    match stmts with
	[] -> []
      | (stmt, l)::stmts ->
	  match stmt with
	      Goto lbl -> begin
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
    if stmts = [] then ([], [])
    else begin
      let (_, l) = List.hd stmts in
      let decl lbl =
	let lbl' = fresh_lbl lbl in
	let init = Data zero in
	let vdecl = 
	  LocalDecl (lbl', VDecl (uint_typ, false, false, Some init))
	in
	  (vdecl, l)
      in
      let stmts' = cond_addition stmts in
      let lbl' = ref [] in
	Hashtbl.iter (fun lbl (gotos, _) -> 
			if gotos = [] then Hashtbl.remove lbls lbl 
			else lbl' := lbl::!lbl') lbls;
	let lbl' = List.rev !lbl' in
	let vdecls = List.map decl lbl' in
	  (vdecls, stmts')
    end
		         

		
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
		let p = related No blk in loops previous p stmts
					    
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
		let rec rel previous cases =
		  match cases with
		      [] -> previous
		    | (_, blk, _)::cases -> 
			let p = related No blk in 
			if p = No then rel previous cases 
			else let p' = loops previous p [] in rel p' cases
		in
		let p = rel No cases in
		let p' = related No default in
		  if p = No then loops previous p' stmts
		  else 
		    if p' = No then loops previous p stmts else raise Indirect

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
    | Direct | Sibling ->false

let avoid_break_continue_capture stmts lwhile l g_offset vdecls = 
  let rec add stmts l var skind =
    let stmts', vars = search stmts in
    let set = Exp (Set( (Var var), None, one)) in
      (set, l)::((skind, l)::stmts'), (skind, var)::vars
  and search stmts =
    match stmts with 
	[] -> [], []
      | (stmt, l')::stmts ->
	  match stmt with
	      Break -> 
		let var = "break."^(Newspeak.string_of_loc l')^"."^g_offset in
		add stmts l' var Break
	
	    | Continue ->
		let var = "continue."^(Newspeak.string_of_loc l')^"."^g_offset in
		  add stmts l' var Continue

	    | If (e, if_blk, else_blk) ->
		let if_blk', if_vars = search if_blk in
		let else_blk', else_vars = search else_blk in
		  (If(e, if_blk', else_blk'), l')::stmts, (if_vars@else_vars)
		 
	    | Block blk ->
		let blk', blk_vars' = search blk in
		let stmts', vars' = search stmts in
		  (Block blk', l)::stmts', blk_vars'@vars'   
	    | _ -> 
		let stmts', vars = search stmts in (stmt, l)::stmts', vars
  in
  let stmts', vars = search stmts in
    if vars = [] then stmts', []
    else 
      let init = Data zero in
      let after = ref [] in 
      let add (skind, var) =
	let vdecl = 
	  LocalDecl (var, VDecl (uint_typ, false, false, Some init))
	in
	let set = Exp (Set (Var var, None, zero)) in
	let if_blk = (set, lwhile)::[skind, lwhile] in
	let if' = If (Var var, if_blk, []) in
	  vdecls := vdecl::!vdecls;
	  after := (if', lwhile)::!after
      in
	List.iter add vars;
	stmts', (List.rev !after)



  let rec extract_decls stmts =
    match stmts with
	[] -> [], []
      | (stmt, l)::stmts ->
	  let s_decls, stmts' = extract_decls stmts in
	    match stmt with
		LocalDecl _ -> 
		  let (decls, stmts') = extract_decls stmts in 
		    ((stmt, l)::decls, stmts')
	      | Block blk -> 
		  let b_decls, blk' = extract_decls blk in 
		    (b_decls@s_decls, (Block blk', l)::stmts')
	      | _ -> (s_decls, (stmt, l)::stmts')


exception Lbl
exception Gto 

let rec split_lbl stmts lbl = 
  match stmts with 
      [] -> [], []
    | (stmt, l)::stmts' ->
	match stmt with
	    Label lbl' when lbl = lbl' -> [], stmts
	  | Block blk -> 
	      if search_lbl blk lbl then 
		let before, blk' = split_lbl blk lbl in
		let before = if before = [] then [] else [Block before, l] in
		let blk' = if blk' = [] then [] else [Block blk', l] in 
		  before,  blk'@stmts'
	      else 
		let before, stmts' = split_lbl stmts' lbl in (stmt, l)::before, stmts'

	  | _ -> let before, stmts' = split_lbl stmts' lbl in (stmt, l)::before, stmts'

let sibling_elimination stmts lbl g_offset vdecls =
let rec direction stmts =
  match stmts with
      [] -> raise Not_found
    | (stmt, _)::stmts ->
	match stmt with
	    Label lbl' when lbl = lbl' -> raise Lbl
	  | If(_, [Goto lbl', _], []) when goto_equal lbl lbl' g_offset -> raise Gto 
	  | Block blk -> begin
	      try direction blk with Not_found -> direction stmts
	    end
	  | _ -> direction stmts
in
  let rec b_delete_goto stmts =
    match stmts with
	[] -> raise Not_found
      | (stmt, l)::stmts ->
	  match stmt with
	      If(e, [Goto lbl', _], []) when goto_equal lbl lbl' g_offset -> e, [], stmts
	    | Block blk -> begin
		try
		  let e, blk', after = b_delete_goto blk in
		  let decls, blk' = extract_decls blk' in
		  let blk' = blk' @ after in
		    if blk' = [] then e, decls, stmts
		    else
		      e, decls @ [Block blk', l], stmts
		with
		    Not_found -> 
		      let e, blk, after = b_delete_goto stmts in
			e, (stmt, l)::blk, after
	      end
	    | _ ->
		let e, blk, after = b_delete_goto stmts in
		  e, (stmt, l)::blk, after
  in
  let backward_wrap stmt l stmts =
    try
      let e, blk', after = b_delete_goto ((stmt,l)::stmts) in
      let l' = try snd (List.hd (List.rev blk')) with Failure "hd" -> l in
      let blk', after' = avoid_break_continue_capture blk' l l' g_offset vdecls in
	(DoWhile (blk', e), l)::(after'@after)
    with
	(* goto may have disappeared because of optimizations *)
	Not_found -> (stmt, l)::stmts
  in

  let rec backward stmts =
    match stmts with
	[] -> []
      | (stmt, l)::stmts ->
	  match stmt with
	      Label lbl' when lbl' = lbl -> backward_wrap stmt l stmts 
	    | Block blk when search_lbl blk lbl -> backward_wrap stmt l stmts
	    | _ -> (stmt, l)::(backward stmts)
  in
  
  let rec f_delete_goto stmts =
    match stmts with
	[] -> raise Not_found
      | (stmt, l)::stmts -> 
	  match stmt with
	      If(_, [Goto lbl', _], []) when goto_equal lbl lbl' g_offset ->
		stmts, (stmt, l)
	    | Block blk -> begin
		try
		  let blk', stmt' = f_delete_goto blk in (Block blk', l)::stmts, stmt'
		with
		    Not_found -> let stmts', stmt' = f_delete_goto stmts in (stmt, l)::stmts', stmt'
	      end
	    | _ -> let stmts', stmt' = f_delete_goto stmts in (stmt, l)::stmts', stmt'
  in

  let rec forward stmts = 
    match stmts with
	[] -> []
      | (stmt, l)::stmts ->
	  match stmt with
	      If(e, [Goto lbl', _], []) when goto_equal lbl lbl' g_offset -> 
		let before, after = split_lbl stmts lbl in
		let decls, before = extract_decls before in
		  if before = [] then decls@after
		  else
		    let if' = If(Unop(PureC.Not, e), before, []) in decls @ ((if', l)::after)
	    | Block blk -> begin
		try
		  let blk', stmt = f_delete_goto blk in
		  let decls, blk' = extract_decls blk' in
		  let stmts' = [Block blk', l] in
		  let stmts' = stmt::(decls @ stmts' @ stmts) in
		    forward stmts'
		with Not_found -> (stmt, l)::(forward stmts)
	      end
	    | _ -> (stmt, l)::(forward stmts)
  
  in
  let rec choose stmts =
    match stmts with
	[] -> [], false
      | (stmt, l)::stmts ->
	  match stmt with
	      If(_, [Goto lbl', _], []) when goto_equal lbl lbl' g_offset ->
		forward ((stmt, l)::stmts), true
	    | Label lbl' when lbl' = lbl -> 
	      let stmts' = (stmt, l)::stmts in backward stmts', true
	    | Block blk -> begin
		  try direction blk with 
		      Gto  -> forward ((stmt, l)::stmts), true 
		    | Lbl -> backward ((stmt, l)::stmts), true
		    | Not_found -> 
			let blk', b' = choose blk in 
			  if not b' then let stmts', b' = choose stmts in (stmt, l)::stmts', b'
			  else (Block blk', l)::stmts, true
	      end
	    | If(e, if_blk, else_blk) -> 
		let if_blk', b = choose if_blk in 
		  if b then (If(e, if_blk', else_blk), l)::stmts, true
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

let cond_equal cond e =
  match cond, e with
      Var v, Var v' when v = v' -> true
    | _, _ -> false

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
		  let n_cond = Unop(PureC.Not, cond) in
		  let stmt = 
		    if cond_equal cond e then [] else [Exp (Set (cond, None, e)), l] 
		  in
		  let if_goto = If(cond, [Goto lbl', l'], []) in
		    level := !level-1; 
		    if stmts = [] then
		      stmt, [if_goto, l]
		    else
		      stmt @ [(If(n_cond, stmts, []), l)], [if_goto, l]

	    | Block blk -> let blk', cond = out blk in 
		if cond = [] then 
		  let stmts', cond = out stmts in (Block blk, l)::stmts', cond
		else (Block blk', l)::stmts, cond

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
		  let stmt = 
		    if cond_equal f_lbl e then []
		    else [Exp (Set (f_lbl, None, e)), l] 
		  in
		  let if_goto_in = If(f_lbl, [Break, l'], []) in
		  let if_goto_out = If(f_lbl, [Goto lbl', l'], []) in
		  let stmts', cond = out stmts in
		    level := !level-1;
		    stmt @ ((if_goto_in, l)::stmts'), (if_goto_out, l)::cond
		end
		else
		  let stmts', cond = out stmts in (stmt, l)::stmts', cond
	    | Block blk ->
		let blk', cond = out blk in
		  if cond = [] then 
		    let stmts', cond = out stmts in (Block blk, l)::stmts', cond
		  else (Block blk', l)::stmts, cond    		      
	    | _ -> let stmts', cond = out stmts in (stmt, l)::stmts', cond
  in out stmts



let outward stmts lbl g_level g_offset =
  (* moves the goto stmt with label lbl at location o: 
     - either until the goto becomes direclty related to the label, 
     if they are in different stmts
     - or until the goto becomes directly related to an if or switch
     containing label lbl otherwise*)
  let rec out blk stmts f p =
    if has_goto blk lbl g_offset then
      if p blk lbl then blk, stmts, false
      else
	let blk', after = f blk lbl g_level g_offset in blk', (after @ stmts), true
    else 
      let blk', b = outward blk in blk', stmts, b

  and fold blk stmts f p =
    let blk', stmts', b = out blk stmts f p in
      if not b then 
	let stmts', b' = outward stmts in blk', stmts', b' 
      else 
	let blk', stmts', _ = fold blk' stmts' f p in blk', stmts', true
  
  and outward stmts =
    match stmts with
	[] -> [], false
      | (stmt, l)::stmts ->
	  match stmt with
	      For (blk1, e, blk2, blk3) ->  
		let blk2', stmts', b' = fold blk2 stmts out_switch_loop search_lbl in
		  (For(blk1, e, blk2', blk3), l)::stmts', b'
		    
	    | DoWhile (blk, e) -> 
		let blk', stmts', b' = fold blk stmts out_switch_loop search_lbl in
		  (DoWhile (blk', e), l)::stmts', b'
		    
	    | If (e, if_blk, else_blk) ->
		let rec if_fold if_blk stmts = 
		  let if_blk', stmts', b' = out if_blk stmts out_if_else search_lbl in
		    if not b' then 
		      let else_blk', stmts', b' = fold else_blk stmts out_if_else search_lbl in
		      if_blk', else_blk', stmts', b'
		    else 
		      let if_blk', else_blk', stmts', _ = if_fold if_blk' stmts' in
			if_blk', else_blk', stmts', true
		in
		let if_blk', else_blk', stmts', b' = if_fold if_blk stmts in
		    (If(e, if_blk', else_blk'), l)::stmts', b'
		  
	    | CSwitch(e, cases, default) ->
		let rec case_fold cases =
		  match cases with 
		      [] -> [], [], false
		    | (e, blk, l')::cases ->
			let blk', stmts, b' = out blk stmts out_switch_loop has_label in
			  if not b' then 
			    let cases', stmts', b' = case_fold cases in
			      (e, blk, l')::cases', stmts', b'
			  else 
			    let cases' = (e, blk', l')::cases in
			      cases', stmts, true
		in
		let cases', stmts', b' = case_fold cases in
		  if not b' then 
		    let default', stmts', b' = fold default stmts out_switch_loop has_label in
			  (CSwitch(e, cases, default'), l)::stmts', b'
		  else
		    (CSwitch(e, cases', default), l)::stmts', true

	    | Block blk -> 
		let blk', b' = outward blk in 
		  if not b' then 
		    let stmts', b' = outward stmts in
		      (stmt, l)::stmts', b'
		  else
		    (Block blk', l)::stmts, b'

	    | _ -> let stmts', b' = outward stmts in (stmt, l)::stmts', b'	

 in fst (outward stmts)
      
      

	      
let rec if_else_in lbl l e before cond if_blk else_blk g_offset g_loc =
  let lbl' = Var (fresh_lbl lbl) in
  let lb = try snd (List.hd before) with Failure "hd" -> l in
  let set = if cond_equal lbl' e then [] else [Exp (Set (lbl', None, e)), lb] in
    if search_lbl if_blk lbl then 
      begin
	let cond = Csyntax.and_bexp (normalize_bexp lbl') cond in
	let l' = try snd (List.hd if_blk) with Failure "hd" -> l in
	let g_lbl = goto_lbl lbl g_offset in
	let if' = If (lbl', [Goto g_lbl, g_loc], []) in
	let if_blk' = (if', l')::if_blk in
	let if_blk' = inward lbl g_offset g_loc if_blk' in
	let if' = If (cond, if_blk', else_blk) in
	let decls, before = extract_decls before in
	  if before = [] then 
	    set @ decls @ [if', l']
	  else
	    let before' = If (Unop(PureC.Not, lbl'), before, []) in
	      set @ decls @ [(before', lb); (if', l')]
      end
    else 
      begin
	let cond = Csyntax.and_bexp (normalize_bexp (Unop(PureC.Not, lbl'))) cond in
	let l' = try snd (List.hd else_blk) with Failure "hd" -> l in
	let g_lbl = goto_lbl lbl g_offset in
	let if' = If (lbl', [Goto g_lbl, g_loc], []) in
	let else_blk' = (if', l')::else_blk in
	let else_blk' = inward lbl g_offset g_loc else_blk' in
	let if' = If (cond, if_blk, else_blk') in
	let decls, before = extract_decls before in
	  if before = [] then 
	    set @ decls @ [if', l']
	  else
	    let before' = If (Unop(PureC.Not, lbl'), before, []) in
	      set @ decls @ [(before', lb); (if', l')]
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
  let lb = try snd (List.hd before) with Failure "hd" -> l in
  let set = 
    if cond_equal lbl' e then [] else [Exp (Set (lbl', None, e)), lb] 
  in
  let e = 
    if b then (* expression for While *) or_bexp (normalize_bexp lbl') cond 
    else (* expression for DoWhile *)  cond 
  in
  let g_lbl = goto_lbl lbl g_offset in
  let if' = If(lbl', [Goto g_lbl, g_loc], []) in
  let blk' = search_and_add blk in
  let l' = try snd (List.hd blk') with Failure "hd" -> l in
  let blk' = (if', l')::blk' in
  let blk' = inward lbl g_offset g_loc blk' in
  let decls, before = extract_decls before in
    if before = [] then 
      (* optimisation when there are no stmts before the loop *)
	set @ decls, blk', e
    else 
      let before' = If(Unop(PureC.Not, lbl'), before, []) in
	  set @ decls @ [before', lb], blk', e
  

and while_in lbl l e before cond blk1 blk2 blk3 g_offset g_loc=
  let stmts', blk2', e = loop_in lbl l e before cond blk2 g_offset g_loc true in
    stmts'@[For(blk1, e, blk2', blk3), l]

and dowhile_in lbl l e before cond blk g_offset g_loc =
  let stmts', blk', e = loop_in lbl l e before cond blk g_offset g_loc false in
    stmts'@[DoWhile(blk', e), l]

and cswitch_in lbl l e before cond cases default g_offset g_loc =
  let lbl' = Var (fresh_lbl lbl) in
  let tswitch = "switch."^(Newspeak.string_of_loc l)^"."^g_offset in
  let declr = LocalDecl (tswitch, VDecl (uint_typ, false, false, None)) in
  let tswitch = Var tswitch in
  let lb = try snd (List.hd before) with Failure "hd" -> l in
  let set = if cond_equal lbl' e then [] else [Exp (Set (lbl', None, e)), lb] in
  let set_if = Exp (Set(tswitch, None, cond)) in
  let last = try (snd (List.hd (List.rev before))) with Failure "hd" -> l in
  let before' = before @ [set_if, last] in
  let decls, before' = extract_decls before' in
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
      let switch' = CSwitch(tswitch, cases', default) in
	if before' = [] then
	  set @ decls @[switch' ,l]
	else
	  let if' = If(Unop(PureC.Not, lbl'), before', [set_else, l]) in
	    set @ decls @ [(declr, lb) ; (if', lb) ; (switch', l)]
    with Not_found ->
      let conds = List.map (fun (e, _, _) -> e) cases in
      let build e e' = or_bexp e e' in
      let e_default = List.fold_left build zero conds in
      let e_default = Unop(PureC.Not, e_default) in
      let set_else = Exp (Set(tswitch, None, e_default)) in
      let default' = (if', l)::default in
      let default' = inward lbl g_offset g_loc default' in
      let switch' = CSwitch(tswitch, cases, default') in
	if before' = [] then
	  set @ decls @[switch', l]
	else
	  let if' = If(Unop(PureC.Not, lbl'), before', [set_else, l]) in
	    set @ decls @ [(declr, lb) ; (if', lb) ; (switch', l)]

and block_in lbl l e before blk g_offset g_loc =
  let lb = try snd (List.hd before) with Failure "hd" -> l in
  let blk' = (If(e, [Goto (goto_lbl lbl g_offset), g_loc], []), lb)::blk in
  let blk' = inward lbl g_offset g_loc blk' in
  let lbl' =  Var (fresh_lbl lbl) in
  let decls, before = extract_decls before in
    if before = [] then decls @ [Block blk', l]
    else 
      let if' = If(Unop(PureC.Not, lbl'), before, []) in
	decls @ [(if', lb) ; (Block blk', l)] 

and inward lbl g_offset g_loc stmts =
  let rec search_goto_cond before stmts =
    match stmts with
	[] -> raise Not_found
      | (stmt, l)::stmts ->
	  match stmt with
	      If(e, [Goto lbl', _], []) when goto_equal lbl lbl' g_offset -> (List.rev before), e, stmts
	    | Block blk -> begin
		try
		  let before', e, blk' = search_goto_cond before blk in
		    before', e, (Block blk', l)::stmts
		with 
		    Not_found -> let before' = (stmt, l)::before in search_goto_cond before' stmts
	      end
	    | _ -> let before' = (stmt, l)::before in search_goto_cond before' stmts
  in
  let rec inward before stmts = 
    match stmts with
	[] -> []
      | (stmt, l)::stmts ->
	  if has_label [stmt, l] lbl then
	    before@((stmt, l)::stmts)
	  else
	    if search_lbl [stmt, l] lbl then 
	      try
		let before', e, after' = search_goto_cond [] before in
		let stmts' =
		  match stmt with
		      If (ie, if_blk, else_blk) -> 
			if_else_in lbl l e after' ie if_blk else_blk g_offset g_loc
			  
		    | For (blk1, cond, blk2, blk3) -> 
			while_in lbl l e after' cond blk1 blk2 blk3 g_offset g_loc
			  
		    | DoWhile(blk, cond) ->
			dowhile_in lbl l e after' cond blk g_offset g_loc
			  
		    | CSwitch (ce, cases, default) -> 
			cswitch_in lbl l e after' ce cases default g_offset g_loc
			  
		    | Block blk -> 
			block_in lbl l e after' blk g_offset g_loc 
			  
		    | _ -> (stmt, l)::stmts 
		in
		  before'@stmts'@stmts
	      with
		  Not_found ->
		    match stmt with
			Block blk -> 
			  let blk' = inward [] blk in before@((Block blk', l)::stmts)
		      | _ -> invalid_arg ("Goto_elimination.inward: goto has to be in that stmt list")
	    else 
	      let before' = before@[stmt, l] in inward before' stmts
  in inward [] stmts
       

       
let rec lifting_and_inward stmts lbl l_level g_level g_offset g_loc vdecls =
  let rec split_goto stmts =
    match stmts with
	[] -> raise Not_found
      | (stmt, l)::stmts ->
	  match stmt with
	      If(e, [Goto lbl', _], _) when goto_equal lbl lbl' g_offset ->
		[], stmts, e
	    | Block blk -> begin
		try
		  let blk', after, e = split_goto blk in 
		    [Block blk', l], (Block after, l)::after, e
		with
		    Not_found -> 
		      let blk, after, e = split_goto stmts in
		  (stmt, l)::blk, after, e
	      end
	    | _ ->
		let blk, after, e = split_goto stmts in
		  (stmt, l)::blk, after, e
		    
  in
  let lifting stmts =
    let rec lift stmts =
      match stmts with
	  [] -> [], false
	| (stmt, l)::stmts ->
	    match stmt with
		Block blk ->
		  let blk', b = lift blk in 
		 if b then (Block blk', l)::stmts, true
		 else 
		   let stmts', b = lift stmts in (stmt, l)::stmts', b
	      | _ -> 
		  if search_lbl [stmt, l] lbl then
		    let blk, after, e = split_goto stmts in
		    let g_lbl = goto_lbl lbl g_offset in
		    let lbl' = Var (fresh_lbl lbl) in
		    let if' = If(lbl', [Goto g_lbl, g_loc], []) in
		    let l_set = try snd (List.hd (List.rev blk)) with Failure "hd" -> l in
		    let set = if cond_equal lbl' e then [] else [Exp (Set (lbl', None, e)), l_set] in
		    let blk', after' = avoid_break_continue_capture blk l l_set g_offset vdecls in
		    let blk' = [(if', l) ; (stmt, l)] @ blk' @ set in
		      (* inward transformations on the blk chunk. We know that the
			 first stmt is the goto stmt and the second one contains
			 the label stmt *)
		    let blk' = inward lbl g_offset g_loc blk' in	   
		      (* do-while loop *)
		    let blk' = [DoWhile(blk', lbl'), l_set] in
		      blk' @ after @ after', true
		  else 
		    let stmts', b = lift stmts in
		    (stmt, l)::stmts', b
    in
      fst (lift stmts)
  in
  let rec lifting_and_inward stmts = 
    if has_goto stmts lbl g_offset then
      begin
	try 
	  (* lifting the backward goto above the label. If
	     the goto is forward then Not_found is raised and lifting is
	     skiped *)
	  let stmts' = lifting stmts in
	    g_level := !g_level + 1;
	    l_level := !l_level + 1;
	    stmts', true
	with 
	    (* goto is forward. Call to inward only*)
	    Not_found -> inward lbl g_offset g_loc stmts, true
      end
    else
      match stmts with
	  [] -> [], false
	| (stmt, l)::stmts -> 
	    match stmt with
		If(e, if_blk, else_blk) -> 
		  let if_blk', b' = lifting_and_inward if_blk in
		  let else_blk', b' =  if b' then else_blk, b' else lifting_and_inward else_blk in
		  let stmts', b' = if b' then stmts, b' else lifting_and_inward stmts in
		  let if' = If(e, if_blk', else_blk') in
		    (if', l)::stmts', b'
		      
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
		  let stmts', b' = if b' then stmts, b' else lifting_and_inward stmts in
		    (CSwitch (e, cases', blk'), l)::stmts', b'
		      
	      | For(blk1, e, blk2, blk3) ->
		  let blk2', b' = lifting_and_inward blk2 in
		  let stmts', b' = if b' then stmts, b' else lifting_and_inward stmts in
		    (For(blk1, e, blk2', blk3), l)::stmts', b' 
		      
	      | DoWhile(blk, e) -> 
		  let blk', b' = lifting_and_inward blk in
		  let stmts', b' = if b' then stmts, b' else lifting_and_inward stmts in
		    (DoWhile(blk', e), l)::stmts', b'

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

  
let elimination stmts lbl (gotos, lo) vdecls =
  (* moves all gotos of the given label lbl. lo is the pair
     (level, offset) of the label statement *)
  let l_level, _ = lo in
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
	  stmts := sibling_elimination !stmts lbl id vdecls
	end
	else begin
	  let l_level = ref l_level in
	    stmts := lifting_and_inward !stmts lbl l_level l id o vdecls;
	    stmts := sibling_elimination !stmts lbl id vdecls
	end
      end
      else
	(* goto and label are sibling; eliminate goto and label *) 
	stmts := sibling_elimination !stmts lbl id vdecls
  in
    List.iter move gotos;
    !stmts

let nth_lv = ref 0
  
let renaming_block_variables stmts =
  let rec search v stack =
    match stack with
	[] -> raise Not_found
      | s::stack ->
	  let n = String.length v in
	    try
	      let s' = String.sub s 0 n in
		if String.compare v s' = 0 && (s.[n] = '.' || n = String.length s) then s 
		else search v stack
	    with
		Invalid_argument _ -> search v stack
  in
  let rename stmt =
    match stmt with
	LocalDecl (n, d) ->
	  let n' = n  ^ "." ^ (string_of_int !nth_lv) in
	    nth_lv := !nth_lv + 1;
	    (LocalDecl (n', d), n')
	      
      | _ -> 
	  invalid_arg "Goto_elimination.extract_decls: stmt is not a declaration"
  in
  let rec replace stack e =
    let rec replace e = 
      try
	match e with
	    Var v ->  let v' = search v stack in Var v'
	  | Field (e, s) ->
	      let s' = search s stack in 
	      let e' = replace e in
		Field(e', s')
	  | Index(e1, e2) ->
	      let e1' = replace e1 in
	      let e2' = replace e2 in
		Index(e1', e2')
	  | Deref e ->
	      let e' = replace e in Deref e'
	  | AddrOf e -> 
	      let e' = replace e in AddrOf e'
	  | Unop (op, e) ->
	      let e' = replace e in Unop (op, e')
	  | Binop (op, e1, e2) ->
	      let e1' = replace e1 in
	      let e2' = replace e2 in
		Binop(op, e1', e2')
	  | Call(e, l) ->
	      let e' = replace e in
	      let l' = List.map replace l in
		Call(e', l')
	  | SizeofE e ->
	      let e' = replace e in SizeofE e'
	  | Cast (e, t) ->
	      let e' = replace e in Cast (e', t)
	  | Set (e1, op, e2) ->
	      let e1' = replace e1 in
	      let e2' = replace e2 in
		Set(e1', op, e2')
	  | OpExp(op, e, b) ->
	      let e' = replace e in
		OpExp(op, e', b)
	  | BlkExp blk -> BlkExp (explore stack blk)
	  | _ -> e
      with Not_found -> e
    in 
      replace e

  and explore stack stmts =
    match stmts with 
	[] -> []
      | (stmt, l)::stmts ->
	  match stmt with
	      LocalDecl(s, _) -> begin
		try 
		  let _ = search s stack in
		  let stmt', s' = rename stmt in
		  let stack' = s'::stack in
		    (stmt', l)::(explore stack' stmts)
		with Not_found ->
		  let stack' = s::stack in
		    (stmt, l)::(explore stack' stmts)
	      end
	    | Block blk ->
		let blk' = explore stack blk in
		  (Block blk', l)::(explore stack stmts)
	    | Exp e -> 
		let e' = replace stack e in
		  (Exp e', l)::(explore stack stmts)
	    | If(e, if_blk, else_blk) ->
		let if_blk' = explore stack if_blk in
		let else_blk' = explore stack else_blk in
		(If(e, if_blk', else_blk'), l)::(explore stack stmts)
	    | CSwitch (e, cases, default) ->
		let apply (e, blk, l) = 
		  let blk' = explore stack blk in (e, blk', l)
		in
		let cases' = List.map apply cases in
		let default' = explore stack default in
		  (CSwitch(e, cases', default'), l)::(explore stack stmts)
	    | For(blk1, e, blk2, blk3) ->
		let blk1' = explore stack blk1 in
		let blk2' = explore stack blk2 in
		let blk3' = explore stack blk3 in
		  (For(blk1', e, blk2', blk3'), l)::(explore stack stmts)
	    | DoWhile(blk, e) ->
		let blk' = explore stack blk in
		  (DoWhile(blk', e), l)::(explore stack stmts)
	    | Return -> (Return, l)::(explore stack stmts)
	    | _ ->
		(stmt, l)::(explore stack stmts)
  in explore [] stmts


  

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
  let elimination lbls stmts vars =
    (* goto elimination *)
    let stmts = ref stmts in
    let move lbl g = 
      stmts := elimination !stmts lbl g vars;
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
	(* no goto found *) 
	stmts'
      else 
	(* processing goto elimination *)
	let vars = ref [] in
	let stmts' = vdecls'@stmts' in
	  (* replacing vars with the same name *)
	let stmts' = renaming_block_variables stmts' in
	let stmts' = elimination lbls stmts' vars in
	let _, l = List.hd vdecls' in
	let vars' = List.map (fun vdecl -> (vdecl, l)) !vars in
	  vars'@stmts'
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
