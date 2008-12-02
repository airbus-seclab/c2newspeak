(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans, Olivier Levillain
  
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

  Olivier Levillain
  email: olivier.levillain@penjili.org
*)

open Newspeak

type stats = {
  nb_vars: int;
  sz_vars: int;
  loop_depth: int;
}

type t = bool * stats

let add_stats st1 st2 =
  { 
    nb_vars = st1.nb_vars + st2.nb_vars;
    sz_vars = st1.sz_vars + st2.sz_vars;
    loop_depth = st1.loop_depth + st2.loop_depth;
  }

let max_stats st1 st2 =
  { 
    nb_vars = max st1.nb_vars st2.nb_vars;
    sz_vars = max st1.sz_vars st2.sz_vars;
    loop_depth = max st1.loop_depth st2.loop_depth;
  }

let init_stats = { nb_vars = 0; sz_vars = 0; loop_depth = 0; }

let count_decl ptr_sz t st =
  let nb = st.nb_vars + 1 in
  let sz = st.sz_vars + ((Newspeak.size_of ptr_sz t) / 8) in 
    { st with nb_vars = nb; sz_vars = sz; }

let count_loop st = { st with loop_depth = st.loop_depth + 1 }

let count debug prog =
  let fid_addrof = Newspeak.collect_fid_addrof prog in
  let unknown_funs = ref [] in
  let exact = ref true in
  let fun_tbl = Hashtbl.create 100 in
  let current_loc = ref Newspeak.unknown_loc in
    
  let rec count_call f =
    try Hashtbl.find fun_tbl f
    with Not_found -> 
      if debug then print_endline ("counting stack height of "^f);
      let height = 
	try 
	  let (_, body) = Hashtbl.find prog.fundecs f in
	    count_blk body init_stats
	with Not_found -> 
	  if not (List.mem f !unknown_funs) then begin
	    unknown_funs := f::!unknown_funs;
	    prerr_endline ("Warning: function "^f^" body not defined")
	  end;
	  init_stats
      in
	Hashtbl.add fun_tbl f height;
	height
	      
  and count_blk x info =
    match x with
	(stmt, loc)::blk -> 
	  current_loc := loc;
	  let info1 = count_stmt stmt info in
	  let info2 = count_blk blk info in
	    max_stats info1 info2
      | [] -> info
	  
  and count_stmt x info =
    match x with
      | Decl (_, t, body) -> 
	  let info = count_decl prog.ptr_sz t info in
	    count_blk body info
      | DoWith (body, _, action) -> 
	  let body_info = count_blk body info in
	  let action_info = count_blk action info in
	    max_stats body_info action_info
      | Call (FunId f) -> 
	  let info_f = count_call f in
	    add_stats info info_f
      | Call _ -> 
	  let build_call f = (Call (FunId f), !current_loc)::[] in
	  let alternatives = List.map build_call fid_addrof in
	    if alternatives <> [] then exact := false;
	    List.fold_left (count_alternatives info) init_stats alternatives
      | ChooseAssert choices -> 
	  let (_, choices) = List.split choices in
	    List.fold_left (count_alternatives info) init_stats choices
      | InfLoop body -> 
	  let info = count_loop info in
	    count_blk body info
      | _ -> info

  and count_alternatives info max_so_far body =
    let info' = count_blk body info in
      max_stats info' max_so_far
  in
    (* TODO: arguments of main not counted ! *)
  let stats = count_call "main" in
    (!exact, stats)
      
let print (exact, st) =
  let symb = if exact then "" else "<= " in
    print_endline ("Maximum number of variables on the stack: "
		    ^symb^(string_of_int st.nb_vars));
    print_endline ("Maximum height of the stack (bytes): "
		    ^symb^(string_of_int st.sz_vars));
    print_endline ("Maximum depth of imbricated loops: "
		    ^symb^(string_of_int st.loop_depth))

