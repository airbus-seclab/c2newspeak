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

(* This is the code that should be implemented by students *)
open Simple

let add_globals tbl s =
  let res = ref s in
  let add_global x _ = res := State.add_var x !res in
    Hashtbl.iter add_global tbl;
    !res

let fixpoint f s =
  let rec fixpoint s1 =
    let s2 = f s1 in
      if State.contains s1 s2 then s1
      else fixpoint (State.join s s2)
  in
    fixpoint s

let compute prog = 
  let rec compute_blk x s =
    match x with
	x::tl -> 
	  let s = compute_stmt x s in
	    compute_blk tl s
      | [] -> s
	  
  and compute_stmt (x, loc) s =
    print_endline (Simple.string_of_loc loc^": "^State.to_string s);
    compute_stmtkind loc x s
      
  and compute_stmtkind loc x s =
    match x with
	Set (lv, e) -> 
	  (* TODO: do a check_exp *)
	  State.assign lv e s
      | Call FunId f -> 
	  let body = 
	    try Hashtbl.find prog.fundecs f
	    with Not_found -> 
	      invalid_arg ("Fixpoint.compute_stmtkind: unknown function "^f)
	  in
	    print_endline ("Call function: "^f);
	    let s = compute_blk body s in
	      print_endline ("Return from function: "^f);
	      s
      | If (e, br1, br2) -> 
	  (* TODO: do a check_exp *)
(* TODO: have a guard of e and not e!!! *)
	  let (s1, s2) = State.guard e s in
	  let s1 = compute_blk br1 s1 in
	  let s2 = compute_blk br2 s2 in
	    State.join s1 s2
      | While (e, body) ->
	  (* TODO: do a check_exp *)
	  let f s =
	    let (s, _) = State.guard e s in
	      compute_blk body s
	  in
	  let s = fixpoint f s in
	  let (_, s) = State.guard e s in
	    s
      | Assert a -> 
	  if not (State.implies s a) 
	  then print_endline (Simple.string_of_loc loc^": assertion violation");
	  s
  in
    
    
    print_endline "Analysis starts";
    let s = State.universe in
    let s = add_globals prog.globals s in
    let s = compute_blk prog.init s in
    let body =
      try Hashtbl.find prog.fundecs "main"
      with Not_found -> invalid_arg "Fixpoint.compute: no main function"
    in
    let s = compute_blk body s in
      print_endline ("Final state: "^State.to_string s)
	
