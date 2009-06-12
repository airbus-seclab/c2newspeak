(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2009  Charles Hymans
 
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

open Equations

(* TODO: could be implemented as a functor Lift *)
module Store =
struct
  type t = Store.t option

  let create () = Some (Store.create ())
    
  let emptyset () = None

  let apply f s =
    match s with
	Some s -> Some (f s)
      | None -> None

  let add_global x = apply (Store.add_global x)

  let add_local = apply Store.add_local

  let remove_local = apply Store.remove_local

  let is_subset s1 s2 =
    match (s1, s2) with
	(Some s1, Some s2) -> Store.is_subset s1 s2
      | (None, _) -> true
      | (_, None) -> false

  let join s1 s2 =
    match (s1, s2) with
	(Some s1, Some s2) -> Some (Store.join s1 s2)
      | (None, s) | (s, None) -> s

  let assign args = apply (Store.assign args)

  let points_to s e1 e2 =
    match s with
	Some s -> Store.points_to s e1 e2
      | None -> false

  let fids_of_exp s e =
    match s with
	Some s -> Store.fids_of_exp s e
      | None -> []

  let to_string s =
    match s with
	Some s -> Store.to_string s
      | None -> "{}"
end
  
let fixpoint f s =
  let rec fixpoint s =
    let s' = f s in
      if Store.is_subset s' s then s
      else fixpoint (Store.join s s')
  in
    fixpoint s

let jump n j s =
  let rec jump n j =
    match j with
	x::_ when n = 0 -> x := Store.join !x s
      | _::tl -> jump (n-1) tl
      | [] -> invalid_arg "Solver.jump: unreachable code"
  in
    jump n j

let run (globals, fundecs, init) = 
  let unknown_funs = ref [] in
  let alarms = ref [] in

  let rec process_blk x c j s =
    match x with
	hd::tl -> 
	  let s = process_stmt hd c j s in
	    process_blk tl c j s
      | [] -> s

  and process_stmt (x, loc) c j s =
    match x with
	Set set -> Store.assign set s
      | Decl body -> 
	  let s = Store.add_local s in
	  let s = process_blk body c j s in
	    Store.remove_local s
      | Select (br1, br2) -> 
	  let s1 = process_blk br1 c j s in
	  let s2 = process_blk br2 c j s in
	    Store.join s1 s2
      | InfLoop body -> 
	  let _ = fixpoint (process_blk body c j) s in
	    Store.emptyset ()
      | BlkLbl body -> 
	  let s_lbl = ref (Store.emptyset ()) in
	  let s = process_blk body c (s_lbl::j) s in
	    Store.join s !s_lbl
      | Goto n -> 
	  jump n j s;
	  Store.emptyset ()
      | Call (Global "malloc") -> 
	  let p = Deref (Local 0) in
	  let t = Global Var.main_tainted in
	    if (Store.points_to s p t) then begin
	      let loc = Newspeak.string_of_loc loc in
	      let msg = loc^": potential malloc with external argument" in
		if not (List.mem msg !alarms) then begin
		  alarms := msg::(!alarms);
		  print_endline msg
		end
	    end;
	    s
      | Call f -> 
	  let f = Store.fids_of_exp s f in
	  let res = ref (Store.emptyset ()) in
	  let add_call f = res := Store.join !res (process_call f c s) in
	    List.iter add_call f;
	    !res
      | Display -> 
	  print_endline (Store.to_string s);
	  s

  and process_call f c s =
    try
      let body = Hashtbl.find fundecs f in
	if not (List.mem f c) then process_blk body (f::c) [] s
	else begin
	  prerr_endline ("Warning: recursive function '"^f
			 ^"', no flow assumed, maybe unsound");
	  s
	end
    with Not_found -> 
      if not (List.mem f !unknown_funs) then begin
	unknown_funs := f::!unknown_funs;
	prerr_endline ("Warning: unknown function '"^f
		       ^"', no flow assumed, maybe unsound")
      end;
      s
  in

  let s = ref (Store.create ()) in
  let declare_global x = s := Store.add_global x !s in
    List.iter declare_global globals;
    let _ = process_blk init [] [] !s in
      ()
