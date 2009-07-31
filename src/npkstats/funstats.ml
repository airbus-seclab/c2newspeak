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

open Newspeak

module Set = Set.Make(String)

type funkind = 
    Empty
  | Pure
  | Globals of (Set.t * Set.t)
  | Other

let print_results funtbl =
  let empty_nb = ref 0 in
  let pure_nb = ref 0 in
  let global_nb = ref 0 in
  let other_nb = ref 0 in
  let other = ref [] in
  let count f kind =
    let counter = 
      match kind with
	  Empty -> empty_nb
	| Pure -> pure_nb
	| Globals _ -> global_nb
	| Other -> 
	    other := f::!other;
	    other_nb
    in
      incr counter
  in
      Hashtbl.iter count funtbl;
      if !empty_nb <> 0 
      then print_endline ("Empty functions: "^string_of_int !empty_nb);
      if !pure_nb <> 0 
      then print_endline ("Pure functions: "^string_of_int !pure_nb);
      if !global_nb <> 0 then begin
	print_endline ("Functions that read/update globals: "
		       ^string_of_int !global_nb)
      end;
      if !other_nb <> 0 then begin
  	print_endline "Remaining functions: ";
	List.iter print_endline !other
      end
  

let collect prog = 
  let funtbl = Hashtbl.create 100 in

  let written_globals = ref Set.empty in
  let read_globals = ref Set.empty in

  let rec write_lval lv =
    match lv with
	(* Assumes local do not *)
	Local _ -> ()
      | Global x -> written_globals := Set.add x !written_globals
      | _ -> raise Exit

  and write_exp e =
    match e with
	Const _ -> ()
      | Lval (lv, _) -> write_lval lv
      | UnOp (Not, e) -> write_exp e
      | BinOp ((Eq _|Gt _), e1, e2) -> 
	  write_exp e1;
	  write_exp e2
      | _ -> raise Exit
  in
    
  let rec read_lval lv =
    match lv with
	Local _ -> ()
      | _ -> raise Exit

  and read_exp e =
    match e with
	Const _ -> ()
      | Lval (lv, _) -> read_lval lv
      | UnOp ((PtrToInt _|IntToPtr _|Cast _), e) -> read_exp e
      | _ -> raise Exit
  in

  let rec process_stmtkind x =
    match x with
	Set (lv, e, _) -> 
	  write_lval lv; 
	  read_exp e
      | Decl (_, _, body) -> process_blk body
      | Select (br1, br2) -> 
	  process_blk br1;
	  process_blk br2
      | Guard e -> write_exp e
      | DoWith (body, _, action) -> 
	  process_blk body;
	  process_blk action
      | Goto _ -> ()
      | Call FunId f -> begin
	  match process_fun f with
	      Empty | Pure -> ()
	    | Globals (read, written) -> 
		written_globals := Set.union written !written_globals;
		read_globals := Set.union read !read_globals
	    | Other -> raise Exit
	end
      | _ -> raise Exit
    
  and process_blk x = 
    match x with
	(hd, _)::tl -> 
	  process_stmtkind hd;
	  process_blk tl
      | [] -> ()

  and process_fun f =
    try Hashtbl.find funtbl f 
    with Not_found -> 
      try
	let (_, blk) = Hashtbl.find prog.fundecs f in
	let kind =
	  match blk with
	      [] -> Empty
	    | _ -> 
		let written = !written_globals in
		let read = !read_globals in 
		  written_globals := Set.empty;
		  read_globals := Set.empty;
		  let kind = 
		    try 
		      process_blk blk;
		      if (Set.is_empty !written_globals)
			&& (Set.is_empty !read_globals) then Pure
		      else Globals (!read_globals, !written_globals)
		    with Exit -> Other
		  in
		    written_globals := written;
		    read_globals := read;
		    kind
	in
	  Hashtbl.add funtbl f kind;
	  kind
      with Not_found -> 
	Hashtbl.add funtbl f Other;
	Other
  in

  let process_fundec f _ = 
    let _ = process_fun f in
      ()
  in
  
    Hashtbl.iter process_fundec prog.fundecs;
    print_results funtbl
