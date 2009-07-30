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

let collect prog = 
  let empty = ref Set.empty in
  let pure = ref Set.empty in
  let global = ref Set.empty in
  let other = ref Set.empty in

  let written_globals = ref Set.empty in
  let read_globals = ref Set.empty in

  let rec write_lval lv =
    match lv with
	(* Assumes local do not *)
	Local _ -> ()
      | Global x -> written_globals := Set.add x !written_globals
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
      | _ -> raise Exit
  in

  let process_stmtkind x =
    match x with
	Set (lv, e, _) -> 
	  write_lval lv; 
	  read_exp e
      | _ -> raise Exit
  in
    
  let rec process_blk x = 
    match x with
	(hd, _)::tl -> 
	  process_stmtkind hd;
	  process_blk tl
      | [] -> ()
  in

  let process_fundec f (_, blk) =
    let tbl =
      match blk with
	  [] -> empty
	| _ -> 
	    let tbl =
	      try 
		process_blk blk;
		if (Set.is_empty !written_globals) 
		  && (Set.is_empty !read_globals) then pure
		else global
	      with Exit -> other
	    in
	      written_globals := Set.empty;
	      read_globals := Set.empty;
	      tbl
    in
      tbl := Set.add f !tbl
  in
  
    Hashtbl.iter process_fundec prog.fundecs;
    let empty_nb = Set.cardinal !empty in
    let pure_nb = Set.cardinal !pure in
    let global_nb = Set.cardinal !global in
    let other_nb = Set.cardinal !other in
      if empty_nb <> 0 
      then print_endline ("Empty functions: "^string_of_int empty_nb);
      if pure_nb <> 0 
      then print_endline ("Pure functions: "^string_of_int pure_nb);
      if global_nb <> 0 then begin
	print_endline ("Functions that read/update globals: "
		       ^string_of_int global_nb)
      end;
      if other_nb <> 0 then begin
  	print_endline "Remaining functions: ";
	Set.iter print_endline !other
      end
  
