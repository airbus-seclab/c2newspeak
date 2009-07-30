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
  let other = ref Set.empty in

  let rec process_lval lv =
    match lv with
	(* Assumes local do not *)
	Local _ -> true
      | _ -> false

  and process_exp e =
    match e with
	Const _ -> true
      | Lval (lv, _) -> process_lval lv
      | _ -> false
  in

  let process_stmtkind x =
    match x with
	Set (lv, e, _) -> (process_lval lv) && (process_exp e)
      | _ -> false
  in
    
  let rec process_blk x = 
    match x with
	(hd, _)::tl -> (process_stmtkind hd) && (process_blk tl)
      | [] -> true
  in

  let process_fundec f (_, blk) =
    let tbl =
      match blk with
	  [] -> empty
	| _ -> if (process_blk blk) then pure else other
    in
      tbl := Set.add f !tbl
  in
  
    Hashtbl.iter process_fundec prog.fundecs;
    let empty_nb = Set.cardinal !empty in
    let pure_nb = Set.cardinal !pure in
    let other_nb = Set.cardinal !other in
      if empty_nb <> 0 
      then print_endline ("Empty functions: "^string_of_int empty_nb);
      if pure_nb <> 0 
      then print_endline ("Pure functions: "^string_of_int pure_nb);
      if other_nb <> 0 then begin
  	print_endline "Remaining functions: ";
	Set.iter print_endline !other
      end
  
