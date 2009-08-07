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

let universe () = ()

type t = unit

let process prog = 
  let current_loc = ref Newspeak.unknown_loc in

  let print_err () = 
    let msg = 
      Newspeak.string_of_loc !current_loc^": potential null pointer deref"
    in
      print_endline msg
  in

  let rec check_lval x = 
    match x with
	Global _ -> ()
      | Local _ -> ()
      | Shift (lv, e) -> 
	  check_lval lv;
	  check_exp e
      | _ -> print_err ()

  and check_exp x = 
    match x with
	Const _ -> ()
      | AddrOf (lv, _) -> check_lval lv
      | _ -> print_err ()
  in

  let rec process_blk x s =
    match x with
	[] -> s
      | (x, loc)::tl -> 
	  current_loc := loc;
	  let s = process_stmtkind x s in
	    process_blk tl s
	      
  and process_stmtkind x s =
    match x with
	Set (lv, e, _) -> 
	  check_lval lv;
	  check_exp e;
	  s
      | _ -> invalid_arg "Analysis.process_stmtkind: case not implemented"
  in



  let s = universe () in
  let s = process_blk prog.init s in
  let (_, body) = 
    try Hashtbl.find prog.fundecs "main"
    with Not_found -> invalid_arg "Analysis.process: missing main function"
  in
  let _ = process_blk body s in
    ()

