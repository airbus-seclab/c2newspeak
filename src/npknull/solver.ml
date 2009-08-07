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

let emptyset = ()

let prepare_call _ _ = ()

let apply _ _ = ()

type t = unit

let process prog = 
  let current_loc = ref Newspeak.unknown_loc in
(* 
   list of functions to analyze
   when this list is empty, analysis ends
*)
(* TODO: could try a queue instead?? *)
  let todo = ref [] in
  let funtbl = Hashtbl.create 100 in

  let print_err msg = 
    print_endline (Newspeak.string_of_loc !current_loc^": "^msg)
  in

  let warn_deref () = print_err "potential null pointer deref" in

(* TODO: maybe find a way to remove emptyset???!!! 
   use an exception instead
*)
  let init_fun f _ = Hashtbl.add funtbl f (emptyset, emptyset) in

  let rec check_lval x = 
    match x with
	Global _ -> ()
      | Local _ -> ()
      | Shift (lv, e) -> 
	  check_lval lv;
	  check_exp e
      | _ -> warn_deref ()

  and check_exp x = 
    match x with
	Const _ -> ()
      | AddrOf (lv, _) -> check_lval lv
      | _ -> warn_deref ()
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
      | Decl (_, _, body) -> process_blk body s
      | Call FunId f -> begin
	  try
	    let (init, rel) = Hashtbl.find funtbl f in
	    let init = prepare_call init s in
	    let s = apply rel s in
	      Hashtbl.replace funtbl f (init, rel);
	      todo := f::!todo;
	      s
	  with Not_found -> 
	    print_err ("missing function: "^f
		       ^", call ignored, analysis may be unsound")
	end
      | _ -> invalid_arg "Analysis.process_stmtkind: case not implemented"
  in

    (* initialization *)
    Hashtbl.iter init_fun prog.fundecs;
    let s = universe () in
    let s = process_blk prog.init s in
    let _ = process_stmtkind (Call (FunId "main")) s in

    (* fixpoint computation *)
    try
      while true do
	match !todo with
	    f::tl -> 
	      todo := tl;
	      let (_, body) = Hashtbl.find prog.fundecs f in
	      let (s, _) = Hashtbl.find funtbl f in
		process_blk body s
	 | [] -> raise Exit
      done;
      ()
    with Exit -> ()
