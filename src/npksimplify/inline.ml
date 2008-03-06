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

(* Function inlining of named functions for newspeak *)
(* Inlines at depth 1 *)

open Newspeak

let process (gdecls, fundecs) =
  let res = Hashtbl.create 100 in
    
  let has_body f = 
    let (_, body) = Hashtbl.find fundecs f in
      body <> None
  in
    
  let get_body f =
    let (_, body) = Hashtbl.find fundecs f in
      match body with
	  Some body -> body
	| None -> 
	    invalid_arg ("Inline.process.get_body: function "^f
			  ^"'s body not defined")
  in

  let rec process_blk x = 
    match x with
	(Call (FunId f), _)::tl when has_body f -> 
	  (get_body f)@(process_blk tl)
      | (x, loc)::tl -> (process_stmtkind x, loc)::(process_blk tl)
      | [] -> []
 
  and process_stmtkind x =
    match x with
	Decl (v, t, body) -> Decl (v, t, process_blk body)
      | ChooseAssert choices -> 
	  let choices = List.map process_choice choices in
	    ChooseAssert choices
      | InfLoop body -> InfLoop (process_blk body)
      | DoWith (body, lbl, action) ->
	  DoWith (process_blk body, lbl, process_blk action)
      | Set _ | Copy _ | Goto _ | Call _ -> x

  and process_choice (cond, body) =  (cond, process_blk body) in

  let process_fun fid (t, body) =
    let body = 
    match body with
	None -> None 
      | Some body -> Some (process_blk body)
    in
      Hashtbl.add res fid (t, body)
  in

    Hashtbl.iter process_fun fundecs;
    (gdecls, res)
