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

(* TODO: find a way to factor both algorithms
   should not be that hard!!! *)
open Newspeak

module FunSet = Set.Make(struct type t = string let compare = compare end)

let process prog =
  let res = Hashtbl.create 100 in
    
  let has_body f = Hashtbl.mem prog.fundecs f in
    
  let get_body f =
    try
      let (_, body) = Hashtbl.find prog.fundecs f in
	body
    with Not_found ->
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
      | Select (blk1, blk2) -> Select (process_blk blk1, process_blk blk2)
      | InfLoop body -> InfLoop (process_blk body)
      | DoWith (body, lbl, action) ->
	  DoWith (process_blk body, lbl, process_blk action)
      | Set _ | Copy _ | Goto _ | Call _ | Guard _ -> x
  in

  let process_fun fid (t, body) =
    let body = process_blk body in
      Hashtbl.add res fid (t, body)
  in

    Hashtbl.iter process_fun prog.fundecs;
    { prog with fundecs = res }

class count_visitor fundecs unprocessed counters =
object (self)
  inherit Newspeak.visitor

  method incr_fun f =
    try 
      let c = Hashtbl.find counters f in
	Hashtbl.replace counters f (c+1)
    with Not_found -> Hashtbl.add counters f 1

  method process_stmt (x, _) =
    match x with
	Call (FunId f) when Hashtbl.mem fundecs f -> 
	  self#incr_fun f;
	  true
      | _ -> true

  method process_fun f _ =
    unprocessed := FunSet.add f !unprocessed;
    true
end

let process_one prog =
  let res = Hashtbl.create 100 in
  let counters = Hashtbl.create 100 in
  let unprocessed = ref FunSet.empty in

  let should_inline f = 
    (Hashtbl.mem prog.fundecs f) && (Hashtbl.find counters f = 1)
  in
    
  let rec get_body f =
    (* Carefull works only in the absence of recursive functions! *)
    if (FunSet.mem f !unprocessed) then process_fun f;
    let (_, body) = Hashtbl.find res f in
      body
  
  and process_blk x = 
    match x with
	(Call (FunId f), _)::tl when should_inline f -> 
	  (get_body f)@(process_blk tl)
      | (x, loc)::tl -> (process_stmtkind x, loc)::(process_blk tl)
      | [] -> []
 
  and process_stmtkind x =
    match x with
	Decl (v, t, body) -> Decl (v, t, process_blk body)
      | Select (blk1, blk2) -> Select (process_blk blk1, process_blk blk2)
      | InfLoop body -> InfLoop (process_blk body)
      | DoWith (body, lbl, action) ->
	  DoWith (process_blk body, lbl, process_blk action)
      | Set _ | Copy _ | Goto _ | Call _ | Guard _ -> x

  and process_fun fid =
    let (t, body) = Hashtbl.find prog.fundecs fid in
    let body = process_blk body in
      Hashtbl.add res fid (t, body);
      unprocessed := FunSet.remove fid !unprocessed
  in

  let count_call = new count_visitor prog.fundecs unprocessed counters in
  let count_call = (count_call :> Newspeak.visitor) in
    Hashtbl.iter (Newspeak.visit_fun count_call) prog.fundecs;
    
    while not (FunSet.is_empty !unprocessed) do
      let f = FunSet.choose !unprocessed in
	process_fun f
    done;
    { prog with fundecs = res }
