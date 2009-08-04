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
(* TODO: factor launcher and error treatment *)
open Newspeak

(* Analysis section *)

module Set = Set.Make(String)

let print_fun f (used, _) =
  let used = 
    match used with
	None -> "?"
      | Some used -> 
	  let used = Set.elements used in
	    ListUtils.to_string (fun x -> x) ", " used
  in
    print_endline (f^": "^used)

let print_results funtbl = 
  Hashtbl.iter print_fun funtbl


(* for each function, collect:
   - the set of globals it needs directly (present in any expression)
   - the set of its callers
   
   algorithm:
   - put all functions in the todo set
   - get one function f
   - compute the set of globals needed in f, at function calls use the info 
   computed so far
   - if globals have changed, then add all f callers to the todo set
   - until there are no more functions in the todo set
*)
let process fids prog =
  let funtbl = Hashtbl.create 100 in
  let todo = Queue.create () in
  let unknown_funs = ref Set.empty in

  let init_fun f _ = 
    Hashtbl.add funtbl f (Some Set.empty, Set.empty);
    Queue.add f todo
  in

  let update_fun f x =
    let (used, preds) = Hashtbl.find funtbl f in
      match used with
	  None -> ()
	| Some used -> 
	    match x with
		Some x when Set.subset x used -> ()
	      | _ -> 
		  let used = 
		    match x with
			None -> None
		      | Some x -> Some (Set.union x used)
		  in
		    Hashtbl.replace funtbl f (used, preds);
		    Set.iter (fun x -> Queue.add x todo) preds
  in

  let get_fun f =
    try
      let (used, _) = Hashtbl.find funtbl f in
	match used with
	    Some used -> used
	  | None -> raise Exit
    with Not_found -> 
      if not (Set.mem f !unknown_funs) then begin
	unknown_funs := Set.add f !unknown_funs;
	print_endline ("unknown function "^f^", assuming no global is modified")
      end;
      Set.empty
  in

  let rec process_lval x =
    match x with
	Local _ -> Set.empty
      | Global x -> Set.singleton x
      | Deref (e, _) -> process_exp e
      | Shift (lv, e) -> Set.union (process_lval lv) (process_exp e)

  and process_exp x =
    match x with
	Const _ -> Set.empty
      | UnOp (_, e) -> process_exp e
      | BinOp (_, e1, e2) -> Set.union (process_exp e1) (process_exp e2)
      | Lval (lv, _) -> process_lval lv
	  (* TODO: maybe this case could be more precise *)
      | AddrOf (lv, _) -> process_lval lv
      | AddrOfFun _ -> Set.empty
  in

  let rec process_blk x =
    match x with
	(x, _)::tl -> Set.union (process_stmtkind x) (process_blk tl)
      | [] -> Set.empty

  and process_stmtkind x =
    match x with
	Set (lv, e, _) -> Set.union (process_lval lv) (process_exp e)
      | Copy (lv1, lv2, _) -> Set.union (process_lval lv1) (process_lval lv2)
      | Guard e -> process_exp e
      | Decl (_, _, body) -> process_blk body
      | Select (br1, br2) -> Set.union (process_blk br1) (process_blk br2)
      | InfLoop body -> process_blk body
      | DoWith (body, _, action) -> 
	  Set.union (process_blk body) (process_blk action)
      | Goto _ -> Set.empty
      | Call FunId f -> get_fun f
      | Call FunDeref _ -> 
	  let res = ref Set.empty in
	  let collect f = res := Set.union (get_fun f) !res in
	    List.iter collect fids;
	    !res
	      
      | _ -> raise Exit
  in

    Hashtbl.iter init_fun prog.fundecs;
    try
      while true do
	let f = Queue.take todo in
	let (_, body) = Hashtbl.find prog.fundecs f in
	let used = try Some (process_blk body) with Exit -> None in
	  update_fun f used
      done
    with Queue.Empty -> 
      print_results funtbl
    

(* Execution section *)

let exec_name = "npkfuns"

let input = ref ""

let speclist = []

let anon_fun file = 
  if !input <> "" then invalid_arg "you can only analyse one file at a time";
  input := file

let usage_msg = exec_name^" [options] [-help|--help] file.npk"

let _ =
  try
    Arg.parse speclist anon_fun usage_msg;
    if !input = "" 
    then invalid_arg ("no file specified. Try "^exec_name^" --help");

    let prog = Newspeak.read !input in
    let fids = collect_fid_addrof prog in
      process fids prog

  with Invalid_argument s -> 
    print_endline ("Fatal error: "^s);
    exit 0
