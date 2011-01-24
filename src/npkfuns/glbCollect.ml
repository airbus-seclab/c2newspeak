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

open Lowspeak

module Set = StrSet
module Map = Map.Make(struct type t = int let compare = compare end)

type t = (Newspeak.fid, string list) Hashtbl.t

let print_stats glb_tbl = 
  let stats = ref Map.empty in
  let collect_stat _ used =
    let n = List.length used in
    let v = 
      try Map.find n !stats
      with Not_found -> 0
    in
      stats := Map.add n (v+1) !stats
  in
  let print_stat n v =
    print_endline ("number of functions using "^string_of_int n^" globals: "
		   ^string_of_int v)
  in
    Hashtbl.iter collect_stat glb_tbl;
    Map.iter print_stat !stats

let print glb_tbl = 
  let print_fun f used =
    let used = ListUtils.to_string (fun x -> x) ", " used in
      print_endline (f^": "^used)
  in
    Hashtbl.iter print_fun glb_tbl


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
let process silent prog =
  let fids = collect_fid_addrof prog in
  let glb_tbl = Hashtbl.create 100 in
  let pred_tbl = Hashtbl.create 100 in
  let todo = Queue.create () in
  let unknown_funs = ref Set.empty in

  let current_fun = ref "" in

  let print_verbose str = if not silent then print_endline str in

  let init_fun f _ = 
    Hashtbl.add glb_tbl f (Set.empty);
    Hashtbl.add pred_tbl f Set.empty;
    Queue.add f todo
  in

  let update_fun f x =
(*    print_verbose ("Updating: "^f); *)
    let used = Hashtbl.find glb_tbl f in
      if not (Set.subset x used) then begin
	let preds = Hashtbl.find pred_tbl f in
	let used = Set.union x used in
	  Hashtbl.replace glb_tbl f used;
	  Set.iter (fun x -> Queue.add x todo) preds
      end
  in

  let get_fun f =
    try 
      (* update predecessor table *)
      let preds = Hashtbl.find pred_tbl f in
      let preds = Set.add !current_fun preds in
	Hashtbl.replace pred_tbl f preds;

	Hashtbl.find glb_tbl f
    with Not_found -> 
      if not (Set.mem f !unknown_funs) then begin
	unknown_funs := Set.add f !unknown_funs;
	print_verbose ("unknown function "^f
		       ^", assuming no global is modified")
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
      | AddrOf lv -> process_lval lv
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
      | InfLoop body
      | DoWith (body, _) -> process_blk body
      | Goto _ -> Set.empty
      | Call FunId f -> get_fun f
      | Call FunDeref _ -> 
	  let res = ref Set.empty in
	  let collect f = res := Set.union (get_fun f) !res in
	    List.iter collect fids;
	    !res
      | UserSpec _ -> Set.empty
  in

    Hashtbl.iter init_fun prog.fundecs;
    begin try
      while true do
	let f = Queue.take todo in
	  current_fun := f;
(*	  print_verbose ("Analyzing: "^f); *)
	  let declaration = Hashtbl.find prog.fundecs f in
	  let used = process_blk declaration.body in
	    update_fun f used
      done;
    with Queue.Empty -> ()
    end;
    
    let res = Hashtbl.create 100 in
      Hashtbl.iter (fun f x -> Hashtbl.add res f (StrSet.elements x)) glb_tbl;
      res
	
