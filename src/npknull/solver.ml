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

module Map = Map.Make(String)
module Set = Set.Make(String)

type offset = int

(* set of location times offset, None is for unknown *)
type info = (Set.t * offset option)

(* map from locations: 
   G.global name, 
   L.local number, or 
   H.heap anonymous location 
   to association list from offset to info
*)
type store = (offset * info) list Map.t

(* None is for emptyset *)
(* TODO: try to get rid of this emptyset, use an exception instead!! *)
type t = store option

let universe () = Some Map.empty

let emptyset = None

let prepare_call _ _ = invalid_arg "Solver.prepare_call: not implemented yet"

let apply _ _ = invalid_arg "Solver.apply: not implemented yet"

(* TODO: this should be greatly improved!!! 
   right now O(n)*log(n)
   Too costly!!
*)
let join s1 s2 = 
  match (s1, s2) with
      (Some s1, Some s2) -> 
	let s = ref s2 in
	let add_info x d1 =
	  try
	    let d2 = Map.find x s2 in
	      if d1 <> d2 
	      then invalid_arg "Solver.join: not implemented yet <>"
	  with Not_found -> 
	    invalid_arg "Solver.join: not implemented yet Not_found"
	in
	  Map.iter add_info s1;
	  Some !s
    | (None, s) | (s, None) -> s

(* TODO: this should be greatly improved!!
   copy on write, patricia trie, partial recursion...
*)
let contains s1 s2 = 
  match (s1, s2) with
      (_, None) -> true
    | (None, _) -> false
    | (Some s1, Some s2) -> 
	let check_info x d1 =
	  try
	    let d2 = Map.find x s2 in
	      if d1 <> d2 
	      then invalid_arg "Solver.contains: not implemented yet"
	  with Not_found -> raise Exit
	in
	  try
	    Map.iter check_info s1;
	    true
	  with Exit -> false

(* TODO: find a way to remove emptyset/None!!! *)
let set_pointsto m1 m2 s = 
  match s with
      Some s -> Some (Map.add m1 ((0, (Set.singleton m2, Some 0))::[]) s)
    | None -> None

(* TODO: this is really awkward that this is needed!! *)
let memloc_is_valid s m = 
  match s with
      Some s -> begin
	try 
	  let s = Map.find m s in
	    List.mem_assoc 0 s
	with Not_found -> false
      end
    | None -> true

let memloc_of_local env v = "L."^string_of_int (env - v)

let process prog = 
  let current_loc = ref Newspeak.unknown_loc in
  let errors = ref StrSet.empty in
(* 
   list of functions to analyze
   when this list is empty, analysis ends
*)
(* TODO: could try a queue instead?? *)
  let todo = ref [] in
  let funtbl = Hashtbl.create 100 in
  let lbl_tbl = ref [] in

  let memloc_cnt = ref (-1) in
  let gen_memloc () = 
    incr memloc_cnt;
    "H."^string_of_int !memloc_cnt
  in

  let push lbl = lbl_tbl := (lbl, emptyset)::!lbl_tbl in
  let pop () = 
    match !lbl_tbl with
	(_, s)::tl -> 
	  lbl_tbl := tl;
	  s
      | [] -> invalid_arg "Solver.pop: unreachable code"
  in
  let goto lbl s = 
    let rec goto tbl =
      match tbl with
	  (lbl', s')::tl when lbl = lbl' -> 
	    let s = join s s' in
	      (lbl, s)::tl
	| hd::tl -> hd::(goto tl)
	| [] -> invalid_arg "Solver.goto: unreachable code"
    in
      lbl_tbl := goto !lbl_tbl
  in

  let print_err msg = 
    let msg = Newspeak.string_of_loc !current_loc^": "^msg in
      if not (StrSet.mem msg !errors) then begin
	errors := StrSet.add msg !errors;
	print_endline msg
      end
  in

  let warn_deref () = print_err "potential null pointer deref" in

  let exp_is_valid env s x =
    match x with
	Lval (Local n, Ptr) -> 
	  let v = memloc_of_local env n in
	    memloc_is_valid s v
      | _ -> false
  in

  let rec check_lval env s x = 
    match x with
	Global _ -> ()
      | Local _ -> ()
      | Shift (lv, e) -> 
	  check_lval env s lv;
	  check_exp env s e
      | Deref (e, _) -> if not (exp_is_valid env s e) then warn_deref ()

  and check_exp env s x = 
    match x with
	Const _ -> ()
      | AddrOf (lv, _) -> check_lval env s lv
      | AddrOfFun _ -> ()
      | Lval (lv, _) -> check_lval env s lv
      | UnOp (_, e) -> check_exp env s e
      | BinOp (_, e1, e2) -> 
	  check_exp env s e1;
	  check_exp env s e2
  in

  let rec process_blk x env s =
    match x with
	[] -> s
      | (x, loc)::tl -> 
	  current_loc := loc;
	  let s = process_stmtkind x env s in
	    process_blk tl env s
	      
  and process_stmtkind x env s =
    match x with
	Set (lv, e, _) -> 
	  check_lval env s lv;
	  check_exp env s e;
	  s
      | Decl (_, _, body) -> process_blk body (env+1) s
      | Call FunId f -> begin
	  try
	    let rel = Hashtbl.find funtbl f in
	      (* TODO: think about this, rewrite *)
	    let (is_new, init) = prepare_call rel s in
	      if is_new then todo := (f, init)::!todo;
	      apply rel s
	  with Not_found -> 
	    print_err ("missing function: "^f
		       ^", call ignored, analysis may be unsound");
	    s
	end
      | Select (br1, br2) -> 
	  let s1 = process_blk br1 env s in
	  let s2 = process_blk br2 env s in
	    join s1 s2
      | Guard e -> 
	  check_exp env s e;
	  s
(* TODO: change labels?? with the number of DoWith to traverse, 
   but harder to manipulate? *)
      | DoWith (body, lbl, action) -> 
	  push lbl;
	  let s1 = process_blk body env s in
	  let s2 = pop () in
	  let s2 = process_blk action env s2 in
	    join s1 s2
      | Goto lbl -> 
	  goto lbl s;
	  emptyset
      | InfLoop body -> 
	  let rec fixpoint x =
	    let x' = process_blk body env x in
	      if not (contains x x') then fixpoint (join x x')
	  in
	    fixpoint s;
	    emptyset
      | _ -> invalid_arg "Analysis.process_stmtkind: case not implemented"
  in

    (* initialization *)
    Hashtbl.iter (fun f _ -> Hashtbl.add funtbl f ()) prog.fundecs;
    let s = universe () in
    let s = process_blk prog.init 0 s in
    let s = set_pointsto "L.2" (gen_memloc ()) s in
      todo := ("main", (2, s))::[];
      
      (* fixpoint computation *)
      try
	while true do
	  match !todo with
	      (f, (env, s))::tl -> 
		todo := tl;
		let (_, body) = Hashtbl.find prog.fundecs f in
		let _ = process_blk body env s in
		  ()
	    | [] -> raise Exit
	done
      with Exit -> ()
