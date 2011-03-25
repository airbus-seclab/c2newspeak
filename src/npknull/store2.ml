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

open State2.PtrSyntax

(* TODO: try to factor this code with pointsTo ? *)

module Make(Subst: Transport.T) =
struct
   
  let cnt = ref (-1)
    
  let fresh_node () = 
    if (!cnt = max_int) then invalid_arg "No more fresh nodes available";
    incr cnt;
    "heap"^string_of_int !cnt
      
  module VarMap = Map.Make(String)
    
  type succ = VarSet.t
      
  let join_nodes = VarSet.union
    
  let node_is_subset = VarSet.subset
    
  type t = succ VarMap.t
      
  let print s =
    let print_pointsto x y =
      let succ = VarSet.to_string y in
	print_endline (x^" -> "^succ);
    in
      VarMap.iter print_pointsto s
	
  let universe () = VarMap.empty
    
  let deref s pointers =
    let result = ref VarSet.empty in
    let deref_one node =
      try
	let v = VarMap.find node s in
	  result := VarSet.union !result v
      with Not_found -> ()
    in
      VarSet.iter deref_one pointers;
      !result

  let deref s e =
    let rec fixpoint_deref v =
      let v' = deref s v in
	if VarSet.subset v' v then v else fixpoint_deref (VarSet.union v v')
    in
    let rec eval_exp e =
      match e with
	  Empty -> VarSet.empty
	| Var x -> VarSet.singleton x
	| Deref e -> 
	    let p = eval_exp e in
	      deref s p
	| Join (e1, e2) ->
	    let v1 = eval_exp e1 in
	    let v2 = eval_exp e2 in
	      VarSet.union v1 v2
	| InfDeref e -> fixpoint_deref (eval_exp e)
    in
      eval_exp e
	
  let add_pointsto x nodes store =
    if VarSet.is_empty nodes then store
    else begin
      try 
	let prev = VarMap.find x store in
	  VarMap.add x (join_nodes prev nodes) store
      with Not_found -> VarMap.add x nodes store
    end
      
  let add_several_pointsto src dst graph =
    let result = ref graph in
      VarSet.iter (fun x -> result := add_pointsto x dst !result) src;
      !result
	
  let assign dst src s =
    let dst = deref s dst in
    let src = deref s src in
      add_several_pointsto dst src s
	
  let join store1 store2 =
    let result = ref store1 in
      VarMap.iter (fun x v -> result := add_pointsto x v !result) store2;
      !result
	
  let is_subset store1 store2 =
    let pointsto_is_in_store2 x v1 =
      try
	let v2 = VarMap.find x store2 in
	  if not (node_is_subset v1 v2) then raise Exit
      with Not_found -> raise Exit
    in
      try 
	VarMap.iter pointsto_is_in_store2 store1;
	true
      with Exit -> false
	
  (* TODO: this efficiently should have both predecessor and successor list *)
  let remove_variables variables store = 
    let result = ref VarMap.empty in
    let remove_variables x v =
      if not (List.mem x variables) then begin
	let nodes = VarSet.remove_variables variables v in
	  result := VarMap.add x nodes !result
      end
    in
      VarMap.iter remove_variables store;
      !result
	
  (* shoud return: 
     the list of all variables that are reachable from root_variables, 
     the part of the store that is reachable,
     the part of the store that is unreachable
  *)
  let split root_variables store = 
    let visited_variables = ref root_variables in
    let unreachable = ref store in
    let result = ref VarMap.empty in
    let todo = ref root_variables in
    let add_todo y = 
      if not (List.mem y !visited_variables) && not (List.mem y !todo)
      then todo := y::!todo
    in
      begin try
	while true do
	  match !todo with
	      x::tl -> 
		visited_variables := x::!visited_variables;
		todo := tl;
		begin try
		  let v = VarMap.find x !unreachable in
		    unreachable := VarMap.remove x !unreachable;
		    result := VarMap.add x v !result;
		    VarSet.iter add_todo v
		with Not_found -> ()
		end
	    | [] -> raise Exit
	done
      with Exit -> ()
      end;
      (!visited_variables, !result, !unreachable)
	
  let substitute subst state =
    let result = ref VarMap.empty in
    let substitute_pointsto x v =
      let x = Subst.apply subst x in
      let v = Subst.apply_set subst v in
	VarSet.iter (fun x -> result := add_pointsto x v !result) x;
    in
      VarMap.iter substitute_pointsto state;
      !result
	
  let list_nodes state =
    let result = ref (VarSet.empty) in
    let add_nodes x y =
      result := VarSet.add x !result;
      result := VarSet.union y !result
    in
      VarMap.iter add_nodes state;
      !result
	
  let restrict nodes state =
    let result = ref VarMap.empty in
    let add_edge x y =
      if (VarSet.mem x nodes) then begin
	let y = VarSet.inter y nodes in
	  result := VarMap.add x y !result
      end
    in
      VarMap.iter add_edge state;
      !result
	
  let merge_successors roots state = 
    let is_root x = List.mem x roots in
    let result = ref state in
    let subst = ref (Subst.identity ()) in
    let todo = ref roots in
      (* TODO: optimization, use a set instead of a list, not efficient *)
    let processed = ref VarSet.empty in
      begin try
	while true do
	  match !todo with
	      x::tl -> 
		todo := tl;
		processed := VarSet.add x !processed;
		begin
		  try
		    let succ = VarMap.find x !result in
		      result := VarMap.remove x !result;
		      let (root_succ, succ) = VarSet.partition is_root succ in
		      let succ = 
			match VarSet.elements succ with
			    [] -> VarSet.empty
			  | y0::tl -> 
			      (* merge all states (that are not in roots!) together 
				 and add to the substitution *)
			      let merge_node y = 
				subst := Subst.associate y y0 !subst;
			      in
				List.iter merge_node tl;
				(* TODO: this may be costly => could be done better? *)
				result := substitute !subst !result;
				if (List.length tl > 0) 
				then processed := VarSet.remove y0 !processed;
				if not (VarSet.mem y0 !processed)
				then todo := y0::!todo;
				VarSet.singleton y0
		      in
		      let succ = VarSet.union root_succ succ in
			result := add_pointsto x succ !result
		  with Not_found -> ()
		end
	  | [] -> raise Exit
	done
      with Exit -> ()
      end;
      (!result, !subst)
	
  let get_heap_succ state roots x =
    let y = VarMap.find x state in
    let y = VarSet.diff y (VarSet.of_list roots) in
      if (VarSet.cardinal y > 1) 
      then invalid_arg "Store3.extract_heap: should be unreachable";
      VarSet.choose y
	
  let normalize roots state =
    let (state, subst) = merge_successors roots state in
      (* step 2: rename all nodes that are not in roots to anonymous variable *)
    let subst = ref subst in
    let todo = ref roots in
      (* TODO: optimization: use a set rather than a list, not efficient *)
    let visited = ref [] in
      begin try
	while true do
	  match !todo with
	      x::tl -> 
		todo := tl;
		begin try
		  let y = get_heap_succ state roots x in
		    if not (List.mem y !visited) then begin
		      let y' = fresh_node () in
			subst := Subst.associate y y' !subst;
			visited := y::!visited;
			todo := y::!todo
		    end
		with Not_found -> ()
		end
	    | [] -> raise Exit
	done
      with Exit -> () 
      end;
      (substitute !subst state, !subst)
	
  let transport roots state input = 
    let result = ref (Subst.identity ()) in
      (* 2: build the substitution so that they recursively match *)
      (* TODO: think about loops *)
    let todo = ref (List.combine roots roots) in
      (* TODO: optimization: use a set instead of a list here *)
    let visited = ref [] in
      begin try
	while true do
	  match !todo with
	      (x1, x2)::tl -> 
		todo := tl;
		begin try
		  (* TODO: could try to always add y1 instead? *)
		  let y1 = get_heap_succ state roots x1 in
		  let y2 = get_heap_succ input roots x2 in
		    result := Subst.associate y1 y2 !result;
		    if not (List.mem y1 !visited) then begin
		      visited := y1::!visited;
		      todo := (y1, y2)::!todo
		    end
		with Not_found -> ()
		end
	    | [] -> raise Exit
	done
      with Exit -> ()
      end;
      !result
	
  let glue = join
    
  let satisfies store (e1, e2) =
    let s1 = deref store e1 in
    let s2 = deref store e2 in
      (VarSet.is_empty (VarSet.inter s1 s2))
	      
  let eval_exp s e = State2.Variables (deref s e)

(*
  let normalize roots store =
  print_endline "Normalize";
  print_endline (ListUtils.to_string ", " (fun x -> x) roots);
  print store;
  print_endline "=>";
  let (result, subst) = normalize roots store in
    print result;
    print_endline (Subst2.to_string subst);
    (result, subst)
*)
(*
let transport roots state input =
  print_endline "Transport";
  print_endline (ListUtils.to_string ", " (fun x -> x) roots);
  print state;
  print_endline "with";
  print input;
  let subst = transport roots state input in
    print_endline "=>";
    print_endline ("subst: "^Subst2.to_string subst);
    subst
*)
end

module Test = Make(Subst2)

let test0 () =
  print_string "Store3.test0...";
  let store = Test.universe () in
  let store = Test.assign (Var "x") (Var "y") store in
  let (store, _) = Test.normalize ["x"] store in
  let v = Test.deref store (Deref (Var "x")) in
  let v = VarSet.choose v in
    if (v <> "heap0") then failwith "merge should not have changed store";
    print_endline "OK"

let test1 () =
  print_string "Store3.test1...";
  let store = Test.universe () in
  let store = Test.assign (Var "x") (Var "a") store in
  let store = Test.assign (Var "y") (Var "heap0") store in
  let (store, _) = Test.normalize ["x"] store in
  let v1 = Test.deref store (Deref (Var "x")) in
  let v2 = Test.deref store (Deref (Var "y")) in
    if not (VarSet.is_empty (VarSet.inter v1 v2))
    then failwith "normalize should not merge capture variables";
    print_endline "OK"

let test2 () =
  print_string "Store3.test2...";
  let store = Test.universe () in
  let store = Test.assign (Var "x") (Var "y") store in
  let (_, subst) = Test.normalize ["x"] store in
    if (VarSet.choose (Subst2.apply subst "y") = "y") 
    then failwith "normalize should change heap variables";
    print_endline "OK"

let test3 () = 
  print_string "Store3.test3...";
  let store = Test.universe () in
  let store = Test.assign (Var "x") (Var "z") store in
  let store = Test.assign (Var "z") (Var "t") store in
  let (store, _) = Test.normalize ["x"] store in
  let v = Test.deref store (Deref (Deref (Var "x"))) in
    if (VarSet.is_empty v) then failwith "normalize should not change pointers";
    print_endline "OK"

let test4 () = 
  print_string "Store3.test4...";
  let store = Test.universe () in
  let store1 = Test.assign (Var "x") (Var "y1") store in
  let store1 = Test.assign (Var "y1") (Var "z1") store1 in
  let store2 = Test.assign (Var "x") (Var "y2") store in
  let store2 = Test.assign (Var "y2") (Var "z2") store2 in
  let subst = Test.transport ["x"] store1 store2 in
  let v = VarSet.choose (Subst2.apply subst "z1") in
    if (v <> "z2") then failwith "transport should go recursively";
    print_endline "OK"

let test5_normalize_should_not_loop () =
  print_string "Store3.test5...";
  let store = Test.universe () in
  let store = Test.assign (Var "x") (Var "x") store in
  let _ = Test.normalize ["x"] store in
    print_endline "OK"

let test6_normalize_should_not_loop () =
  print_string "Store3.test6...";
  let store = Test.universe () in
  let store = Test.assign (Var "x") (Var "y") store in
  let store = Test.assign (Var "y") (Var "y") store in
  let _ = Test.normalize ["x"] store in
    print_endline "OK"

let test7_transport_should_not_loop () =
  print_string "Store3.test7...";
  let store = Test.universe () in
  let store1 = Test.assign (Var "x") (Var "x") store in
  let store2 = Test.assign (Var "x") (Var "x") store in
  let _ = Test.transport ["x"] store1 store2 in
    print_endline "OK"

let test8 () =
  print_string "Store3.test8...";
  let store = Test.universe () in
  let store = Test.assign (Var "a") (Var "x") store in
  let store = Test.assign (Var "b") (Var "x") store in
  let (store, _) = Test.normalize ["a"; "b"] store in
  let v = Test.deref store (Deref (Var "a")) in
    if (VarSet.cardinal v <> 1) then failwith "should have only one successor";
    print_endline "OK"

let test9 () =
  print_string "Store3.test9...";
  let store = Test.universe () in
  let store = Test.assign (Var "x") (Var "a") store in
  let store = Test.assign (Var "x") (Var "b") store in
  let store = Test.assign (Var "x") (Var "c") store in
  let _ = Test.normalize ["x"] store in
    print_endline "OK"

let test10 () =
  print_string "Store3.test10...";
  let store = Test.universe () in
  let store1 = Test.assign (Var "x1") (Var "h") store in
  let store1 = Test.assign (Var "x1") (Var "x2") store1 in
  let _ = Test.transport ["x1"; "x2"] store1 store in
    print_endline "OK"

let test11 () =
  print_string "Store3.test11...";
  let store = Test.universe () in
  let store1 = Test.assign (Var "f") (Var "h2") store in
  let store1 = Test.assign (Var "a1") (Var "a1") store1 in
  let store1 = Test.assign (Var "a1") (Var "h2") store1 in
  let store2 = Test.assign (Var "f") (Var "h1") store in
  let _ = Test.transport ["f"; "a1"] store1 store2 in
    print_endline "OK"

let test12 () =
  print_string "Store3.test12...";
  let store = Test.universe () in
  let store = Test.assign (Var "x") (Var "e") store in
  let store = Test.assign (Var "x") (Var "x") store in
  let store = Test.assign (Var "y") (Var "x") store in
  let roots = ["e"; "y"] in
  let (store, _) = Test.normalize roots store in
  let v = Test.deref store (Deref (Deref (Var "y"))) in
  let v = VarSet.diff v (VarSet.of_list roots) in
    if (VarSet.cardinal v > 1) 
    then failwith "should not point to more than one heap variable";
    print_endline "OK"

let test13 () =
  print_string "Store3.test13...";
  let store = Test.universe () in
  let store = Test.assign (Var "a") (Var "x") store in
  let store = Test.assign (Var "x") (Var "x") store in
  let store = Test.assign (Var "x") (Var "y") store in
  let store = Test.assign (Var "y") (Var "z") store in
  let _ = Test.normalize ["a"] store in
    print_endline "OK"

let test () =
  test0 ();
  test1 ();
  test2 ();
  test3 ();
  test4 ();
  test5_normalize_should_not_loop ();
  test6_normalize_should_not_loop ();
  test7_transport_should_not_loop ();
  test8 ();
  test9 ();
  test10 ();
  test11 ();
  test12 ();
  test13 ()
