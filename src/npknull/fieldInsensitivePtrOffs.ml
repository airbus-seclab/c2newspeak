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

module Map = Map.Make(Memloc)
module Set = Set.Make(Memloc)

(* TODO: try to factor as much code as possible with fieldInsensitiveBuf *)

(* offset, delta, size of the zone *)
type num_pred = (int * int) option * int option

type exp =
    Lval of Memloc.t
  | AddrOf of (Memloc.t * num_pred)

let join_num_pred (o1, n1) (o2, n2) = 
  let o = if o1 = o2 then o1 else None in
  let n = if n1 = n2 then n1 else None in
    (o, n)

let contains_num_pred (o1, n1) (o2, n2) =
  ((o1 = None) || (o1 = o2)) && ((n1 = None) || (n1 = n2))

let string_of_num_pred y p = 
  match p with
      (Some (o, delta), Some n) -> 
	"<"^y^", ("^string_of_int o^", "^string_of_int delta
	^", "^string_of_int n^")>"
    | (Some (o, delta), None) ->
	"<"^y^", ("^string_of_int o^", "^string_of_int delta^", ?)>"
    | (None, Some n) -> "<"^y^", (?, ?, "^string_of_int n^")>"
    | (None, None) -> y


type info = (Memloc.t * num_pred) list

(* TODO: could be factored into a ListSet module in utils *)
let insert_info (x, o) l =
  let rec insert l =
    match l with
	(y, _ as hd)::tl when Memloc.compare x y > 0 -> hd::(insert tl)
      | (y, o')::tl when Memloc.compare x y = 0 -> (x, join_num_pred o o')::tl
      | _ -> (x, o)::l
  in
    insert l

let rec join_info l1 l2 =
  match (l1, l2) with
      ((x1, _ as hd)::tl1, (x2, _)::_) when Memloc.compare x1 x2 < 0 -> 
	hd::(join_info tl1 l2)
    | ((x1, _)::_, (x2, _ as hd)::tl2) when Memloc.compare x1 x2 > 0 -> 
	hd::(join_info l1 tl2)
    | ((x, o1)::tl1, (_, o2)::tl2) -> 
	(x, join_num_pred o1 o2)::(join_info tl1 tl2)
    | ([], l) | (l, []) -> l

let rec contains_info l1 l2 =
  match (l1, l2) with
      ((x1, _)::tl1, (x2, _)::_) when Memloc.compare x1 x2 < 0 -> 
	contains_info tl1 l2
    | ((x1, _)::_, (x2, _)::_) when Memloc.compare x1 x2 > 0 -> false
    | ((_, o1)::tl1, (_, o2)::tl2) -> 
	contains_num_pred o1 o2 && contains_info tl1 tl2
    | (_, []) -> true
    | ([], _) -> false

let string_of_info x (y, o) =
  let y = Memloc.to_string y in
  let y = string_of_num_pred y o in
    x^" -> "^y

let memlocs_of_info v = List.map (fun (x, _) -> x) v

let subst_info tr v = 
  let res = ref [] in
  let add_info (x, o) =
    let x = Subst.apply tr x in
      List.iter (fun x -> res := insert_info (x, o) !res) x
  in
    List.iter add_info v;
    !res

type t = info Map.t

let universe = Map.empty

(* TODO: could be optimized!! by a map2 that doesn't traverse shared 
   subtrees *)
let join s1 s2 =
  let res = ref s1 in
  let add_info x d2 =
    let d1 = try Map.find x s1 with Not_found -> [] in
    let d = join_info d1 d2 in
      res := Map.add x d !res
  in
    Map.iter add_info s2;
    !res

let to_string s =
  let res = ref "" in
  let to_string m info =
    let m = Memloc.to_string m in
      List.iter (fun x -> res := !res^" "^string_of_info m x) info
  in
    Map.iter to_string s;
    !res


(* TODO: could be optimized *)
let contains s1 s2 =
  let check x d2 =
    try
      let d1 = Map.find x s1 in
	if not (contains_info d1 d2) then raise Exit
    with Not_found -> raise Exit
  in
    try
      Map.iter check s2;
      true
    with Exit -> false

let read s m = try Map.find m s with Not_found -> raise Exceptions.Emptyset

let eval s e =
  match e with
      Lval m -> begin
	try Map.find m s 
	with Not_found -> []
      end
    | AddrOf v -> v::[]


let assign m e s =
  let v = List.map (eval s) e in
  let v = List.flatten v in
    if v = [] then s 
    else begin
      let res = ref s in
      let assign_memloc m =
	(* TODO: could be optimized *)
	let info = ref (try Map.find m !res with Not_found -> []) in
	  List.iter (fun x -> info := insert_info x !info) v;
	  res := Map.add m !info !res
      in
	List.iter assign_memloc m;
	!res
    end
  
let addr_is_valid _ _ = false

let guard _ s = s

let remove_memloc = Map.remove

let build_transport src memlocs dst =
  let todo = ref [] in
  let res = ref Subst.identity in
  let visited = ref [] in

  let add_assoc x y = 
    if not (List.mem (x, y) !visited) then begin
      visited := (x, y)::!visited;
      res := Subst.assoc x y !res;
      todo := (x, y)::!todo
    end
  in

  let rec unify_values v1 v2 =
    match (v1, v2) with
	(v1::tl1, v2::tl2) -> 
	  add_assoc v1 v2;
	  unify_values tl1 tl2
      | ([], []) -> ()
(* TODO: if transport could suppress locations, there could be less 
   Unknown exceptions here??
   so there could be less distinct pre/post tuple in the global tabulation
   would be less re-analyses of the same function...
*)
      | _ -> raise Exceptions.Unknown
  in
    
    List.iter (fun x -> add_assoc x x) memlocs;
    begin try
      while true do
	match !todo with
	    (m1, m2)::tl ->
	      todo := tl;
	      let v1 = try Map.find m1 src with Not_found -> [] in
	      let v1 = memlocs_of_info v1 in
	      let v2 = try Map.find m2 dst with Not_found -> [] in 
	      let v2 = memlocs_of_info v2 in
		unify_values v1 v2;
	  | [] -> raise Exit
      done
    with Exit -> ()
    end;
    !res

let split memlocs s = 
  let todo = ref memlocs in
  let unreach = ref s in
  let reach = ref Map.empty in
  let vars = ref [] in

    begin try
      while true do
	match !todo with
	    x::tl -> 
	      todo := tl;
	      vars := x::!vars;
	      let v = try Map.find x !unreach with Not_found -> [] in
		if v <> [] then reach := Map.add x v !reach;
		unreach := Map.remove x !unreach;
		(* consider only the memory locations *)
		let v = memlocs_of_info v in
		  todo := v@(!todo)
	  | [] -> raise Exit
      done
    with Exit -> ()
    end;
    (!unreach, !reach, !vars)
  

(* tr is a substition of many to one
   TODO: O(n) expensive? 
   Have the inverse map?
   TOOD: code very similar to shift??
   shouldn't it be better to iterated on the substitution only?? and keep the
   map unchanged?? 
*)
let transport tr s =
  let res = ref Map.empty in
  let subst m info =
    let m = Subst.apply tr m in
    let info = subst_info tr info in
    let add_info m = 
      let info = try join_info info (Map.find m !res) with Not_found -> info in
	res := Map.add m info !res
    in
      List.iter add_info m
  in
    Map.iter subst s;
    !res

    
(* TODO: think about it, factor this code with transport!! *)
let transport_invert = transport

(* TODO: very similar code with NonNullPtr and FPtrStore, how to factor?? *)
let compose s1 memlocs s2 =
  let res = ref s2 in
  let add_s1 m =
(* TODO: factor this try Map.find... with *)
    let info1 = try Map.find m s1 with Not_found -> [] in
    let info2 = try Map.find m s2 with Not_found -> [] in
    let info = join_info info1 info2 in
      res := Map.add m info !res
  in
    List.iter add_s1 memlocs;
    !res

(*
  merges nodes that are indistiguashible by access path from memlocs
  pseudo-code:
  P = { {x} | x in Init } \cup { x | notin Init }
  W = { {x} | x in Init }
  while W <> 0 do
    get X from W
    Y = succ (X)
    for all R in P such that R \inter Y <> 0 and R notincludedin Y do
      R1 = R \inter Y
      R2 = R \ R1
      P = (P\ {R}) \cup {R1, R2}
      if R \in W then W = (W\{R}) \cup {R1, R2} 
      else W = W \cup R1
    done
  done
*)
let normalize memlocs s =
  let succ x = 
    let res = ref Set.empty in
    let add_succ x =
      try 
	let y = Map.find x s in
	let y = memlocs_of_info y in
	  List.iter (fun x -> res := Set.add x !res) y
      with Not_found -> ()
    in
      Set.iter add_succ x;
      !res
  in


  (* mapping from variables to equivalence class (set of variables) *)
  let partition = ref Map.empty in
  (* worklist of sets of variables to process *)
  let worklist = ref [] in

  let add_partition nodes =
    Set.iter (fun x -> partition := Map.add x nodes !partition) nodes
  in    

  let init x = 
    let set_x = Set.singleton x in
      partition := Map.add x set_x !partition;
      worklist := set_x::!worklist
  in

    List.iter init memlocs;
    begin
      try
	while true do
	  match !worklist with
	      x::tl -> 
		worklist := tl;
		let y = succ x in
		let new_nodes = ref Set.empty in
		let nodes = ref [] in
		let add_node n =
		  try
		    let r = Map.find n !partition in
		      if not (Set.subset r y) then begin
			if not (List.mem r !nodes) then begin
			  nodes := r::!nodes
			end
		      end
		  with Not_found -> new_nodes := Set.add n !new_nodes
		in
		  Set.iter add_node y;
		  
		  if not (Set.is_empty !new_nodes) then begin
		    add_partition !new_nodes;
		    worklist := !new_nodes::!worklist
		  end;
		  
		  let add_node r =
		    let r1 = Set.inter r y in
		    let r2 = Set.diff r y in
		      add_partition r1;
		      add_partition r2;
		      worklist := r1::!worklist
		  in
		  List.iter add_node !nodes;
		  ()

	    | [] -> raise Exit
	done
      with Exit -> ()
    end;
    
    (* a list of nodes to merge *)
    let tr = ref Subst.identity in
    let add_merge x s = 
      if Set.cardinal s <> 1 then begin
	let y = Set.min_elt s in
	  tr := Subst.assoc x y !tr;
      end
    in
      Map.iter add_merge !partition;
      
      !tr

let glue s1 s2 =
  let res = ref s1 in
  let add_info x y = res := Map.add x y !res in
    Map.iter add_info s2;
    !res

(* usefull for debug *)
(*
let shift n1 s n2 =
  print_endline "Store.shift";
  print_endline (string_of_int n1);
  print_endline (string_of_int n2);
  print_endline (to_string s);
  let (s, tr) = shift n1 s n2 in
    print_endline (to_string s);
    print_endline "Store.shift ends";
    (s, tr)
*)
(*
let unify_on dst n src =
  print_endline "Store.unify_on";
  print_endline (to_string dst);
  print_endline (to_string src);
  let s = unify_on dst n src in
    print_endline (to_string s);
    print_endline "Store.unify_on ends";
    s

let contains s1 s2 =
  print_endline "Store.contains";
  print_endline (to_string s1);
  print_endline (to_string s2);
  let r = contains s1 s2 in
    print_endline (string_of_bool r);
    print_endline "Store.contains ends";
    r
*)
(*
let build_transport src memlocs dst =
  print_endline "Store.build_transport starts";
  print_endline (to_string src);
  List.iter (fun x -> print_endline (Memloc.to_string x)) memlocs;
  print_endline (to_string dst);
  let tr = build_transport src memlocs dst in
    print_endline (Subst.to_string tr);
    print_endline "Store.build_transport ends";
    tr
*)
(*
let join s1 s2 =
  print_endline "Store.join";
  print_endline (to_string s1);
  print_endline (to_string s2);
  let s = join s1 s2 in
    print_endline (to_string s);
    print_endline "Store.join ends";
    s
*)

let test1 () =
  print_string "npknull/FieldInsensitivePtrOffs.test1... ";
  let exp_of_var x = [AddrOf (x, (None, None))] in

  let a = Memloc.of_global "a" in
  let b = Memloc.of_global "b" in
  let c = Memloc.of_global "c" in
  let d = Memloc.of_global "d" in

  let src = universe in
    
  let src = assign [a] (exp_of_var d) src in
  let src = assign [b] (exp_of_var d) src in

  let dst = universe in

  let dst = assign [a] (exp_of_var d) dst in
  let dst = assign [b] (exp_of_var c) dst in

    try
      let _ = build_transport src [b; a] dst in
	invalid_arg "Failed"
    with Exceptions.Unknown -> print_endline "OK"

let test () =
  test1 ()
