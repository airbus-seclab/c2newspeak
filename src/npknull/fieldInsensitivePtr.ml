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

(* TODO: could be factored into a ListSet module in utils *)

let list_insert x l =
  let rec insert l =
    match l with
	y::tl -> 
	  let c = Memloc.compare x y in
	    if c = 0 then l
	    else if c < 0 then x::l
	    else y::(insert tl)
      | _ -> x::l
  in
    insert l

let rec list_join l1 l2 =
  match (l1, l2) with
      (x1::tl1, x2::_) when Memloc.compare x1 x2 < 0 -> x1::(list_join tl1 l2)
    | (x1::_, x2::tl2) when Memloc.compare x1 x2 > 0 -> x2::(list_join l1 tl2)
    | (x::tl1, _::tl2) -> x::(list_join tl1 tl2)
    | ([], l) | (l, []) -> l

let rec list_contains l1 l2 =
  match (l1, l2) with
      (x1::tl1, x2::_) when Memloc.compare x1 x2 < 0 -> list_contains tl1 l2
    | (x1::_, x2::_) when Memloc.compare x1 x2 > 0 -> false
    | (_::tl1, _::tl2) -> list_contains tl1 tl2
    | (_, []) -> true
    | ([], _) -> false

module Map = Map.Make(Memloc)

type t = Memloc.t list Map.t

let universe = Map.empty

(* TODO: could be optimized!! by a map2 that doesn't traverse shared 
   subtrees *)
let join s1 s2 =
  let res = ref s1 in
  let add_info x d2 =
    let d1 = try Map.find x s1 with Not_found -> [] in
    let d = list_join d1 d2 in
      res := Map.add x d !res
  in
    Map.iter add_info s2;
    !res

let to_string s =
  let res = ref "" in
  let to_string m1 data =
    let m1 = Memloc.to_string m1 in
    let to_string m2 =
      let m2 = Memloc.to_string m2 in
	res := !res^" "^m1^" -> "^m2
    in
      List.iter to_string data
  in
    Map.iter to_string s;
    !res


(* TODO: could be optimized *)
let contains s1 s2 =
  let check x d2 =
    try
      let d1 = Map.find x s1 in
	if not (list_contains d1 d2) then raise Exit
    with Not_found -> raise Exit
  in
    try
      Map.iter check s2;
      true
    with Exit -> false

let read_addr s (m, _) =
  let x = try Map.find m s with Not_found -> [] in
    match x with
	m::[] -> Some (m, None)
      | [] -> None
      | _ -> raise Exceptions.Unknown

let assign (m1, _) (m2, _) s =
(* TODO: could be optimized *)
  let d = try Map.find m1 s with Not_found -> [] in
  let d = list_insert m2 d in
    Map.add m1 d s

let addr_is_valid _ _ = false

let guard _ s = s

let remove_memloc = Map.remove

let build_transport src memlocs dst =
  let todo = ref [] in
  let res = ref Subst.identity in
    
  let add_assoc x y = 
    todo := (x, y)::!todo;
    if x <> y then res := Subst.assoc x y !res
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
	      let v2 = try Map.find m2 dst with Not_found -> [] in 
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
		todo := v@(!todo)
	  | [] -> raise Exit
      done
    with Exit -> ()
    end;
    (!unreach, !reach, !vars)
    
(* TODO: O(n) expensive? 
   Have the inverse map?
   TOOD: code very similar to shift??
*)
let transport tr s =
  let res = ref Map.empty in
  let subst m info =
    let m = Subst.apply tr m in
    let info = List.map (Subst.apply tr) info in
      res := Map.add m info !res
  in
    Map.iter subst s;
    !res

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
