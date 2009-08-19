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

type offset = int

type addr = Memloc.t * offset

(* set of location times offset, None is for unknown *)
type info = 
    NotNull                 (* this value is not really nice, 
			       think about removing it *)
  | PointsTo of (Set.t * offset option)

(* map from locations: 
   G.global name, 
   L.local number, or 
   H.heap anonymous location 
   to association list from offset to info
*)
type t = (offset * info) list Map.t

let universe = Map.empty

let addr_is_valid s (m, o) =
  try List.mem_assoc o (Map.find m s)
  with Not_found -> false

let read_addr s (m, o) =
  let i = 
    try List.assoc o (Map.find m s)
    with Not_found -> raise Exceptions.Unknown
  in
  let (m, o) = 
    match i with
	PointsTo d -> d
      | _ -> raise Exceptions.Unknown
  in
  let m =
    match Set.elements m with
	m::[] -> m
      | _ -> raise Exceptions.Unknown
  in
    (m, o)

let write (m1, o) m2 s =
  (* TODO: could be optimized/made more precise *)
  if Map.mem m1 s then Map.remove m1 s
  else Map.add m1 ((o, PointsTo (Set.singleton m2, Some 0))::[]) s

let guard (m, o) s = if Map.mem m s then s else Map.add m ((o, NotNull)::[]) s

let forget_memloc = Map.remove

let string_of_info i =
  match i with
      NotNull -> "<> 0"
    | PointsTo (a, o) -> 
	let a = Set.elements a in
	let a =
	  match a with
	      a::[] -> Memloc.to_string a
	    | _ -> "{"^ListUtils.to_string Memloc.to_string "," a^"}"
	in
	let o =
	  match o with
	      None -> "?"
	    | Some x -> string_of_int x
	in
	  "("^a^", "^o^")"

let join s1 s2 =
  let s = ref s2 in
  let add_info x d1 =
    try
      let d2 = Map.find x s2 in
	if d1 <> d2 
	then invalid_arg "Store.join: not implemented yet <>"
    with Not_found -> ()
  in
    Map.iter add_info s1;
    !s

let to_string s =
  let res = ref "" in
  let to_string m data =
    let to_string (offset, info) =
      let m = Memloc.to_string m in
      let info = string_of_info info in
	res := !res^" ("^m^", "^string_of_int offset^") -> "^info
    in
      List.iter to_string data
  in
    Map.iter to_string s;
    !res

let contains s1 s2 =
  let check_info x d1 =
    try
      let d2 = Map.find x s2 in
	if d1 <> d2 
	then begin
	  let msg = "Store.contains: "^to_string s1^" && "^to_string s2 in
	    raise (Exceptions.NotImplemented msg)
	end
    with Not_found -> raise Exit
  in
    try
      Map.iter check_info s1;
      true
    with Exit -> false

let remove_memloc = Map.remove

(* TODO: O(n) expensive? *)
(* shifts and removes unreachable cells *)
let shift n1 s n2 =
  let n = n1-n2 in
  let res = ref Map.empty in
  let tr = ref [] in
  let shift_memloc x = 
    let y = Memloc.shift n x in
      if (x <> y) && not (List.mem_assoc x !tr) then tr := (x, y)::!tr;
      y
  in
  let todo = ref [] in
  let visited = ref [] in

  let shift_info (offset, x) =
    let x =
      match x with
	  PointsTo (m, o) -> 
	    let res = ref Set.empty in
	    let shift_memloc x = 
	      if not (List.mem x !visited) then todo := x::!todo;
	      res := Set.add (shift_memloc x) !res 
	    in
	      Set.iter shift_memloc m;
	      PointsTo (!res, o)
	| NotNull -> NotNull    
    in
      (offset, x)
  in

    for i = n to n1 do
      todo := (Memloc.of_local i)::!todo
    done; begin
      try
	while true do
	  match !todo with
	      [] -> raise Exit
	    | m::tl -> 
		visited := m::!visited;
		todo := tl;
		try
		  let info = Map.find m s in
		  let m = shift_memloc m in
		  let info = List.map shift_info info in
		    res := Map.add m info !res
		with Not_found -> ()
	done
      with Exit -> ()
    end;
    (!res, !tr)

(* TODO: O(n) expensive? 
   Have the inverse map?
   TOOD: code very similar to shift??
*)
let subst tr s = 
  let res = ref Map.empty in
  let subst_info x = 
    match x with
	PointsTo (m, o) -> 
	  let res = ref Set.empty in
	  let subst_memloc x = res := Set.add (Memloc.subst tr x) !res in
	    Set.iter subst_memloc m;
	    PointsTo (!res, o)
      | NotNull -> NotNull
  in
  let subst m info =
    let m = Memloc.subst tr m in
    let info = 
      List.map (fun (offset, info) -> (offset, subst_info info)) info
    in
      res := Map.add m info !res
  in
    Map.iter subst s;
    !res

(* TODO: O(n) expensive? *)
let unify_on dst n src =
  let todo = ref [] in
  let tr = ref [] in
  let unify_info_on v1 v2 =
    match (v1, v2) with
	(PointsTo (x1, Some o1), PointsTo (x2, Some o2)) -> begin
	  if o1 <> 0 
	  then invalid_arg "Store.unify_info_on: not implemented yet1";
	  if o2 <> 0 
	  then invalid_arg "Store.unify_info_on: not implemented yet2";
	  (* TODO: since always doing Set.elements, why not use a list?? *)
	  match (Set.elements x1, Set.elements x2) with
	      (x1::[], x2::[]) -> todo := (x1, x2)::!todo
	    | _ -> 
		print_endline (string_of_info v1);
		print_endline (string_of_info v2);
		invalid_arg "Store.unify_info_on: not implemented yet3"
	end
      | _ -> 
	  print_endline (string_of_info v1);
	  print_endline (string_of_info v2);
	  invalid_arg "Store.unify_info_on: not implemented yet4"
  in
    for i = 0 to n-1 do
      let m = Memloc.of_local i in
	todo := (m, m)::!todo
    done;
    begin
      try
	while true do
	  match !todo with
	      [] -> raise Exit
	    | (m1, m2)::tl -> 
		todo := tl;
		if Memloc.unify m1 m2 then tr := (m2, m1)::!tr;
		let v1 = 
		  try Some (Map.find m1 dst) 
		  with Not_found -> None 
		in
		let v2 = 
		  try Some (Map.find m2 src) 
		  with Not_found -> None 
		in
		  match (v1, v2) with
		      (None, None) -> ()
		    | (Some ((0, v1)::[]), Some ((0, v2)::[])) -> 
			unify_info_on v1 v2
		    | (None, _) -> ()
		    | (Some _, None) -> ()
		    | _ -> invalid_arg "Store.unify_on: not implemented yet"
	done
      with Exit -> ()
    end;
    subst !tr src


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
