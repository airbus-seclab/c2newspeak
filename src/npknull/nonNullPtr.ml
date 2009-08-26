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

type offset = int

type addr = Memloc.t * offset

(* Set of locations wich are nulls *)
type t = offset list Map.t

let universe = Map.empty

(* TODO: could be optimized!! by a map2 that doesn't traverse shared 
   subtrees *)
let join s1 s2 =
  let s = ref Map.empty in
  let add_info x d1 =
    try
      let d2 = Map.find x s2 in
	if d1 <> d2 
	then invalid_arg "Store.join: not implemented yet <>";
	s := Map.add x d1 !s
    with Not_found -> ()
  in
    Map.iter add_info s1;
    !s

let to_string s =
  let res = ref "" in
  let to_string m1 data =
    let m1 = Memloc.to_string m1 in
    let string_of_addr o = "("^m1^", "^string_of_int o^") <> 0" in
    let str = ListUtils.to_string string_of_addr ", " data in
      res := !res^" "^str
  in
    Map.iter to_string s;
    !res


(* TODO: could be optimized *)
let contains s1 s2 =
  let check x d1 =
    try
      let d2 = Map.find x s2 in
	if d1 <> d2 then begin
	  let msg = "Store.contains: "^to_string s1^" && "^to_string s2 in
	    raise (Exceptions.NotImplemented msg)
	end
    with Not_found -> raise Exit
  in
    try
      Map.iter check s1;
      true
    with Exit -> false

let read_addr _ _ = raise Exceptions.Unknown

(* TODO: could be factored with list_insert in fieldInsensitivePtr through
   a ListSet module!! *)
let list_insert x l =
  let rec insert l =
    match l with
	y::tl when compare x y > 0 -> y::(insert tl)
      | y::_ when compare x y = 0 -> l
      | _ -> x::l
  in
    insert l

let write_nonnull (m, o) s = 
(* TODO: could be optimized *)
  let d = try Map.find m s with Not_found -> [] in
  let d = list_insert o d in
    Map.add m d s
  
let assign a _ s = write_nonnull a s

let addr_is_valid s (m, o) = 
  try 
    let d = Map.find m s in
      List.mem o d
  with Not_found -> false

let guard = write_nonnull

let remove_memloc = Map.remove

(* most probably incorrect, unsound, such a primitive shouldn't exist! *)
let forget_memloc = Map.remove

let split memlocs s = 
  let unreach = ref s in
  let reach = ref Map.empty in

  let add_memloc x =
    try
      let d = Map.find x !unreach in
	reach := Map.add x d !reach;
	unreach := Map.remove x !unreach
    with Not_found -> ()
  in
    
    List.iter add_memloc memlocs;
    (!unreach, !reach)
    
(* TODO: O(n) expensive? 
   Have the inverse map?
   TOOD: code very similar to shift??
*)
let transport tr s =
  let res = ref Map.empty in
  let subst m i =
    let m = Memloc.subst tr m in
      res := Map.add m i !res
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

let build_transport src memlocs dst =
  print_endline "Store.build_transport starts";
  print_endline (to_string src);
  List.iter (fun x -> print_endline (Memloc.to_string x)) memlocs;
  print_endline (to_string dst);
  let x = build_transport src memlocs dst in
    print_endline "Store.build_transport ends";
    x
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
(*
let guard m s =
  print_endline "NonNullPtr.guard";
  print_endline (to_string s);
  let s = guard m s in
    print_endline (to_string s);
    print_endline "NonNullPtr.guard ends";
    s
*)
