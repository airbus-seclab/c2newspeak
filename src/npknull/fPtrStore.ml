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

(* TODO: could be factored into a ListSet module in utils *)

let list_insert x l =
  let rec insert l =
    match l with
	y::tl when compare x y > 0 -> y::(insert tl)
      | _ -> x::l
  in
    insert l

let rec list_join l1 l2 =
  match (l1, l2) with
      (x1::tl1, x2::_) when compare x1 x2 < 0 -> x1::(list_join tl1 l2)
    | (x1::_, x2::tl2) when compare x1 x2 > 0 -> x2::(list_join l1 tl2)
    | (x::tl1, _::tl2) -> x::(list_join tl1 tl2)
    | ([], l) | (l, []) -> l

let rec list_contains l1 l2 =
  match (l1, l2) with
      (x1::tl1, x2::_) when compare x1 x2 < 0 -> list_contains tl1 l2
    | (x1::_, x2::_) when compare x1 x2 > 0 -> false
    | (_::tl1, _::tl2) -> list_contains tl1 tl2
    | (_, []) -> true
    | ([], _) -> false

module Map = Map.Make(Memloc)

type offset = int

type addr = Memloc.t * offset

type exp = 
    Lval of Memloc.t
  | AddrOfFun of string

(* for each location the names of functions it may point to *)
type t = string list Map.t

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
  let to_string m data =
    let m = Memloc.to_string m in
    let to_string f =
	res := !res^" "^m^" -> "^f
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

let eval s e = 
  match e with
      Lval m -> begin try Map.find m s with Not_found -> [] end
    | AddrOfFun f -> [f]

let assign m e s =
  let v = List.map (eval s) e in
  let v = List.flatten v in
    if v = [] then s 
    else begin
      let res = ref s in
	(* TODO: could be optimized+factored with FieldInsensitivePtrBuf!! *)
      let assign_memloc m =
	let info = ref (try Map.find m !res with Not_found -> []) in
	  List.iter (fun x -> info := list_insert x !info) v;
	  res := Map.add m !info !res
      in
	List.iter assign_memloc m;
	!res
    end
  
(*
(* TODO: think about it should not be an address but rather a function here! *)
let assign (m, _) f s = 
  let d = try Map.find m s with Not_found -> [] in
  let d = list_insert f d in
    Map.add m d s
*)

let remove_memloc = Map.remove

let split memlocs s = 
  let unreach = ref s in
  let reach = ref Map.empty in
    
  let transfer x =
    try
(* TODO: could be optimized with ad-hoc data-structure *)
      let v = Map.find x !unreach in
	unreach := Map.remove x !unreach;
	reach := Map.add x v !reach
    with Not_found -> ()
  in
    List.iter transfer memlocs;
    (!unreach, !reach)
    
(* TODO: O(n) expensive? 
   Have the inverse map?
   TODO: code very similar to shift??
*)
let transport tr s =
  let res = ref Map.empty in
  let subst m info =
    let m = Subst.apply tr m in
    let add_info m =
      let info = try list_join info (Map.find m !res) with Not_found -> info in
	res := Map.add m info !res
    in
      List.iter add_info m
  in
    Map.iter subst s;
    !res

(* TODO: very similar code with NonNullPtr and Field, how to factor?? *)
let compose s1 memlocs s2 =
  let res = ref s2 in
  let add_s1 m =
(* TODO: factor this try Map.find... with *)
    let info1 = try Map.find m s1 with Not_found -> [] in
    let info2 = try Map.find m s2 with Not_found -> [] in
    let info = list_join info1 info2 in
      res := Map.add m info !res
  in
    List.iter add_s1 memlocs;
    !res

let glue s1 s2 =
  let res = ref s1 in
  let add_info x y = res := Map.add x y !res in
    Map.iter add_info s2;
    !res

let read s m = try Map.find m s with Not_found -> []

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
