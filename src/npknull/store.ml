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
	  with Not_found -> s := Map.add x d1 !s
  in
    Map.iter add_info s1;
    !s

let contains s1 s2 =
  let check_info x d1 =
    try
      let d2 = Map.find x s2 in
	if d1 <> d2 
	then invalid_arg "Store.contains: not implemented yet"
    with Not_found -> raise Exit
  in
    try
      Map.iter check_info s1;
      true
    with Exit -> false

let remove_memloc = Map.remove

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

let shift n s =
  let res = ref Map.empty in
  let shift_info x =
    match x with
	PointsTo (m, o) -> 
	  let res = ref Set.empty in
	  let shift_memloc x = res := Set.add (Memloc.shift n x) !res in
	    Set.iter shift_memloc m;
	    PointsTo (!res, o)
      | NotNull -> NotNull
  in
  let shift m info =
    let m = Memloc.shift n m in
    let info = 
      List.map (fun (offset, info) -> (offset, shift_info info)) info
    in
      res := Map.add m info !res
  in
    Map.iter shift s;
    !res

(*
(* usefull for debug *)
let shift n s =
  print_endline "Store.shift";
  print_endline (string_of_int n);
  print_endline (to_string s);
  let s = shift n s in
    print_endline (to_string s);
    print_endline "Store.shift ends";
    s
*)
(*
let contains s1 s2 =
  print_endline "Store.contains";
  print_endline "Store.contains ends";
*)
