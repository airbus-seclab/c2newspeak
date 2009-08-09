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

type memloc = string

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

