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

type t = (Memloc.t * Memloc.t list) list

let to_string tr =
  let string_of_assoc (x, y) = 
    let y = ListUtils.to_string Memloc.to_string ", " y in
      Memloc.to_string x^" -> { "^y^" }"
  in
    "["^ListUtils.to_string string_of_assoc ", " tr^"]"

let identity = []

let invert tr =
  let res = ref [] in
  let add_assoc y x =
    try 
      let im = List.assoc y !res in
      let tl = List.remove_assoc y !res in
	res := (y, x::im)::tl
    with Not_found -> res := (y, x::[])::!res
  in
  let invert (x, y) = List.iter (fun y -> add_assoc y x) y in
    List.iter invert tr;
    !res
      
let compose subst1 subst2 = 
  let res = ref [] in
  let remaining = ref subst2 in
  let compose_with (x, y) =
    let im = ref [] in
    let add_im y = 
      let z = 
	try
	  let z = List.assoc y subst2 in
	    remaining := List.remove_assoc y !remaining;
	    z
	with Not_found -> y::[]
      in
	im := z@(!im)
    in
      List.iter add_im y;
      res := (x, !im)::!res
  in
    List.iter compose_with subst1;
    !res@(!remaining)

let assoc x y s =
  try
    let prev_y = List.assoc x s in
      match prev_y with
	  prev_y::[] -> 
	    (* do not associate the same variable to different values twice *) 
	    if prev_y <> y then raise Exceptions.Unknown;
	    print_endline "B";
	    s
	| _ -> print_endline "C"; invalid_arg "Subst.assoc: not implemented yet"
  with Not_found -> (x, y::[])::s

let apply tr x = 
  try List.assoc x tr 
  with Not_found -> x::[]

let build_param_map env n =
  let res = ref [] in
    for i = 0 to env do
      let j = if i <= n then Memloc.of_local (n-i) else Memloc.gen () in
	res := (Memloc.of_local (env-i), j::[])::!res
    done;
    !res

let domain tr = 
  let (memlocs, _) = List.split tr in
    memlocs


(* useful for debug
let assoc x y s =
  print_endline "Subst.assoc";
  print_endline (to_string s);
  print_endline (Memloc.to_string x);
  print_endline (Memloc.to_string y);
  let s = assoc x y s in
    print_endline (to_string s);
    s
*)

let test1 () =
  print_string "npknull/Subst.test1... ";
  let a = Memloc.of_global "a" in
  let b = Memloc.of_global "b" in
  let s = identity in
  let s = assoc a a s in
    try
      let _ = assoc a b s in
	invalid_arg "Failed"
    with Exceptions.Unknown -> print_endline "OK"

let test2 () =
  print_string "npknull/Subst.test2... ";
  let a = Memloc.of_global "a" in
  let b = Memloc.of_global "b" in
  let c = Memloc.of_global "c" in
  let s = identity in
  let s = assoc c a s in
  let s = assoc a b s in
  let s = invert s in
    if List.length (apply s a) <> 1 then invalid_arg "Failed";
    print_endline "OK"

let test () =
  test1 ();
  test2 ()
