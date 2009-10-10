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

type t = (Memloc.t * Memloc.t) list

let build_param_map env n =
  let res = ref [] in
    for i = 0 to env do
      let j = if i <= n then Memloc.of_local (n-i) else Memloc.gen () in
	res := (Memloc.of_local (env-i), j)::!res
    done;
    !res

let invert subst = List.map (fun (x, y) -> (y, x)) subst

let compose subst1 subst2 = 
  let subst2 = ref subst2 in
  let compose_with (x, y) =
    try
      let z = List.assoc y !subst2 in
	subst2 := List.remove_assoc y !subst2;
	(x, z)
    with Not_found -> (x, y)
  in
  let subst1 = List.map compose_with subst1 in
    subst1@(!subst2)

let to_string tr =
  let string_of_assoc (x, y) = Memloc.to_string x^" -> "^Memloc.to_string y in
    "["^ListUtils.to_string string_of_assoc ", " tr^"]"

let identity = []

let assoc x y s =
  if x = y then s
  else begin
    try
      let prev_y = List.assoc x s in
	(* do not associate the same variable to different values twice *) 
	if prev_y <> y then raise Exceptions.Unknown;
	s
    with Not_found -> (x, y)::s
  end

let apply tr x = try List.assoc x tr with Not_found -> x

