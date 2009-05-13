(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2009  Charles Hymans
 
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

module Map = 
struct
  include Map.Make(Var)

  let for_all2 p m1 m2 =
    let check k v1 =
      let v2 = find k m2 in
	if not (p v1 v2) then raise Exit
    in
      try 
	iter check m1;
	true
      with Exit -> false

  let map2 f m1 m2 =
    let apply k v1 =
      let v2 = find k m2 in
	f v1 v2
    in
      mapi apply m1
end

module Set = Var.Set

type t = Set.t Map.t

let create () = Map.empty

let add_var x s = Map.add x Set.empty s

let remove_var = Map.remove

let is_subset = Map.for_all2 Set.subset

let join = Map.map2 Set.union

let pointsto s x =
  try Map.find x s
  with Not_found -> invalid_arg "PtrDom.pointsto: unreachable code"

let deref s v = 
  let res = ref Set.empty in
  let deref x = res := Set.union (pointsto s x) !res in
    Set.iter deref v;
    !res

let assign x y s = 
  let res = ref s in
  let assign x =
    let v = pointsto s x in
      res := Map.add x (Set.union y v) !res
  in
    Set.iter assign x;
    !res

let to_string s =
  let res = ref "" in
  let to_string x y =
    let x = Var.to_string x in
    let y = Set.to_string y in
      res := !res^x^" -> "^y^"\n"
  in
    Map.iter to_string s;
    !res
