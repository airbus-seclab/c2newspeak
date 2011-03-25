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

module Map = Map.Make(String)

type t = VarSet.t Map.t

let identity () = Map.empty

let domain subst = 
  let result = ref VarSet.empty in
  let add_variable x _ = result := VarSet.add x !result in
    Map.iter add_variable subst;
    !result

let apply subst x = 
  try Map.find x subst
  with Not_found -> VarSet.singleton x

let apply_variable_start subst x =
  try (Map.find x subst, false)
  with Not_found -> (VarSet.singleton x, true)

let apply_set subst s =
  let result = ref VarSet.empty in
  let add_variable x = 
    let x = apply subst x in
      result := VarSet.union x !result
  in
    VarSet.iter add_variable s;
    !result

let associate_set x y subst =
  let v = 
    try Map.find x subst
    with Not_found -> VarSet.empty
  in
  let v = VarSet.union y v in
    Map.add x v subst

(* TODO: rename to add_assoc? *)
let associate x y subst = associate_set x (VarSet.singleton y) subst

(* TODO: remove this function *)
let is_identity _ = invalid_arg "Not implemented yet"

(* TODO: rename to invert *)
let inverse subst = 
  let result = ref (identity ()) in
  let invert_assoc x y =
    VarSet.iter (fun y -> result := associate y x !result) y
  in
    Map.iter invert_assoc subst;
    !result

(* TODO: remove this function ?? *)
let of_list subst = 
  let result = ref (identity ()) in
  let add_assoc (x, y) = result := associate x y !result in
    List.iter add_assoc subst;
    !result

let compose subst1 subst2 = 
  let result = ref (identity ()) in
  let rest_of_subst2 = ref subst2 in
  let compose_with_subst2 x y =
    rest_of_subst2 := Map.remove x !rest_of_subst2;
    let z = apply_set subst2 y in
      result := associate_set x z !result
  in
    Map.iter compose_with_subst2 subst1;
    Map.iter (fun x y -> result := associate_set x y !result) !rest_of_subst2;
    !result

let to_string subst = 
  let result = ref "" in
  let string_of_assoc x y =
    result := !result^x^"\\"^(VarSet.to_string y)
  in
    Map.iter string_of_assoc subst;
    !result

(*
(* TODO: optimization should use a map rather *)
type t = (string * VarSet.t) list

let identity () = []

let apply subst x =
  try List.assoc x subst
  with Not_found -> VarSet.singleton x

let apply_set subst s =
  let result = ref VarSet.empty in
  let add_variable x = 
    let x = apply subst x in
      result := VarSet.union x !result
  in
    VarSet.iter add_variable s;
    !result

(* TODO: think about it, what if x is already mapped? Should be ok. *)
let associate x y subst = (x, VarSet.singleton y)::subst

let is_identity subst = (subst = [])

let inverse subst = List.map (fun (x, y) -> (y, x)) subst

let of_list subst =
  let result = ref (identity ()) in
  let add_assoc (x, y) = result := associate x y !result in
    List.iter add_assoc subst;
    !result

let compose subst1 subst2 =
  let result = ref subst1 in
  let add_assoc (x, y) = result := associate x y !result in
    List.iter add_assoc subst2;
    !result

let to_string subst =
  ListUtils.to_string "," (fun (x, y) -> x^"\\"^y) subst
*)
