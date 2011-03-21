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

module Data =
struct
(* true if there is no offset difference *)
  type t = VarSet.t * bool

  let empty = (VarSet.empty, true)

  let union (vars1, has_no_delta1) (vars2, has_no_delta2) = 
    (VarSet.union vars1 vars2, has_no_delta1 && has_no_delta2)

end

type t = Data.t Map.t

let identity () = Map.empty

let associate x y subst =
  let v = 
    try Map.find x subst
    with Not_found -> Data.empty
  in
  let v = Data.union v (VarSet.singleton y, false) in
    Map.add x v subst

let apply subst x =
  try 
    let (y, _) = Map.find x subst in
    let result = ref VarSet.empty in
    let add_address y = result := VarSet.add y !result in
      VarSet.iter add_address y;
      !result
  with Not_found -> VarSet.singleton x

let apply_variable_start subst x =
  try Map.find x subst
  with Not_found -> (VarSet.singleton x, true)

(* TODO: factor code with subst2 *)
let apply_set subst s =
  let result = ref VarSet.empty in
  let add_variable x = 
    let x = apply subst x in
      result := VarSet.union x !result
  in
    VarSet.iter add_variable s;
    !result

(* TODO: remove this function ?? *)
let of_list subst = 
  let result = ref (identity ()) in
  let add_assoc (x, y) = result := associate x y !result in
    List.iter add_assoc subst;
    !result

(* TODO: rename to invert *)
let inverse subst = 
  let result = ref (identity ()) in
  let invert_assoc x (y, _has_no_delta) =
    VarSet.iter (fun y -> result := associate y x !result) y
  in
    Map.iter invert_assoc subst;
    !result
