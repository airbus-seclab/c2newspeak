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

module Map = Map.Make(Var)
module Set = Var.Set

type t = Set.t Map.t

let create () = Map.empty

let add_var x s = Map.add x Set.empty s

let remove_var = Map.remove

let join _ _ = invalid_arg "PtrDom.join: not implemented yet"

let pointsto s x =
  try Map.find x s
  with Not_found -> invalid_arg "PtrDom.pointsto: unreachable code"

let deref s v = 
  let res = ref Set.empty in
  let deref x = res := Set.union (pointsto s x) !res in
    Set.iter deref v;
    !res

let assign x y s = 
  let v = deref s y in
  let res = ref s in
  let assign x =
    let v' = pointsto s x in
      res := Map.add x (Set.union v' v) !res
  in
    Set.iter assign x;
    !res
