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

open UnrelState

type t = (Int32.t * Int32.t)

let universe = 
  let (l, u) = Newspeak.domain_of_typ (Newspeak.Signed, 32) in
    (Int32.of_string (Newspeak.Nat.to_string l), 
     Int32.of_string (Newspeak.Nat.to_string u))

let singleton i = (i, i)

let of_bounds x = x

let contains (l1, u1) (l2, u2) = 
  Int32.compare l1 l2 <= 0 && Int32.compare u1 u2 >= 0

let join (l1, u1) (l2, u2) = 
  let l = if Int32.compare l1 l2 <= 0 then l1 else l2 in
  let u = if Int32.compare u1 u2 >= 0 then u1 else u2 in
    (l, u)

let implies _ = false

let neg _ = invalid_arg "Range.neg: not implemented yet"

let add (l1, u1) (l2, u2) = 
  let u = Int32.add u1 u2 in
    if (Int32.compare u u1 >= 0) && (Int32.compare u u2 >= 0) then begin
      (Int32.add l1 l2, u)
    end else universe
  

let is_safe_add (_, u1) (_, u2) = 
  let u = Int32.add u1 u2 in
    (Int32.compare u u1 >= 0) && (Int32.compare u u2 >= 0)

let is_safe_mul _ _ = false
  

let gt _ _ = (Int32.zero, Int32.one)

let normalize (l, u) =
  if Int32.compare l u > 0 then raise Emptyset;
  (l, u)

let guard op c x = 
  let y = 
    match (op, c, x) with
	(LTE, (v, _), (l, u)) -> 
	  let l = if Int32.compare v l >= 0 then v else l in
	    (l, u)
      | (GT, (_, v), (l, u)) -> 
	  let v = if v = Int32.min_int then raise Emptyset else Int32.pred v in
	  let u = if Int32.compare v u <= 0 then v else u in
	    (l, u)
      | _ -> x
  in
    normalize y

let to_string (l, u) = 
  if Int32.compare l u = 0 then Int32.to_string l
  else "["^Int32.to_string l^", "^Int32.to_string u^"]"
