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

type t = 
    Top
  | Val of Int32.t

let universe = Top

let singleton i = Val i

let of_bounds (l, u) =  if Int32.compare l u = 0 then Val l else Top

let contains x y =
  match (x, y) with
      (Top, _) -> true
    | (Val i, Val j) when i = j -> true
    | _ -> false

let join x y =
  match (x, y) with
      (Val x, Val y) when x = y -> Val x
    | _ -> Top

let neg x =
  match x with
      Val i when i = Int32.zero -> Val Int32.one
    | Val _ -> Val Int32.zero
    | Top -> Top

let gt x y =
  match (x, y) with
      (Val x, Val y) -> 
	if Int32.compare x y > 0 then Val Int32.one
	else Val Int32.zero
    | _ -> Top

let add x y =
  match (x, y) with
      (Val x, Val y) -> 
	let z = Int32.add x y in
         if (Int64.compare (Int64.add (Int64.of_int32 x) (Int64.of_int32 y)) (Int64.of_int32 z) != 0) then Top
	  else Val z
    | _ -> Top

let is_safe_add x y =
  match (x, y) with
      (Val x, Val y) ->
	let z = Int32.add x y in
         (Int64.compare (Int64.add (Int64.of_int32 x) (Int64.of_int32 y)) (Int64.of_int32 z) == 0) 
    | _ -> false
	
let implies _ = false

let guard op c x =
  match (op, c, x) with
      (LTE, Val i, Val x) when Int32.compare i x > 0 -> raise Emptyset
    | _ -> x

let to_string v = 
  match v with
      Val i -> Int32.to_string i
    | Top -> "?"
