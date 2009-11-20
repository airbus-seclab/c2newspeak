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

(* offset, size of zone *)
type range_offset = int * int

let offset_zero = (0, 1)

let offset_universe = (0, max_int)

let offset_shift n (o, sz) = 
  let o = o + n in
  let o = if o < 0 then max_int else o in
    (o, sz)

let offset_to_int (o, n) = 
  if n <> 1 then raise Exceptions.Unknown;
  o

let offset_to_zone x = x

let offset_to_string (o, sz) = 
  "["^string_of_int o^": "^string_of_int (o+sz-1)^"]"

type t = Memloc.t * range_offset
    
let singleton x = (x, offset_zero)

let of_buffer ((x, o), n) = (x, (o, n))

let shift n (x, o) = (x, offset_shift n o)

let forget_offset (x, _) = (x, offset_universe)

let to_memloc (x, _) = x

let to_addr (x, o) = (x, offset_to_int o)

let to_buffer (x, o) = 
  let (o, n) = offset_to_zone o in
    ((x, o), n)
      
let to_exp n (x, (o, sz)) =
  let sz = sz + n - 1 in
  let sz = if sz < 0 then max_int else sz in
    Dom.Ptr (x, Some (o, sz))

let to_string (x, o) = "("^Memloc.to_string x^", "^offset_to_string o^")"
