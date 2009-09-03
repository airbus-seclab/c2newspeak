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

type t = 
    Local of int
  | Global of string
  | Heap of int

let cnt = ref (-1)
  
let gen () =
  if !cnt = max_int then invalid_arg "Memloc.gen: no more locations";
  incr cnt;
  Heap !cnt

let of_global x = Global x

let of_local x = Local x

let compare = compare

let shift n m = 
  match m with
      Local x when x >= n -> Local (x-n)
    | Local _ -> gen () 
    | _ -> m

let to_string x =
  match x with
      Local x -> "L."^string_of_int x
    | Global x -> "G."^x
    | Heap x -> "H."^string_of_int x
