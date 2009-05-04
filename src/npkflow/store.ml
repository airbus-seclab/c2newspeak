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

open Equations

module Set = Set.Make(String)

type t = {
  last_local: int;
  tainted: Set.t
}

let create () = { last_local = 0; tainted = Set.empty }

let add_global _ s = s

let add_local s = { s with last_local = s.last_local + 1 }

let remove_local s = 
  let tainted = Set.remove (string_of_int s.last_local) s.tainted in
    { last_local = s.last_local - 1; tainted = tainted }

let join s1 s2 = { s1 with tainted = Set.union s1.tainted s2.tainted }

let eval_lval s lv =
  match lv with
      Local x -> string_of_int (s.last_local - x)
    | Global x -> x

let eval_exp s e =  
  let rec eval e =
    match e with
	lv::tl -> 
	  let x = eval_lval s lv in
	    if (Set.mem x s.tainted) then true
	    else eval tl
      | [] -> false
  in
    eval e

let taint lv s = 
  let x = eval_lval s lv in
    { s with tainted = Set.add x s.tainted }

let is_tainted lv s =
  let x = eval_lval s lv in
    Set.mem x s.tainted

let assign (lv, e) s = 
  let x = eval_lval s lv in
  let t = eval_exp s e in
    if t then { s with tainted = Set.add x s.tainted }
    else s
