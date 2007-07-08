(* TODO: to remove this n, change interpretation of locals! *)

open Newspeak
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

(* TODO: make an interface for this file !! *)
exception Unknown

let eval_lval n lv =
  match lv with
      Local x -> n - x - 1
    | _ -> raise Unknown

let universe n = (n, [])

let forget (n, _) = (n, [])

let assign (n, _) lv e = 
  try
    let x = eval_lval n lv in
    let s = (x, e)::[] in
      (n, s)
  with Unknown -> (n, [])

let push (n, s) = (n + 1, s)

let exp_of_local (n, s) (lv, t) = 
  try 
    let x = eval_lval n lv in
      List.assoc x s
  with Not_found | Unknown -> Lval (lv, t)

let to_string (n, s) =
  let string_of_assoc (x, e) =
    let x = string_of_int x in
    let v = Newspeak.string_of_exp e in
      x^" -> "^v
  in
  let rec to_string s =
    match s with
	hd::[] -> string_of_assoc hd
      | hd::tl -> (string_of_assoc hd)^", "^(to_string tl)
      | [] -> ""
  in
    to_string s
