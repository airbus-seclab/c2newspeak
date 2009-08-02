(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans, Olivier Levillain
  
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

open Ptrspeak

(* TODO: have the possibility to keep the function call in Newspeak
   and not have a stack of local variables, but rather designate all variables 
   by their name 
   THINK ABOUT IT!!! 
*)

(* put in file analysis.ml function run *)      
let join p1 p2 = ListUtils.merge compare p1 p2
let singleton x = x::[]
let emptyset = []
let rec is_subset x y =
  match (x, y) with
      (p1::_, p2::_) when p1 < p2 -> false
    | (p1::_, p2::y) when p1 > p2 -> is_subset x y
    | (_::x, _::y) -> is_subset x y
    | _ -> x = []
let to_string x = 
  let str = ListUtils.to_string (fun x -> x) ", " x in
    "{ "^str^" }"

let run (fundecs, prog) = 
  let g = Hashtbl.create 100 in
  let changed = ref false in
  let deref p = 
    try Hashtbl.find g p 
    with Not_found -> []
  in
  (* TODO: could be slightly optimized by using a update function *)
  let add_pointsto x y = 
    let p = deref x in
      if not (is_subset y p) then begin
	let y = join p y in
	  Hashtbl.replace g x y;
	  changed := true
      end
  in
    
  let rec eval_exp e =
    match e with
	Const -> emptyset
      | Var x -> singleton x
      | Deref e ->
	  let p = eval_exp e in
	  let p = List.map deref p in
	  let res = ref [] in
	  let add x = res := join !res x in
	    List.iter add p;
	    !res
  in

  let transmit_param formal actual =
    add_pointsto formal (deref actual);
    add_pointsto actual (deref formal)
  in

  let eval_call f actuals =
    let formals = Hashtbl.find fundecs f in
      List.iter2 transmit_param formals actuals
  in

  let process_stmt x =
    match x with
	Set (e1, e2) -> 
	  let a = eval_exp e1 in
	  let p = eval_exp e2 in
	    List.iter (fun x -> add_pointsto x p) a
      | Call (f, params) -> 
	  let f = eval_exp f in
	    List.iter (fun x -> eval_call x params) f
  in

    changed := true;
    while !changed do
      changed := false;
      List.iter process_stmt prog
    done;
    g
