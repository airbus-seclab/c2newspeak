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

(* TODO: maybe could perform the pointer analysis beforehand!*)

module Set = Var.Set

type t = {
  last_local: int;
  ptrs: PtrDom.t
}

let create () = { 
  last_local = 0; 
  ptrs = PtrDom.create ();
}

let add_global x s = 
  let v = Var.of_global x in
    { s with ptrs = PtrDom.add_var v s.ptrs }

let add_local s = 
  let last_local = s.last_local + 1 in
  let v = Var.of_local last_local in
    { ptrs = PtrDom.add_var v s.ptrs; last_local = last_local }

let remove_local s = 
  let v = Var.of_local s.last_local in
  let ptrs = PtrDom.remove_var v s.ptrs in
    { last_local = s.last_local - 1; ptrs = ptrs }

let is_subset s1 s2 = PtrDom.is_subset s1.ptrs s2.ptrs

let join s1 s2 = { s1 with ptrs = PtrDom.join s1.ptrs s2.ptrs }

let eval_exp s e =
  let rec eval_exp e =
    match e with
	Const -> Set.empty
      | Local x -> Set.singleton (Var.of_local (s.last_local - x))
      | Global x -> Set.singleton (Var.of_global x)
      | BinOp (e1, e2) -> 
	  let x1 = eval_exp e1 in
	  let x2 = eval_exp e2 in
	    Set.union x1 x2
      | Deref e -> 
	  let x = eval_exp e in
	    PtrDom.deref s.ptrs x
  in
    eval_exp e

let to_string s = PtrDom.to_string s.ptrs

let points_to s e v =
  let t = eval_exp s e in
  let v = eval_exp s v in
    Set.subset v t

let assign (lv, e) s =
  let x = eval_exp s lv in
  let y = eval_exp s e in
  let ptrs = PtrDom.assign x y s.ptrs in
    { s with ptrs = ptrs }

let fids_of_exp s e = 
  let x = Set.elements (eval_exp s e) in
    List.map Var.to_fid x
    
