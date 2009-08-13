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

(* TODO: if there are may cells get rid of their cases in this module *)
open Newspeak

exception Emptyset

module Map = Map.Make(String)
module Set = Set.Make(String)

type offset = int

(* None is for emptyset *)
(* TODO: try to get rid of this emptyset, use an exception instead!! *)
type t = (Store.t * Store.t) option

type transport = (Memloc.t * Memloc.t) list

let universe = Some (Store.universe, Store.universe)

let emptyset = None

let is_empty s = s = None

(* TODO: this should be greatly improved!!! 
   right now O(n)*log(n)
   Too costly!!
*)
let join s1 s2 = 
  match (s1, s2) with
(* TODO: strange, assumes init are both the same here!! *)
      (Some (i, s1), Some (_, s2)) -> Some (i, Store.join s1 s2)
    | (None, s) | (s, None) -> s

(* TODO: this should be greatly improved!!
   copy on write, patricia trie, partial recursion...
*)
let contains s1 s2 = 
  match (s1, s2) with
      (_, None) -> true
    | (None, _) -> false
(* TODO: strange but assumes they have both the same init *)
    | (Some (_, s1), Some (_, s2)) -> Store.contains s1 s2

(* TODO: this is really awkward that this is needed!! *)
let addr_is_valid s a = 
  match s with
      Some (_, s) -> Store.addr_is_valid s a
    | None -> true

let abaddr_to_addr (m, o) = 
  match o with
      Some o -> (m, o)
    | None -> raise Exceptions.Unknown

let rec lval_to_abaddr env s lv =
  match lv with
      Global x -> (Memloc.of_global x, Some 0)
    | Local x -> (Memloc.of_local (env - x), Some 0)
    | Shift (lv, Const CInt n) -> 
	let (m, o) = lval_to_abaddr env s lv in
	let o = 
	  match o with
	      None -> None
	    | Some o -> Some (o + (Nat.to_int n))
	in
	  (m, o)
    | Deref (Lval (lv, Ptr), _) -> 
	let a = lval_to_abaddr env s lv in
	let a = abaddr_to_addr a in
	  Store.read_addr s a
    | _ -> raise Exceptions.Unknown

let eval_exp env s e =
  match e with
      AddrOf (lv, _) -> 
	let (m, _) = lval_to_abaddr env s lv in
	  m
    | _ -> raise Exceptions.Unknown

let abaddr_to_memloc (m, _) = m

(* TODO: there is most probably a bug if there is a value 
   at address (v, 0) and a value is copied at address (v, 1)
   the value at (v, 0) should be removed!!!
*)
let assign (lv, e, t) env s =
  match s with
      None -> None
    | Some (i, s) -> 
	try
	  let a = lval_to_abaddr env s lv in
	    try
	      let a = abaddr_to_addr a in
		match t with
		    Ptr -> 
		      let p = eval_exp env s e in
		      let s = Store.write a p s in
			Some (i, s)
		  | _ -> raise Exceptions.Unknown
	    with Exceptions.Unknown -> 
	      let m = abaddr_to_memloc a in
		Some (i, Store.forget_memloc m s)
	with Exceptions.Unknown -> universe

let to_string s =
  match s with
      Some (_, m) -> Store.to_string m
    | None -> "{}"

(* TODO: find a way to remove this option, remove emptyset!! *)
let remove_local v s =
  match s with
      Some (i, s) -> 
	let v = Memloc.of_local v in
	  Some (i, Store.remove_memloc v s)
    | None -> None

(* TODO: find a way to remove emptyset/None!!! *)
let set_pointsto m1 m2 s = 
  match s with
      Some (i, s) -> Some (i, Store.write m1 m2 s)
    | None -> None

let forget_lval lv env s =
  match s with
      None -> None
    | Some (i, s) -> 
	try
	  let a = lval_to_abaddr env s lv in
	  let m = abaddr_to_memloc a in
	  let s = Store.forget_memloc m s in
	    Some (i, s)
	with Exceptions.Unknown -> universe

let guard e env s = 
  match s with
      None -> None
    | Some (i, s) -> 
	let s = 
	  try
	    match e with
		UnOp (Not, BinOp (Eq Ptr, Lval (lv, Ptr), Const Nil)) ->
		  let a = lval_to_abaddr env s lv in
		  let a = abaddr_to_addr a in
		    Store.guard a s
	      | _ -> s
	  with Exceptions.Unknown -> s
	in
	  Some (i, s)

let lval_to_abaddr env s lv =
  match s with
      Some (_, s) -> lval_to_abaddr env s lv
    | None -> raise Emptyset

let apply s _ rel = 
  match (s, rel) with
      (None, _) | (_, None) -> None
    | (Some (i, _), Some (_, s)) -> Some (i, s)  

let prepare_call (env, s) (env_f, rel) = 
  match s with
      None -> (None, [])
    | Some (_, s) ->
	let s = Store.shift (env-env_f) s in
	  match rel with
	      None -> (Some (Some (s, s)), [])
	    | Some (i, _) -> 
		let s = Store.unify_on i env s in
		let s =
		  if Store.contains i s then None else Some (Some (s, s))
		in
		  (s, [])

(* usefull for debug *)
(*
let assign set env s =
  print_endline "Store.assign";
  print_endline (to_string s);
  let s = assign set env s in
    print_endline (to_string s);
    print_endline "Store.assign ends";
    s

let prepare_call rel s =
  print_endline "Store.prepare_call";
  print_endline (to_string rel);
  print_endline (to_string s);
  let (is_new, s) = prepare_call rel s in
    print_endline (to_string s);
    print_endline "Store.prepare_call ends";
    (is_new, s)

let apply s rel = 
  print_endline "Store.apply";
  print_endline (to_string s);
  print_endline (to_string rel);
  let s = apply s rel in
    print_endline (to_string s);
    print_endline "Store.apply ends";
    s

let guard e env s =
  print_endline "Store.guard";
  print_endline (Newspeak.string_of_exp e);
  print_endline (to_string s);
  let s = guard e env s in
  print_endline (to_string s);
    print_endline "Store.guard ends";
    s

let contains s1 s2 =
  print_endline "State.contains";
  print_endline (to_string s1);
  print_endline (to_string s2);
  let b = contains s1 s2 in
    print_endline "State.contains ends";
    b
*)
