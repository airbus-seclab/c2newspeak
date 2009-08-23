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

module Map = Map.Make(String)
module Set = Set.Make(String)

(* None is for emptyset *)
(* TODO: try to get rid of this emptyset, use an exception instead?? *)
type t = Store.t option

let universe = Some (Store.universe)

let emptyset = None

let is_empty s = (s = None)

(* TODO: this should be greatly improved!!! 
   right now O(n)*log(n)
   Too costly!!
*)
let join s1 s2 = 
  match (s1, s2) with
      (Some s1, Some s2) -> Some (Store.join s1 s2)
    | (None, s) | (s, None) -> s

(* TODO: this should be greatly improved!!
   copy on write, patricia trie, partial recursion...
*)
let contains s1 s2 = 
  match (s1, s2) with
      (_, None) -> true
    | (None, _) -> false
    | (Some s1, Some s2) -> Store.contains s1 s2

(* TODO: this is really awkward that this is needed!! *)
let addr_is_valid s a = 
  match s with
      Some s -> Store.addr_is_valid s a
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
	(* TODO: could there be a bug here throwing away the offset *)
	let (m, o) = lval_to_abaddr env s lv in begin
	    match o with
		Some 0 -> m
	      | _ -> raise Exceptions.Unknown
	  end
    | Lval (lv, Ptr) -> 
(* TODO: code looks similar to Deref *)
	let a = lval_to_abaddr env s lv in
	let a = abaddr_to_addr a in
	let (m, o) = Store.read_addr s a in begin
	  match o with
	      Some 0 -> m
	    | _ -> raise Exceptions.Unknown
	  end
    | _ -> raise Exceptions.Unknown

let abaddr_to_memloc (m, _) = m

(* TODO: there is most probably a bug if there is a value 
   at address (v, 0) and a value is copied at address (v, 1)
   the value at (v, 0) should be removed!!!
*)
let assign (lv, e, t) env s =
  match s with
      None -> None
    | Some s -> 
	try
	  let a = lval_to_abaddr env s lv in
	    try
	      let a = abaddr_to_addr a in
		match t with
		    Ptr -> 
		      let p = eval_exp env s e in
		      let s = Store.write a p s in
			Some s
		  | _ -> raise Exceptions.Unknown
	    with Exceptions.Unknown -> 
	      let m = abaddr_to_memloc a in
		Some (Store.forget_memloc m s)
	with Exceptions.Unknown -> universe

let to_string s =
  match s with
      Some m -> Store.to_string m
    | None -> "{}"

(* TODO: find a way to remove this option, remove emptyset!! *)
let remove_local v s =
  match s with
      Some s -> 
	let v = Memloc.of_local v in
	  Some (Store.remove_memloc v s)
    | None -> None

(* TODO: find a way to remove emptyset/None!!! *)
let set_pointsto m1 m2 s = 
  match s with
      Some s -> Some (Store.write m1 m2 s)
    | None -> None

let forget_lval lv env s =
  match s with
      None -> None
    | Some s -> 
	try
	  let a = lval_to_abaddr env s lv in
	  let m = abaddr_to_memloc a in
	  let s = Store.forget_memloc m s in
	    Some s
	with Exceptions.Unknown -> universe

let guard e env s = 
  match s with
      None -> None
    | Some s -> 
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
	  Some s

let lval_to_abaddr env s lv =
  match s with
      Some s -> lval_to_abaddr env s lv
    | None -> raise Exceptions.Unknown

(* TODO: write an unsound example with write into a universe pointer!!! *)
let build_transport s memlocs pre = 
  match (s, pre) with
      (Some s, Some pre) -> Store.build_transport s memlocs pre
    | _ -> []

let split memlocs s =
  match s with
      Some s -> 
	let (unreach, reach) = Store.split memlocs s in
	  (Some unreach, Some reach)
(* TODO: not nice to have the possibility of emptyset!!! *)
    | None -> (None, None)

type subst = (Memloc.t * Memloc.t) list

let build_param_map env n =
  let rec build n =
    if n < 0 then [] 
    else (Memloc.of_local (env-n), Memloc.of_local n)::(build (n-1))
  in
    build n

let transport tr s =
  match s with
      Some s -> Some (Store.transport tr s)
    | None -> None

let invert subst = List.map (fun (x, y) -> (y, x)) subst

let compose subst1 subst2 = subst1@subst2

let glue s1 s2 =
  match (s1, s2) with
      (Some s1, Some s2) -> Some (Store.glue s1 s2)
    | _ -> None

(* TODO:
let apply s tr rel = 
  match (s, rel) with
      (None, _) | (_, None) -> None
    | (Some (i, _), Some (_, s)) -> 
	let tr = invert tr in
	let s = Store.subst tr s in
	  Some (i, s)

let prepare_call (env, s) (env_f, rel) = 
  match s with
      None -> (None, [])
    | Some (_, s) ->
	let (s, tr) = Store.shift env s env_f in
	  match rel with
	      None -> (Some (Some (s, s)), [])
	    | Some (i, _) -> 
		let s = Store.unify_on i env s in
		let s =
		  if Store.contains i s then None else Some (Some (s, s))
		in
		  (s, tr)

type transport = (Memloc.t * Memloc.t) list

let invert tr = List.map (fun (x, y) -> (y, x)) tr

let string_of_transport tr =
  let string_of_assoc (x, y) =
    Memloc.to_string x^"/"^Memloc.to_string y
  in
    "["^ListUtils.to_string string_of_assoc ", " tr^"]"
*)

(* usefull for debug *)
(*
let assign set env s =
  print_endline "Store.assign";
  let (lv, e, _) = set in
    print_endline (Newspeak.string_of_lval lv^" = "^Newspeak.string_of_exp e);
    print_endline (to_string s);
    let s = assign set env s in
      print_endline (to_string s);
      print_endline "Store.assign ends";
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
(*
let prepare_call (env, s) (env_f, rel) =
  print_endline "State.prepare_call";
  print_endline (string_of_int env);
  print_endline (to_string s);
  print_endline (string_of_int env_f);
  print_endline (to_string rel);
  let (s, tr) = prepare_call (env, s) (env_f, rel) in begin
      match s with
	  Some s -> print_endline (to_string s)
	| None -> print_endline "not reanalyzing"
    end;
    print_endline (string_of_transport tr);
    print_endline "State.prepare_call ends";
    (s, tr)
*)
(*
let apply s tr rel = 
  print_endline "State.apply";
  print_endline (to_string s);
  print_endline (to_string rel);
  let s = apply s tr rel in
    print_endline (to_string s);
    print_endline "State.apply ends";
    s
*)
