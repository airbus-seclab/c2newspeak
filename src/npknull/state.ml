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

(* TODO: remove totally all forget_memlocs, universes....
   if there is a top within a function analysis, the whole analysis may be
   unsound!!!
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

let deref_abaddr a =
  match a with
      Dom.Abaddr a -> a
    | _ -> raise Exceptions.Emptyset

let rec lval_to_abaddr env s lv =
  match lv with
      Global x -> (Memloc.of_global x, Some 0)
    | Local x -> (Memloc.of_local (env - x), Some 0)
    | Shift (lv, i) -> 
	let (m, o) = lval_to_abaddr env s lv in
	let o = 
	  match (o, i) with
	      (Some o, Const CInt n) -> Some (o + (Nat.to_int n))
	    | _ -> None
	in
	  (m, o)
    | Deref (e, _) -> 
	let a = translate_exp env s e in
	  deref_abaddr a

and translate_exp env s e =
  match e with
      AddrOf (lv, _) -> Dom.Abaddr (lval_to_abaddr env s lv)
    | AddrOfFun (f, _) -> Dom.AddrOfFun f
(* TODO: is it sound not to consider type here?? *)
    | Lval (lv, _) -> 
(* TODO: code looks similar to Deref *)
	let a = lval_to_abaddr env s lv in
	let a = abaddr_to_addr a in 
	let a = 
	  match Store.read_addr s a with
	      Some a -> Dom.Abaddr a
	    | None -> Dom.Cst
	in
	  a
    | UnOp ((PtrToInt _| IntToPtr _), e) -> translate_exp env s e
    | BinOp (PlusPI, e, _) -> 
	let a = translate_exp env s e in
	let (v, _) = deref_abaddr a in
	  Dom.Abaddr (v, None)
    | _ -> raise Exceptions.Unknown

let exp_to_fun env s e = 
(* TODO: is it sound not to consider type here?? *)
  match e with
      Lval (lv, _) -> begin
	try
	  let a = lval_to_abaddr env s lv in
	    let a = abaddr_to_addr a in 
	      Store.read_fun s a
	with Exceptions.Emptyset -> []
	end
    | _ -> raise Exceptions.Unknown

let abaddr_to_memloc (m, _) = m

(* TODO: there is most probably a bug if there is a value 
   at address (v, 0) and a value is copied at address (v, 1)
   the value at (v, 0) should be removed!!!
*)
(* TODO: most probably a bug because the type of the assignment 
   is not considered *)
let assign (lv, e, _) env s =
  match s with
      None -> None
    | Some s -> 
	try
	  let a = lval_to_abaddr env s lv in
	    try
	      let a = abaddr_to_addr a in
	      let e = translate_exp env s e in
	      let s = Store.assign a e s in
		Some s
	    with Exceptions.Unknown -> 
	      let m = abaddr_to_memloc a in
		Some (Store.forget_memloc m s)
	with
	    Exceptions.Emptyset -> emptyset
	  | Exceptions.Unknown -> universe (* TODO: should remove this case altogether, source of unsoundness!! *)

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
      Some s -> Some (Store.assign m1 (Dom.Abaddr (m2, Some 0)) s)
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

let transport tr s =
  match s with
      Some s -> Some (Store.transport tr s)
    | None -> None

let glue s1 s2 =
  match (s1, s2) with
      (Some s1, Some s2) -> Some (Store.glue s1 s2)
    | _ -> None

let exp_to_fun env s e =
  match s with
      Some s -> exp_to_fun env s e
    | None -> []

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

let join s1 s2 =
  print_endline "State.join";
  print_endline (to_string s1);
  print_endline (to_string s2);
  let s = join s1 s2 in
    print_endline (to_string s);
    print_endline "State.join ends";
    s

let glue s1 s2 =
  print_endline "State.glue";
  print_endline (to_string s1);
  print_endline (to_string s2);
  let s = glue s1 s2 in
    print_endline (to_string s);
    print_endline "State.glue ends";
    s

let compose tr1 tr2 =
  print_endline "State.compose";
  print_endline (string_of_transport tr1);
  print_endline (string_of_transport tr2);
  let tr = compose tr1 tr2 in
  print_endline (string_of_transport tr);
    print_endline "State.ends";
    tr

let split memlocs s =
  print_endline "State.split";
  print_endline (ListUtils.to_string Memloc.to_string ", " memlocs);
  print_endline (to_string s);
  let (unreach, reach, tr) = split memlocs s in
    print_endline (to_string unreach);
    print_endline (to_string reach);
    print_endline (string_of_transport tr);
    print_endline "State.split ends";
    (unreach, reach, tr)
*)
(*
let build_param_map env n =
  print_endline "State.build_param_map";
  print_endline (string_of_int env);
  print_endline (string_of_int n);
  let tr = build_param_map env n in
    print_endline (string_of_transport tr);
    tr
*)
