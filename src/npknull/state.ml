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

(* TODO: remove totally all forget_memlocs, forgets....
   if there is a top within a function analysis, the whole analysis may be
   unsound!!!
*)

(* TODO: think about cleaning up between state and store!!! 
   (in particular remove translate_exp and lval_to_abaddr from this module) *)
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

let deref_abaddr a n =
  match a with
      Dom.Ptr (x, Some (o, sz)) -> ((x, o), sz-n)
    | Dom.Ptr (x, None) -> ((x, 0), max_int)
    | _ -> raise Exceptions.Emptyset

(* TODO:should remove lval_to_abaddr and translate_exp, since they are done in
   store.ml, think about simplifying types of abaddr and dom too!! *)
let rec lval_to_abaddr env s lv =
  match lv with
      Global x -> Abaddr.singleton (Memloc.of_global x)
    | Local x -> Abaddr.singleton (Memloc.of_local (env - x))
    | Shift (lv, Const CInt n) -> 
	let a = lval_to_abaddr env s lv in
	  Abaddr.shift (Nat.to_int n) a
    | Shift (lv, _) -> Abaddr.forget_offset (lval_to_abaddr env s lv)
    | Deref (e, n) -> 
	let a = translate_exp env s e in
	let buf = deref_abaddr a n in
	  Abaddr.of_buffer buf

and translate_exp env s e =
  match e with
      Const _ -> Dom.Cst
    | AddrOf (lv, n) -> 
	let a = lval_to_abaddr env s lv in
	  Abaddr.to_exp n a
    | AddrOfFun (f, _) -> Dom.AddrOfFun f
(* TODO: is it sound not to consider type here?? *)
    | Lval (lv, _) -> 
	let a = lval_to_abaddr env s lv in
	let a = Abaddr.to_addr a in 
	let a = 
	  (* TODO: read_addr should directly return a Dom.exp!!! *)
	  try Dom.Ptr (Store.read_addr s a) 
	  with Exceptions.Emptyset -> Dom.Cst
	in
	  a
    | UnOp ((PtrToInt _| IntToPtr _), e) -> translate_exp env s e
    | BinOp (PlusPI, e, _) -> 
	let a = translate_exp env s e in
	let p = 
	  match a with
	      Dom.Ptr p -> p
	    | _ -> raise Exceptions.Unknown
	in
	  Dom.Ptr p
    | _ -> raise Exceptions.Unknown

let exp_to_ptr env s e =
  let rec exp_to_ptr e =
    match e with
	Lval (lv, Ptr) -> lval_to_abaddr env s lv
      | BinOp (PlusPI, e, _) -> exp_to_ptr e
      | _ -> raise Exceptions.Unknown
  in
    exp_to_ptr e

let exp_is_valid env s e =
  match s with
      None -> true
    | Some s -> 
	try
	  let a = exp_to_ptr env s e in
	  let a = Abaddr.to_addr a in
	    Store.addr_is_valid s a
	with
	    Exceptions.Emptyset -> true
	  | Exceptions.Unknown -> false

let exp_to_fun env s e = 
  match s with
      None -> []
    | Some s -> 
	let rec exp_to_fun e =
	  (* TODO: is it sound not to consider type here?? *)
	  match e with
	      Lval (lv, _) -> begin
		try Store.read_fun env s lv
		with 
		    Exceptions.Emptyset -> []
		  | Exceptions.Unknown -> 
		      raise (Exceptions.NotImplemented 
			       "State.exp_to_fun: case Lval")
	      end
		(* TODO: not good these constants!!! *)
	    | UnOp (Cast (_, t), e) when Newspeak.size_of_scalar 32 t = 32 ->
		exp_to_fun e
	    | _ -> 
		let msg = "State.exp_to_fun: "^(Newspeak.string_of_exp e) in
		  raise (Exceptions.NotImplemented msg)
	in
	  exp_to_fun e

(* TODO: there is most probably a bug if there is a value 
   at address (v, 0) and a value is copied at address (v, 1)
   the value at (v, 0) should be removed!!!
*)
(* TODO: most probably a bug because the type of the assignment 
   is not considered *)
let assign args env s =
  match s with
      None -> None
    | Some s -> 
	try Some (Store.assign args env s)
	with Exceptions.Emptyset -> emptyset
(*
	try
	  let a = lval_to_abaddr env s lv in
	    try
	      let a = Abaddr.to_addr a in
	      let e = translate_exp env s e in
		(* TODO: this constant not nice!! *)
	      let sz = Newspeak.size_of_scalar 32 t in
		print_endline "here";
	      let s = Store.assign (a, e, sz) s in
		Some s
	    with Exceptions.Unknown -> 
	      try
		let (a, n) = Abaddr.to_buffer a in
(* TODO: not good this constant, remove!! *)
		let sz = Newspeak.size_of_scalar 32 t in
		let n = if n > max_int - sz then max_int else n + sz in
		  Some (Store.forget_buffer (a, n) s)
	      with Exceptions.Unknown ->
		let m = Abaddr.to_memloc a in
		  Some (Store.forget_memloc m s)
	with
	    Exceptions.Emptyset -> emptyset
	  | Exceptions.Unknown -> forget () (* TODO: should remove this case altogether, source of unsoundness!! *)
*)

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
(* TODO: this primitive is strange, to remove?? *)
let set_pointsto m1 m2 s = 
  match s with
      Some s -> Some (Store.set_pointsto m1 m2 s)
    | None -> None

let copy args env s =
  match s with
      None -> None
    | Some s -> Some (Store.copy args env s)

let guard e env s = 
  match s with
      None -> None
    | Some s -> 
	try
	  let s =
	    match e with
		UnOp (Not, BinOp (Eq Ptr, Lval (lv, Ptr), Const Nil))
	      | UnOp (Not, BinOp (Eq Ptr, Const Nil, Lval (lv, Ptr))) ->
		  let a = lval_to_abaddr env s lv in
		  let a = Abaddr.to_addr a in
		    Store.guard a s
	      | _ -> s
	  in
	    Some s
	with 
	    Exceptions.Emptyset -> None
	  | Exceptions.Unknown -> Some s

(* TODO: write an unsound example with write into a universe pointer!!! *)
let build_transport s memlocs pre = 
  match (s, pre) with
      (Some s, Some pre) -> Store.build_transport s memlocs pre
    | _ -> Subst.identity

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
(*
let build_transport s memlocs pre =
  print_endline "State.build_transport";
  List.iter (fun x -> print_string (Memloc.to_string x^" ")) memlocs;
  print_newline ();
  print_endline (to_string s);
  print_endline (to_string pre);
  let tr = build_transport s memlocs pre in
  print_endline (Subst.to_string tr);
  print_endline "State.build_transport ends";
    tr
*)
