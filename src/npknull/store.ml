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

open Newspeak

exception Unknown
exception Emptyset

module Map = Map.Make(String)
module Set = Set.Make(String)

type offset = int

(* set of location times offset, None is for unknown *)
type info = 
    NotNull                 (* this value is not really nice, 
			       think about removing it *)
  | PointsTo of (Set.t * offset option)

(* map from locations: 
   G.global name, 
   L.local number, or 
   H.heap anonymous location 
   to association list from offset to info
*)
type store = (offset * info) list Map.t

(* None is for emptyset *)
(* TODO: try to get rid of this emptyset, use an exception instead!! *)
type t = (store * store) option

let universe () = Some (Map.empty, Map.empty)

let emptyset = None

let is_empty s = s = None

let apply s rel = 
  match (s, rel) with
      (None, _) | (_, None) -> None
    | (Some (i, _), Some (_, s)) -> Some (i, s)

let store_join s1 s2 =
  let s = ref s2 in
  let add_info x d1 =
    try
      let d2 = Map.find x s2 in
	if d1 <> d2 
	then invalid_arg "Store.join: not implemented yet <>"
	  with Not_found -> s := Map.add x d1 !s
  in
    Map.iter add_info s1;
    !s
  

(* TODO: this should be greatly improved!!! 
   right now O(n)*log(n)
   Too costly!!
*)
let join s1 s2 = 
  match (s1, s2) with
(* TODO: strange, assumes init are both the same here!! *)
      (Some (i, s1), Some (_, s2)) -> Some (i, store_join s1 s2)
    | (None, s) | (s, None) -> s

let store_contains s1 s2 =
  let check_info x d1 =
    try
      let d2 = Map.find x s2 in
	if d1 <> d2 
	then invalid_arg "Store.contains: not implemented yet"
    with Not_found -> raise Exit
  in
    try
      Map.iter check_info s1;
      true
    with Exit -> false

(* TODO: this should be greatly improved!!
   copy on write, patricia trie, partial recursion...
*)
let contains s1 s2 = 
  match (s1, s2) with
      (_, None) -> true
    | (None, _) -> false
(* TODO: strange but assumes they have both the same init *)
    | (Some (_, s1), Some (_, s2)) -> store_contains s1 s2

let prepare_call s rel = 
  match (rel, s) with
      (None, None) -> 
	invalid_arg "Store.prepare_call: not implemented yet NoneNone"
    | (None, _) -> (true, s)
    | (Some (i, _), Some (_, s)) -> 
	let (is_new, s) = 
	  if store_contains i s then (false, s) 
	    (* TODO: this is strange because here
	       s is not really needed, behavior not
	       assembled.
	       think about primitives again
	    *)
	  else (true, store_join i s)
	in
	  (is_new, Some (s, s))
    | _ -> invalid_arg "Store.prepare_call: not implemented yet"

let find_info (m, o) s = List.assoc o (Map.find m s)

(* TODO: this is really awkward that this is needed!! *)
let addr_is_valid s (m, o) = 
  match s with
      Some (_, s) -> begin
	try 
	  let s = Map.find m s in
	    List.mem_assoc o s
	with Not_found -> false
      end
    | None -> true

let abaddr_to_addr (m, o) = 
  match o with
      Some o -> (m, o)
    | None -> raise Unknown

let rec lval_to_abaddr env s lv =
  match lv with
      Global x -> ("G."^x, Some 0)
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
	let i = 
	  try find_info a s
	  with Not_found -> raise Unknown
	in
	let (m, o) = 
	  match i with
	      PointsTo d -> d
	    | _ -> raise Unknown
	in
	let m =
	  match Set.elements m with
	      m::[] -> m
	    | _ -> raise Unknown
	in
	  (m, o)
    | _ -> raise Unknown

let eval_exp env s e =
  match e with
      AddrOf (lv, _) -> 
	let (m, _) = lval_to_abaddr env s lv in
	  m
    | _ -> raise Unknown

let abaddr_to_memloc (m, _) = m

let forget_memloc m s = Map.remove m s 

let store_set_pointsto (m1, o) m2 s =
  (* TODO: could be optimized/made more precise *)
  if Map.mem m1 s then Map.remove m1 s
  else Map.add m1 ((o, PointsTo (Set.singleton m2, Some 0))::[]) s

let store_guard (m, o) s =
  if Map.mem m s then s
  else Map.add m ((o, NotNull)::[]) s

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
		      let s = store_set_pointsto a p s in
			Some (i, s)
		  | _ -> raise Unknown
	    with Unknown -> 
	      let m = abaddr_to_memloc a in
		Some (i, forget_memloc m s)
	with Unknown -> universe ()

let string_of_info i =
  match i with
      NotNull -> "<> 0"
    | PointsTo (a, o) -> 
	let a = Set.elements a in
	let a =
	  match a with
	      a::[] -> a
	    | _ -> "{"^ListUtils.to_string (fun x -> x) "," a^"}"
	in
	let o =
	  match o with
	      None -> "?"
	    | Some x -> string_of_int x
	in
	  "("^a^", "^o^")"

let to_string s =
  match s with
      Some (_, m) -> 
	let res = ref "" in
	let to_string l data =
	  let to_string (offset, info) =
	    let info = string_of_info info in
	      res := !res^" ("^l^", "^string_of_int offset^") -> "^info
	  in
	    List.iter to_string data
	in
	  Map.iter to_string m;
	  !res
    | None -> "{}"

(* TODO: find a way to remove this option, remove emptyset!! *)
let remove_local v s =
  match s with
      Some (i, s) -> 
	let v = "L."^string_of_int v in
	  Some (i, Map.remove v s)
    | None -> None

(* TODO: find a way to remove emptyset/None!!! *)
let set_pointsto m1 m2 s = 
  match s with
      Some (i, s) -> Some (i, store_set_pointsto m1 m2 s)
    | None -> None

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
		  store_guard a s
	      | _ -> s
	  with Unknown -> s
	in
	  Some (i, s)

let lval_to_abaddr env s lv =
  match s with
      Some (_, s) -> lval_to_abaddr env s lv
    | None -> raise Emptyset

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
*)
