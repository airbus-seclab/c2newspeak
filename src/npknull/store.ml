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

(* TODO: anything that goes to top is unsound, because to deref something in top
   stops the analysis:

   blabla
   top
   *ptr = 1; // even if it was correct this stops execution
   *ptr2 = 2;
*)
(* TODO: this domain is too complex, the translations should be done on top
   of each domain into some independent adapters *)
open Newspeak

module P1 = FieldInsensitivePtrBuf
module P2 = NonNullPtr
module P3 = FPtrStore

type t = (P1.t * P2.t * P3.t)

let universe = (P1.universe, P2.universe, P3.universe)

let forget () = (P1.forget (), P2.universe, P3.forget ())

let join (a1, a2, a3) (b1, b2, b3) = 
  (P1.join a1 b1, P2.join a2 b2, P3.join a3 b3)

let contains (a1, a2, a3) (b1, b2, b3) = 
  P1.contains a1 b1 && P2.contains a2 b2 && P3.contains a3 b3

(* TOOD: this function should not exist!! *)
let get_one_abptr x =
  match x with
      x::[] -> x
    | _ -> raise Exceptions.Unknown

(* TODO: shouldn't this all be done in P1?? *)
(* TODO: should remove lval_to_memloc?? *)
let lval_to_memloc env s lv =
  let rec lval_to_memloc lv =
    match lv with
	Global x -> Memloc.of_global x
      | Local x -> Memloc.of_local (env - x)
      | Shift (lv, _) -> lval_to_memloc lv
      | Deref (e, _) -> exp_to_memloc e
  
  and exp_to_memloc e =
    match e with
	Lval (lv, _) ->
	  let m = lval_to_memloc lv in
	  let (m, _) = get_one_abptr (P1.read s m) in
	    m
      | BinOp (PlusPI, e, _) -> exp_to_memloc e
      | _ -> raise Exceptions.Unknown
  in
    lval_to_memloc lv

(* TODO: should never raise Exceptions.Unknown!!! (may be unsound) *)
let lval_to_memloc_list env s lv =
  let rec translate_lval lv =
    match lv with
	Global x -> (Memloc.of_global x)::[]
      | Local x -> (Memloc.of_local (env - x))::[]
      | Shift (lv, _) -> translate_lval lv
      | Deref (e, _) -> translate_exp e
  
  and translate_exp e =
    match e with
	Lval (lv, _) ->
	  let m = translate_lval lv in
	  let res = ref [] in
	  let read_loc m =
	    let m = P1.read s m  in
	    let m = List.map (fun (x, _) -> x) m in
	      res := !res@m
	  in
	    List.iter read_loc m;
	    !res
      | UnOp (IntToPtr _, e) 
      | BinOp (PlusPI, e, _) -> translate_exp e
      | _ -> 
	  invalid_arg ("Store.lval_to_memloc_list: case not handled yet: "
		       ^ Newspeak.string_of_exp e)
  in
    translate_lval lv

let lval_to_addr env s lv =
  let rec lval_to_addr lv =
    match lv with
	Global _ | Local _ -> (lval_to_memloc env s lv, 0)
      | Shift (lv, Const CInt n) -> 
	  let (m, o) = lval_to_addr lv in
	    (* TODO: maybe a bug with integer overflow here!! *)
	    (m, o + (Nat.to_int n))
      | _ ->
	  (*print_endline (Newspeak.string_of_lval lv);*)
	  raise Exceptions.Unknown
  in
    lval_to_addr lv

let exp_to_ptr env s e =
  let rec exp_to_ptr e =
    match e with
	Const _ | AddrOfFun _ | BinOp (Eq _, _, _) -> None
      | Lval (lv, _) ->
	  let m = lval_to_memloc env s lv in
	  let p =
	    try Some (get_one_abptr (P1.read s m))
	    with Exceptions.Emptyset -> None
	  in
	    p
      | AddrOf (lv, n) -> 
	  let (m, o) = lval_to_addr env s lv in
	    Some (m, Some (o, n))
(*
	  let m = lval_to_memloc env s lv in
	    Some (m, None)
*)
      | UnOp (Coerce _, e) -> exp_to_ptr e
      | BinOp (PlusPI, e, _) -> exp_to_ptr e
      | BinOp ((PlusI|Shiftrt|BAnd _), e1, e2) ->
	  let v1 = exp_to_ptr e1 in
	  let v2 = exp_to_ptr e2 in
	  let v =
	    match (v1, v2) with
		(Some _, Some _) -> raise Exceptions.Unknown
	      | (Some _, _) -> v1
	      | _ -> v2
	  in
	    v
      | UnOp (Cast (Int _, FunPtr), e) -> exp_to_ptr e
      | _ -> raise Exceptions.Unknown
  in
    exp_to_ptr e

(* TODO: here may use abaddr??? *)
let lval_to_buffer env s lv =
  match lv with
      Global _ | Local _ -> ((lval_to_memloc env s lv, 0), 1)
    | Shift (lv, Const CInt n) -> 
	let (m, o) = lval_to_addr env s lv in
	  (* TODO: maybe a bug with integer overflow here!! *)
	  ((m, o + (Nat.to_int n)), 1)
    | Deref (e, _) -> 
	let res =
	  match exp_to_ptr env s e with
	      Some (m, o) ->
		let (o, n) =
		  match o with
		      Some x -> x
		    | None -> (0, max_int)
		in
		  ((m, o), n)
	    | None -> raise Exceptions.Unknown
	in
	  res
    | _ ->
	(*print_endline (Newspeak.string_of_lval lv);*)
	raise Exceptions.Unknown

let may_be_null env s1 s2 e =
  let rec may_be_null e =
    match e with
	AddrOf _ -> false
	  (* TODO: this constant: not nice!!! *)
      | Lval (lv, t) when Newspeak.size_of_scalar 32 t = 32 ->
	  let a = lval_to_addr env s1 lv in
	    not (P2.addr_is_valid s2 a)
      | BinOp (PlusPI, e, _) -> may_be_null e
      | _ ->
	  (*	print_endline (Newspeak.string_of_exp e);*)
	  true
  in
    may_be_null e

let assign2 (lv, e, sz) env s1 s =
  try
    (* TODO: this constant is not nice*)
    if (sz <> 32) then raise Exceptions.Unknown;
    if may_be_null env s1 s e then raise Exceptions.Unknown;
    let a = lval_to_addr env s1 lv in
      P2.assign a s
  with Exceptions.Unknown -> 
    (* TODO: this memloc gets computed multiple times, optimize?? *) 
    let (a, n) = lval_to_buffer env s1 lv in
      (* TODO: maybe a bug here because of overflow!! *)
      P2.forget_buffer (a, n+sz-1) s

let forget_memloc_list2 m s =
  let res = ref s in
    List.iter (fun x -> res := P2.forget_memloc x !res) m;
    !res
  
(* TODO: should put this into a P1 adapter, rather than have such a complex 
   store, remove exp_to_ptr (redundant code!!) *)   
let translate_exp_P1 env s e =
  let rec translate e =
    match e with
	Const _ | AddrOfFun _ | BinOp (Eq _, _, _) -> []
      | Lval (lv, _) -> 
	  let m = lval_to_memloc_list env s lv in
	    List.map (fun x -> P1.Lval x) m
      | AddrOf (lv, n) -> 
	  let (m, o) = lval_to_addr env s lv in
	    [P1.AddrOf (m, Some (o, n))]
      | UnOp ((Coerce _|PtrToInt _|BNot _|Focus _
	      |Cast (Int _, FunPtr)|Cast (Float _, Float _)|Cast (FunPtr, Ptr)
	      |Cast (Int _, Float _)), e)
      | BinOp ((PlusPI|DivI|Mod), e, _) -> translate e
      | BinOp ((PlusI|MinusI|MultI|Shiftlt|Shiftrt
	       |BAnd _|BOr _|MinusPP), e1, e2) ->
	  let v1 = translate e1 in
	  let v2 = translate e2 in
	    v1@v2
      | _ -> 
	  invalid_arg ("Store.translate_exp_P1: case not implemented yet: "
		       ^Newspeak.string_of_exp e)
  in
    translate e

let translate_exp_P3 env s e =
  let rec translate e =
    match e with
	Const _ | AddrOf _ | BinOp (Eq _, _, _) -> []
      | Lval (lv, _) -> 
	  let m = lval_to_memloc_list env s lv in
	    List.map (fun x -> P3.Lval x) m
      | AddrOfFun (f, _) -> [P3.AddrOfFun f]
      | UnOp ((Coerce _|Cast _|PtrToInt _|BNot _|Focus _), e) 
      | BinOp ((PlusPI|DivI|Mod), e, _) -> translate e
      | BinOp ((PlusI|MinusI|MultI|Shiftrt|Shiftlt
	       |BAnd _|BOr _|MinusPP), e1, e2) ->
	  (translate e1)@(translate e2)
      | _ -> 
	  invalid_arg ("Store.translate_exp_P3: case not implemented yet: "
		       ^Newspeak.string_of_exp e)
  in
    translate e

let assign (lv, e, t) env (s1, s2, s3) = 
  try
    let m = lval_to_memloc_list env s1 lv in
    let p = translate_exp_P1 env s1 e in
    let s1 = P1.assign m p s1 in
    (* TODO: this constant not nice!! *)
    let sz = Newspeak.size_of_scalar 32 t in
    let s2 = 
      try assign2 (lv, e, sz) env s1 s2 
      with Exceptions.Unknown -> forget_memloc_list2 m s2
    in
    let fp = translate_exp_P3 env s1 e in
    let s3 = P3.assign m fp s3 in
      (s1, s2, s3)
  with Exceptions.Unknown -> 
    print_endline "Store.assign";
    (* TODO: most probably unsound, should remove all universe cases!!! *)
    forget ()

let copy (dst, src) env (s1, s2, s3) =
  let dst = lval_to_memloc_list env s1 dst in
  let src = lval_to_memloc_list env s1 src in
  let p = List.map (fun x -> P1.Lval x) src in
  let s1 = P1.assign dst p s1 in
  let s2 = forget_memloc_list2 dst s2 in
  let fp = List.map (fun x -> P3.Lval x) src in
  let s3 = P3.assign dst fp s3 in
    (s1, s2, s3)

let set_pointsto (m1, o) m2 (s1, s2, s3) =
(* TODO: not good this constant!!! *)
  let s1 = P1.assign [m1] [P1.AddrOf (m2, Some (0, 32))] s1 in
  let s2 = P2.assign (m1, o) s2 in
    (s1, s2, s3)

let guard a (s1, s2, s3) = (P1.guard a s1, P2.guard a s2, s3)

let remove_memloc m (s1, s2, s3) = 
  (P1.remove_memloc m s1, P2.remove_memloc m s2, P3.remove_memloc m s3)

(* TODO: could be optimized *)
let addr_is_valid (s1, s2, _) a =
  (P1.addr_is_valid s1 a) || (P2.addr_is_valid s2 a)

let build_transport (src, _, _) memlocs (dst, _, _) =
  P1.build_transport src memlocs dst

let split memlocs (s1, s2, s3)= 
  let (unreach1, reach1, memlocs) = P1.split memlocs s1 in
  let (unreach2, reach2) = P2.split memlocs s2 in
  let (unreach3, reach3) = P3.split memlocs s3 in
    ((unreach1, unreach2, unreach3), (reach1, reach2, reach3))

let transport tr (s1, s2, s3) = 
  (P1.transport tr s1, P2.transport tr s2, P3.transport tr s3)

let glue (a1, a2, a3) (b1, b2, b3) = 
  (P1.glue a1 b1, P2.glue a2 b2, P3.glue a3 b3)

let read_addr (s, _, _) (m, _) = get_one_abptr (P1.read s m)

let read_fun env (s1, _, s) lv = 
(* TODO: should do a lval_to_memloc_list!! *)
  let m = lval_to_memloc_list env s1 lv in
  let res = ref [] in
    List.iter (fun x -> res := !res@(P3.read s x)) m;
    !res

let to_string (s1, s2, s3) = 
  P1.to_string s1^" "^P2.to_string s2^" "^P3.to_string s3

(* usefull for debug *)
(*
let shift n1 s n2 =
  print_endline "Store.shift";
  print_endline (string_of_int n1);
  print_endline (string_of_int n2);
  print_endline (to_string s);
  let (s, tr) = shift n1 s n2 in
    print_endline (to_string s);
    print_endline "Store.shift ends";
    (s, tr)
*)
(*
let unify_on dst n src =
  print_endline "Store.unify_on";
  print_endline (to_string dst);
  print_endline (to_string src);
  let s = unify_on dst n src in
    print_endline (to_string s);
    print_endline "Store.unify_on ends";
    s

let contains s1 s2 =
  print_endline "Store.contains";
  print_endline (to_string s1);
  print_endline (to_string s2);
  let r = contains s1 s2 in
    print_endline (string_of_bool r);
    print_endline "Store.contains ends";
    r

let build_transport src memlocs dst =
  print_endline "Store.build_transport starts";
  print_endline (to_string src);
  List.iter (fun x -> print_endline (Memloc.to_string x)) memlocs;
  print_endline (to_string dst);
  let x = build_transport src memlocs dst in
    print_endline "Store.build_transport ends";
    x
*)
(*
let join s1 s2 =
  print_endline "Store.join";
  print_endline (to_string s1);
  print_endline (to_string s2);
  let s = join s1 s2 in
    print_endline (to_string s);
    print_endline "Store.join ends";
    s
*)
