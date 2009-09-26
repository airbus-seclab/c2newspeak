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

module P1 = FieldInsensitivePtrOffs
module P2 = NonNullPtr
module P3 = FPtrStore

type t = (P1.t * P2.t * P3.t)

let universe = (P1.universe, P2.universe, P3.universe)

let join (a1, a2, a3) (b1, b2, b3) = 
  (P1.join a1 b1, P2.join a2 b2, P3.join a3 b3)

let contains (a1, a2, a3) (b1, b2, b3) = 
  P1.contains a1 b1 && P2.contains a2 b2 && P3.contains a3 b3

let assign ((m, o), e, sz) (s1, s2, s3) = 
  match e with
      Dom.Abaddr a -> 
	(P1.assign (m, o) a s1, P2.assign (m, o) a s2, P3.forget_memloc m s3)
    | Dom.AddrOfFun f -> (s1, P2.forget_memloc m s2, P3.assign (m, o) f s3)
    | Dom.Cst -> 
	(s1, P2.forget_buffer ((m, o), sz) s2, P3.forget_memloc m s3)

let guard a (s1, s2, s3) = (P1.guard a s1, P2.guard a s2, s3)

let remove_memloc m (s1, s2, s3) = 
  (P1.remove_memloc m s1, P2.remove_memloc m s2, P3.remove_memloc m s3)

let forget_memloc m (s1, s2, s3) = 
  (s1, P2.forget_memloc m s2, P3.forget_memloc m s3)

(* TODO: could be optimized *)
let addr_is_valid (s1, s2, _) a =
  (P1.addr_is_valid s1 a) || (P2.addr_is_valid s2 a)

let build_transport (src, _, _) memlocs (dst, _, _) =
  P1.build_transport src memlocs dst

let split memlocs (s1, s2, s3) =
  let (unreach1, reach1, memlocs) = P1.split memlocs s1 in
  let (unreach2, reach2) = P2.split memlocs s2 in
  let (unreach3, reach3) = P3.split memlocs s3 in
    ((unreach1, unreach2, unreach3), (reach1, reach2, reach3))

let transport tr (s1, s2, s3) = 
  (P1.transport tr s1, P2.transport tr s2, P3.transport tr s3)

let glue (a1, a2, a3) (b1, b2, b3) = 
  (P1.glue a1 b1, P2.glue a2 b2, P3.glue a3 b3)

let read_addr (s, _, _) a = P1.read_addr s a

let read_fun (_, _, s) a = P3.read s a

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
