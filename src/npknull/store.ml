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

module P1 = FieldInsensitivePtr
module P2 = NonNullPtr

type offset = int

type addr = Memloc.t * offset

type t = (P1.t * P2.t)

let universe = (P1.universe, P2.universe)

let join (a1, a2) (b1, b2) = (P1.join a1 b1, P2.join a2 b2)

let contains (a1, a2) (b1, b2) = P1.contains a1 b1 && P2.contains a2 b2

let assign a m (s1, s2) = (P1.assign a m s1, P2.assign a m s2)

let guard a (s1, s2) = (P1.guard a s1, P2.guard a s2)

let remove_memloc m (s1, s2) = (P1.remove_memloc m s1, P2.remove_memloc m s2)

let forget_memloc m (s1, s2) = (P1.forget_memloc m s1, P2.forget_memloc m s2)

(* TODO: could be optimized *)
let addr_is_valid (s1, s2) a =
  (P1.addr_is_valid s1 a) || (P2.addr_is_valid s2 a)

let build_transport (src, _) memlocs (dst, _) =
  P1.build_transport src memlocs dst

let split memlocs (s1, s2) =
  let (unreach1, reach1, memlocs, tr) = P1.split memlocs s1 in
  let (unreach2, reach2) = P2.split memlocs s2 in
    ((unreach1, unreach2), (reach1, reach2), tr)

let transport tr (s1, s2) = (P1.transport tr s1, P2.transport tr s2)

let glue (a1, a2) (b1, b2) = (P1.glue a1 b1, P2.glue a2 b2)

let read_addr (s, _) a = P1.read_addr s a

let to_string (s1, s2) = P1.to_string s1^" "^P2.to_string s2

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
