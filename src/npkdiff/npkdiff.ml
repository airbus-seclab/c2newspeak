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

(* 
   + global variable or function added
   - global variable or function removed
   * global variable or function modified
*)

open Newspeak

let input1 = ref ""
let input2 = ref ""

let anon_fun file =
  if !input1 = "" then input1 := file
  else if !input2 = "" then input2 := file
  else invalid_arg "a diff is done between only two files"

let usage_msg = Sys.argv.(0)^" [options] [-help|--help] file1.npk file2.npk"

let speclist = 
  [ ]

let hashtbl_to_list tbl =
  let res = ref [] in
  let add_binding k d = res := (k, d)::!res in
    Hashtbl.iter add_binding tbl;
    !res

let compare_key (x, _) (y, _) = compare x y

let remove_location (k, declaration) = 
  (k, (declaration.args, declaration.rets, declaration.body))

let diff postfix x1 x2 =
  let rec diff x1 x2 = 
    match (x1, x2) with
	((k1, _)::x1, (k2, _)::_) when k1 < k2 -> 
	  print_endline ("- "^k1^postfix);
	  diff x1 x2
      | ((k1, _)::_, (k2, _)::x2) when k1 > k2 ->
	  print_endline ("+ "^k2^postfix);
	  diff x1 x2
      | ((k, d1)::x1, (_, d2)::x2) when d1 <> d2 ->
	  print_endline ("* "^k^postfix);
	  diff x1 x2
      | (_::x1, _::x2) -> diff x1 x2
      | ((k, _)::x1, []) -> 
	  print_endline ("- "^k^postfix);
	  diff x1 x2
      | ([], (k, _)::x2) -> 
	  print_endline ("+ "^k^postfix);
	  diff x1 x2
      | ([], []) -> ()
  in
    diff x1 x2

let process () =
  let prog1 = Newspeak.read !input1 in
  let prog2 = Newspeak.read !input2 in
  let glbs1 = hashtbl_to_list prog1.globals in
  let glbs1 = List.sort compare_key glbs1 in
  let glbs2 = hashtbl_to_list prog2.globals in
  let glbs2 = List.sort compare_key glbs2 in
  let fundecs1 = hashtbl_to_list prog1.fundecs in
  let fundecs1 = List.map remove_location fundecs1 in
  let fundecs1 = List.sort compare_key fundecs1 in
  let fundecs2 = hashtbl_to_list prog2.fundecs in
  let fundecs2 = List.map remove_location fundecs2 in
  let fundecs2 = List.sort compare_key fundecs2 in
    diff "" glbs1 glbs2;
    diff "()" fundecs1 fundecs2

let _ = 
  StandardApplication.launch speclist anon_fun usage_msg process
