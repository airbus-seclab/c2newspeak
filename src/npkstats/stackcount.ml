(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans, Olivier Levillain
  
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

  Olivier Levillain
  email: olivier.levillain@penjili.org
*)

open Newspeak

type stats = (bool * int * int)

let count debug ptr_sz (_, fundecs, _) =
  let unknown_funs = ref [] in
  let exact = ref true in
  let fun_tbl = Hashtbl.create 100 in

  let max_info (nb1, sz1) (nb2, sz2) = (max nb1 nb2, max sz1 sz2) in

  let rec count_call f =
    try Hashtbl.find fun_tbl f
    with Not_found -> 
      if debug then print_endline ("counting stack height of "^f);
      let (_, body) = 
	try Hashtbl.find fundecs f 
	with Not_found -> invalid_arg ("function "^f^" not defined")
      in
      let height = 
	match body with
	    Some body -> count_blk body (0, 0)
	  | None -> 
	      if not (List.mem f !unknown_funs) then begin
		unknown_funs := f::!unknown_funs;
		prerr_endline ("Warning: function "^f^" body not defined")
	      end;
	      exact := false;
	      (0, 0)
      in
	Hashtbl.add fun_tbl f height;
	height
	      
  and count_blk x info =
    match x with
	(stmt, _)::blk -> 
	  let info1 = count_stmt stmt info in
	  let info2 = count_blk blk info in
	    max_info info1 info2
      | [] -> info
	  
  and count_stmt x info =
    match x with
      | Decl (_, t, body) -> 
	  let (nb, sz) = info in
	  let nb = nb + 1 in
	  let sz = sz + ((Newspeak.size_of ptr_sz t) / 8) in 
	    count_blk body (nb, sz)
      | DoWith (body, _, []) -> count_blk body info
      | Call (FunId f) -> 
	  let (nb, sz) = info in
	  let (f_nb, f_sz) = count_call f in
	    (nb + f_nb, sz + f_sz)
      | ChooseAssert choices -> 
	  List.fold_left (count_choice info) (0, 0) choices
      | InfLoop body -> count_blk body info
      | Call _ -> invalid_arg "case not handle yet"
      | DoWith _ -> invalid_arg "case not handle yet"
      | _ -> info

  and count_choice info max_so_far (_, body) =
    let info' = count_blk body info in
      max_info info' max_so_far
  in
    (* TODO: arguments of main not counted ! *)
  let (nb, sz) = count_call "main" in
    (!exact, nb, sz)
      
let print (exact, max_nb, max_sz) =
  let symb = if exact then "" else ">= " in
    print_endline ("Maximum number of variables on the stack: "
		    ^symb^(string_of_int max_nb));
    print_endline ("Maximum height of the stack (bytes): "
		    ^symb^(string_of_int max_sz))
