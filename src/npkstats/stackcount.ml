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


let count ptr_sz (_, fundecs, _) =
  let max_nb = ref 0 in
  let max_sz = ref 0 in
  let rec count_call f sz =
    let (_, body) = 
      try Hashtbl.find fundecs f 
      with Not_found -> invalid_arg ("function "^f^" not defined")
    in
      match body with
	  Some body -> count_blk body sz
	| None -> invalid_arg ("function "^f^" not defined")

  and count_blk x sz =
    match x with
	(stmt, _)::blk -> 
	  count_stmt stmt sz;
	  count_blk blk sz
      | [] -> ()
  
  and count_stmt x info =
    match x with
      | Decl (_, t, body) -> 
	  let (nb, sz) = info in
	  let nb = nb + 1 in
	  let sz = sz + ((Newspeak.size_of ptr_sz t) / 8) in 
	    if !max_nb < nb then max_nb := nb;
	    if !max_sz < sz then max_sz := sz;
	    count_blk body (nb, sz)
      | DoWith (body, _, []) -> count_blk body info
      | Call (FunId f) -> count_call f info
      | ChooseAssert choices -> 
	  List.iter (fun (_, body) -> count_blk body info) choices
      | InfLoop body -> count_blk body info
      | Call _ -> invalid_arg "case not handle yet"
      | DoWith _ -> invalid_arg "case not handle yet"
      | _ -> ()
  in

(* TODO: arguments of main not counted ! *)
    count_call "main" (0, 0);
    print_endline ("Maximum number of variables on the stack: "
		   ^(string_of_int !max_nb));
    print_endline ("Maximum height of the stack (bytes): "
		   ^(string_of_int !max_sz))
