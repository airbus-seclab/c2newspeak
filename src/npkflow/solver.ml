(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2009  Charles Hymans
 
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

open Equations

let run (globals, fundecs, init) = 

  let rec process_blk x s =
    match x with
	hd::tl -> 
	  let s = process_stmt hd s in
	    process_blk tl s
      | [] -> s

  and process_stmt (x, loc) s =
    match x with
	Set set -> Store.assign set s
      | Taint lv -> Store.taint lv s
      | Decl body -> 
	  let s = Store.add_local s in
	  let s = process_blk body s in
	    Store.remove_local s
      | Select (br1, br2) -> 
	  let s1 = process_blk br1 s in
	  let s2 = process_blk br2 s in
	    Store.join s1 s2
      | Call "malloc" -> 
	  if (Store.is_tainted s (Local 0)) then begin
	    let loc = Newspeak.string_of_loc loc in
	      print_endline (loc^": malloc with external argument")
	  end;
	  s
      | Call f -> 
	  let body = Hashtbl.find fundecs f in
	    process_blk body s
  in

  let s = ref (Store.create ()) in
  let declare_global x = s := Store.add_global x !s in
    List.iter declare_global globals;
    let _ = process_blk init !s in
      ()
