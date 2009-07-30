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

module Set = Set.Make(String)

let collect prog = 
  let pure = ref Set.empty in
  let other = ref Set.empty in

  let sort_function f (_, blk) =
    match blk with
	[] -> pure := Set.add f !pure
      | _ -> other := Set.add f !other
  in
  
    Hashtbl.iter sort_function prog.fundecs;
    let pure_nb = Set.cardinal !pure in
    let other_nb = Set.cardinal !other in
      if pure_nb <> 0 
      then print_endline ("Pure functions: "^string_of_int pure_nb);
      if other_nb <> 0 then begin
  	print_endline "Remaining functions: ";
	Set.iter print_endline !other
      end
  
