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

open Lowspeak

class collector warnings =
object (this)
  inherit Lowspeak.visitor

  method process_lval x =
    let _ = 
      match x with
	  Deref _ -> 
	    let loc = Newspeak.string_of_loc this#get_loc in
	      warnings := StrSet.add loc !warnings
	| _ -> ()
    in
      (* TODO: maybe visitor too complex, shouldn't have to return a bool, just
	 goes everywhere!!! *)
      true
end

(* TODO: share this code with npkstats!! *)
let count_derefs prog live_funs =
  let warnings = ref StrSet.empty in
  let collector = new collector warnings in
  let visit_fun f dec =
    if StrSet.mem f live_funs 
    then Lowspeak.visit_fun collector f dec
  in
    Hashtbl.iter visit_fun prog.fundecs;
    StrSet.cardinal !warnings

let print prog (live_funs, warn_nb) = 
  let funs = Hashtbl.length prog.fundecs in
  let dead_funs = funs - StrSet.cardinal live_funs in
  let dead_msg = 
    "Dead functions: "^string_of_int dead_funs^" / "^string_of_int funs
  in
  let deref_nb = count_derefs prog live_funs in
  let warn_msg =
    "Null pointer derefs: "^string_of_int warn_nb^" / "^string_of_int deref_nb
  in
    print_endline dead_msg;
    print_endline warn_msg
