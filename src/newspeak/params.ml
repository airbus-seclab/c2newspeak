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

(*-----------------*)
(* File extensions *)
(*-----------------*)

let c_suffix = ".c"
let npko_suffix = ".no"



(*------------------------*)
(* Version and other info *)
(*------------------------*)

let software = "C2Newspeak"
let version = "0.8"
let authors = "Olivier Levillain and Charles Hymans"
let licence = "LGPL v. ??"
let copyright = "EADS"
let comment =[
  "   The Newspeak language and C2Newspeak have initially been developped in";
  " EADS Innovation Works, Suresnes, France, by Olivier Levillain and Charles";
  " Hymans.";
  "";
  "   C2Newspeak compiles C code into Newspeak language, which allows static";
  " analysis and statistics on C code thanks to a simpler and non ambiguous";
  " language."]


let version_string =
  software^" v."^version^" by "^authors^".\nSoftware under "
  ^licence^". Copyright "^copyright^".\n"


let print_version () =
  print_endline (version_string);
  List.iter print_endline comment
