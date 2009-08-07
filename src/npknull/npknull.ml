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
(* TODO: factor launcher and error treatment for the several newspeak 
   utilities *)

let exec_name = "npknull"

let input = ref ""

let stats = ref false

let speclist = 
  [
    ("--stats", Arg.Set stats, "prints stats instead of function signatures")
  ]

let anon_fun file = 
  if !input <> "" then invalid_arg "you can only analyse one file at a time";
  input := file

let usage_msg = exec_name^" [options] [-help|--help] file.npk"

let _ =
  try
    Arg.parse speclist anon_fun usage_msg;
    if !input = "" 
    then invalid_arg ("no file specified. Try "^exec_name^" --help");

    let prog = Newspeak.read !input in
      Solver.process prog

  with Invalid_argument s -> 
    print_endline ("Fatal error: "^s);
    exit 0
