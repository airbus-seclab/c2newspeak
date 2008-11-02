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


let input = ref ""
let output = ref "a.npk"
let print = ref false
let inline = ref false
let hoist = ref false
let propag_exp = ref false

let anon_fun fname =
  if !input = "" then input := fname
  else invalid_arg "You can only simplify one file at a time."

let usage_msg = Sys.argv.(0)^" [options] [-help|--help] file.npk"

let speclist = 
  [ ("--newspeak", Arg.Set print, "prints output");
   
    ("-o", Arg.Set_string output, 
    "chooses name of output files, default is a.npk");

    ("--inline", Arg.Set inline, 
    "applies function inlining of depth 1");

    ("--hoist", Arg.Set hoist, 
    "applies hoist variables transformation");

    ("--propag-exp", Arg.Set propag_exp, 
    "applies copy propagation of expression");
  ]

let _ =
  try
    Arg.parse speclist anon_fun usage_msg;
    
    if !input = ""
    then invalid_arg ("no file specified. Try "^Sys.argv.(0)^" --help");

    let (files, prog, ptr_sz) = Newspeak.read !input in
    let prog = if !propag_exp then Copy_propagation.process prog else prog in
    let prog = if !inline then Inline.process prog else prog in
    let prog = if !hoist then Var_hoist.process prog else prog in
    let npk = (files, prog, ptr_sz) in
      if !print then Newspeak.dump npk;
      Newspeak.write !output npk
  with Invalid_argument s -> 
    print_endline ("Fatal error: "^s);
    exit 0
