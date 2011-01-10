(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2011  Sarah Zennou
  
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

  Sarah Zennou
  EADS Innovation Works - SE/IS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: sarah(dot)zennou(at)eads(dot)net
*)

module N = Newspeak

let check_compatibility prog1 prog2 =
  if prog1.N.src_lang <> prog2.N.src_lang then 
    raise (Invalid_argument "different source languages");
  if prog1.N.ptr_sz <> prog2.N.ptr_sz then
    raise (invalid_arg "different configuration (pointer size)")

type couple = {mutable fst : string; mutable snd : string}
let debug = ref false
let input     = {fst = ""; snd = ""} 
let speclist  = [("--debug", Arg.Set debug, "prints debug information")]
let usage_msg = Sys.argv.(0)^" [options] [-help|--help] file1.npk file2.npk"

let anon_fun f = 
  if input.fst = "" then input.fst <- f
  else 
    if input.snd = "" then input.snd <- f
    else invalid_arg "you can only analyse a pair of files at a time"
 
let merge _p1 _p2 = 
 invalid_arg "Npkmerger.merge: to continue"

let process () = 
  let p1 = Newspeak.read input.fst in
  let p2 = Newspeak.read input.snd in
    check_compatibility p1 p2;
    merge p1 p2

let _ = StandardApplication.launch [] anon_fun usage_msg process
