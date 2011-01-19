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

module N      = Newspeak
let debug     = ref false
let print     = ref false
let input     = ref []
let output    = ref "a.npk"

let check progs =
  if !debug then print_endline "Checking compatibility of programs...";
  if List.length progs < 2 then 
    raise (Invalid_argument "give at least two npk files to merge");
  let p = List.hd progs in
  let same p' =
    p.N.src_lang = p'.N.src_lang
  in
  if not (List.for_all same (List.tl progs)) then 
    raise (Invalid_argument "different configurations (pointer size or source language)");
    if true then raise (Invalid_argument "check function signature");
  if true then raise (Invalid_argument "check something about globals")

(*let merge_globals t1 t2 =*)
  (* check that common globals are of the same type and add new
  globals of t2 into t1 *)
  (* let check_and_add s gdecl = *)
(*     try  *)
(*       let gdecl' = Hashtbl.find s t1 in *)
(*     with Not_found -> Hashtbl.add s gdecl t1 *)
(*   in *)
(*     Hashtbl.iter check_and_add t2 *)

let merge progs = 
  if !debug then print_endline "Merging...";
  let pi = List.hd progs in
  let po = {
    N.globals  = Hashtbl.copy pi.N.globals;
    N.init     = pi.N.init;
    N.fundecs  = Hashtbl.copy pi.N.fundecs;
    N.ptr_sz   = pi.N.ptr_sz;
    N.src_lang = pi.N.src_lang;
  } 
  in
  let add init p =
    (*merge_globals po.N.globals p.N.globals;
    merge_funs po.N.globals p.N.fundecs; *)
    p.N.init @ init
  in
  let init = List.fold_left add po.N.init (List.tl progs) in
    { po with N.init = List.rev init; }

let process () = 
  let progs = List.map Newspeak.read !input in
    check progs;
    let p = merge progs in
      if !print then Newspeak.dump p

let _ = 
  let anon_fun (f: N.fid) = input := f::!input in
  let usage_msg 	  = Sys.argv.(0)^" [options] [-help|--help] file1.npk file2.npk ..." in
  let speclist  	  = [
    ("--debug", Arg.Set debug, "prints debug information");
    ("--print", Arg.Set print, "prints output");
    ("-o"     , Arg.Set_string output, "gives a name to the output (default is "^(!output)^")")] 
  in
    StandardApplication.launch speclist anon_fun usage_msg process
