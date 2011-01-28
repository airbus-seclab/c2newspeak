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
let output    = ref "merge.npk"

let check progs =
  if !debug then print_endline "Checking compatibility of programs...";
  if List.length progs < 2 then 
    raise (Invalid_argument "give at least two npk files to merge");
  let p = List.hd progs in
  let same p' =
    p.N.src_lang = p'.N.src_lang
  in
  if not (List.for_all same (List.tl progs)) then 
    raise (Invalid_argument "different source language")

let merge_globals t1 t2 =
  (* check that common globals are of the same type and add new
     globals of t2 into t1 *)
  let check_and_add s gdecl =
    try
      let gdecl' = Hashtbl.find t1 s in
	if gdecl = gdecl' then () 
	else raise (Invalid_argument ("variable "^s^" with different types"))
    with Not_found -> Hashtbl.add t1 s gdecl
  in
    Hashtbl.iter check_and_add t2

let merge_funs t1 t2 =
  (* check that signatures are compatible and add functions of t2 into
     t1 *)
  let sig_tbl = Hashtbl.create 100 in
  let add_sig fid args_t rets_t =
    try
      let args_t', rets_t' = Hashtbl.find sig_tbl fid in
	if not (List.for_all2 (=) args_t args_t') 
	  || not (List.for_all2 (=) rets_t rets_t') then
	    raise (Invalid_argument ("incompatible signatures for function "^fid))
    with Not_found ->     
      Hashtbl.add sig_tbl fid (args_t, rets_t) 
      | Invalid_argument _ -> 
	  raise (Invalid_argument ("incompatible signatures for function "^fid))
  in
  let rec explore_blk blk =
    match blk with
	[] 		 -> ()
      | (skind, _)::blk' -> 
	  explore_stmt skind;
	  explore_blk blk'

  and explore_stmt skind =
    match skind with
      | N.Decl (_, _, blk) | N.InfLoop blk | N.DoWith (blk, _) 	 -> explore_blk blk
      | N.Select (blk1, blk2) 				 	 -> 
	  explore_blk blk1;
	  explore_blk blk2
      | N.Call (args, N.FunId fid, rets) 			 -> 
	  let args_t = List.map snd args in
	  let rets_t = List.map snd rets in
	    add_sig fid args_t rets_t 
      | _ 						 	 -> ()
  in
  let check fid fundec =
    let args_t = List.map snd fundec.N.args in 
    let rets_t = List.map snd fundec.N.rets in 
      add_sig fid args_t rets_t;
      explore_blk fundec.N.body
  in
  let check_and_add fid fundec =
    (* check *)
    check fid fundec;
    (* add *)
    if Hashtbl.mem t1 fid then 
      raise (Invalid_argument ("function "^fid^" defined twice"))
    else
      Hashtbl.add t1 fid fundec
  in
    Hashtbl.iter check t1;
    Hashtbl.iter check_and_add t2

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
    merge_globals po.N.globals p.N.globals;
    merge_funs po.N.fundecs p.N.fundecs;
    p.N.init @ init
  in
  let init = List.fold_left add po.N.init (List.tl progs) in
    { po with N.init = List.rev init; }

let process () = 
  let progs = List.map Newspeak.read !input in
    check progs;
    let p = merge progs in
      Newspeak.write !output p;
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
