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

open Lowspeak

module L = Lowspeak
module S = Set.Make(String)

let print funs cout =
  if not (S.is_empty funs) then
    let s = S.fold (fun fid s -> s ^"\n"^fid) funs "" in
      print_endline ("Library functions potentially called:"^s);
      match cout with
	  None -> ()
	| Some cout ->
	    let s = 
	      S.fold (fun fid s -> s^"\n"^"<libfun val=\""^fid^"\"></libfun>") funs ""
	    in
	      output_string cout s

let collect prog cout =

  let libfuns = ref S.empty in
  let add fid =
	  if not (Hashtbl.mem prog.L.fundecs fid) then 
	    libfuns := S.add fid !libfuns 
	  else ()
  in
  let rec process_exp e =
    match e with
	L.Const _ -> ()
      | L.Lval _ -> ()
      | L.AddrOf _ -> ()
      | L.AddrOfFun (f, _) -> add f
      | L.UnOp (_, e') -> process_exp e'
      | L.BinOp (_, e1, e2) -> process_exp e1; process_exp e2

  and process_stmt s =
    match s with
      | L.Set (_, e, _)        -> process_exp e (* for function pointer *)
      | L.Copy _ 	       -> ()
      | L.Guard _ 	       -> ()
      | L.Decl (_, _, blk)     -> process_blk blk
      | L.Select (iblk, eblk)  -> process_blk iblk ; process_blk eblk
      | L.InfLoop blk 	       -> process_blk blk
      | L.DoWith (blk, _)      -> process_blk blk
      | L.Goto _ 	       -> ()
      | L.Call (L.FunId f)     -> add f
      | L.Call _ 	       -> ()
      | L.UserSpec _ 	       -> ()

  and process_blk blk =
    match blk with
	[] -> ()
      | (stmt, _)::blk' -> process_stmt stmt; process_blk blk'

  and process_fun _ declaration = process_blk declaration.body in

    Hashtbl.iter process_fun prog.L.fundecs;
    print !libfuns cout;
