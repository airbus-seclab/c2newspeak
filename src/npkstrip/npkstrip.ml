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

let input = ref ""
let output = ref "a.npk"
let main = ref "main"
let print = ref false

let anon_fun file =
  if !input = "" then input := file
  else invalid_arg "You can only strip one file at a time."

let usage_msg = Sys.argv.(0)^" [options] [-help|--help] file.npk"

let speclist = 
  [ ("--main", Arg.Set_string main,
    "Choose main function from which to strip");
    
    ("--print-newspeak", Arg.Set print,
    "Print output");
    
    ("-o", Arg.Set_string output, 
    "Choose name of output file, default is a.npk");
  ]

module StringSet = Set.Make(String)

class collector (globals, fundecs) used_gvars used_funs =
object (this)
  inherit Newspeak.visitor

  method add_global x =
    if not (Hashtbl.mem used_gvars x) then begin
      let gdecl = Hashtbl.find globals x in
	Hashtbl.add used_gvars x gdecl;
	Newspeak.visit_glb (this :> Newspeak.visitor) x gdecl
    end

  method visit_fun f = 
    try
      if not (Hashtbl.mem used_funs f) then begin
	let fundec = Hashtbl.find fundecs f in
	  Hashtbl.add used_funs f fundec;
	  Newspeak.visit_fun (this :> Newspeak.visitor) f fundec
      end
    with Not_found -> 
      this#print_warning ("unknown function "^f^" called:"
			  ^" assuming it does not call any function,"
			  ^" strip may be incorrect")

  method process_funexp x =
    begin match x with
	FunId f -> this#visit_fun f
      | _ -> ()
    end;
    true

  method process_lval x =
    match x with
	Global x -> 
	  this#add_global x;
	  true
      | _ -> true

  method process_exp x =
    begin match x with
	AddrOfFun (f, _) -> this#visit_fun f
      | _ -> ()
    end;
    true

end

let rec global_of_lval x =
  match x with
      Global x -> x
    | Shift (lv, _) -> global_of_lval lv
    | _ -> invalid_arg "Npkstrip.global_of_lval: unreachable code"

let collect_used prog =
  let used_gvars = Hashtbl.create 100 in
  let used_funs = Hashtbl.create 100 in

  let collector = 
    new collector (prog.globals, prog.fundecs) used_gvars used_funs
  in

  let visit_init (x, _) =
    match x with
	Set (lv, e, _) -> 
	  let g = global_of_lval lv in
	    if Hashtbl.mem used_gvars g 
	    then Newspeak.visit_exp (collector :> Newspeak.visitor) e
      | UserSpec x -> Newspeak.visit_assertion (collector :> Newspeak.visitor) x
      | _ -> invalid_arg "Npkstrip.visit_init: case not implemented yet"
  in
    
  let rec strip_init x =
    match x with
	((Set (lv, _, _), _) as hd)::tl -> 
	  let tl = strip_init tl in
	  let g = global_of_lval lv in
	    if Hashtbl.mem used_gvars g then hd::tl
	    else tl
      | ((UserSpec _, _) as hd)::tl -> hd::(strip_init tl)
      | [] -> []
      | _ -> invalid_arg "Npkstrip.strip_init: case not implemented yet"
  in

    collector#visit_fun !main;
    List.iter visit_init prog.init;
    let init = strip_init prog.init in
      { prog with init = init; globals = used_gvars; fundecs = used_funs }
 
let filter_globals used_gvars globals = 
  let is_used (x, _, _) = StringSet.mem x used_gvars in
    List.filter is_used globals
      
let process () =
  let prog = Newspeak.read !input in
  let npk = collect_used prog in
    if !print then Newspeak.dump npk;
    Newspeak.write !output npk
  
let _ = 
  StandardApplication.launch speclist anon_fun usage_msg process
