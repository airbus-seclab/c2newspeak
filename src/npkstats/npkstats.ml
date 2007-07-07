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
open Newspeak

let f_tbl = Hashtbl.create 10
let count_call f = 
  try 
    let n = Hashtbl.find f_tbl f in
      Hashtbl.replace f_tbl f (n+1)
  with Not_found -> ()

let add_counted_call f = Hashtbl.add f_tbl f 0

let speclist = 
  ["--count-call", Arg.String add_counted_call, 
  "count the number of function calls"]

class collector ptr_sz =
object (this)
  inherit Newspeak.nop_visitor
    
  val mutable globals = 0
  val mutable bytes = 0

  val mutable instrs = 0
  val mutable loop = 0
  val mutable array = 0
  val mutable pointer_deref = 0
  val mutable pointer_arith = 0
  val mutable fpointer = 0
  val mutable funct = 0

  method incr_bytes i = 
    assert (i < max_int - bytes);
    bytes <- bytes + i

  method process_unop x =
    match x with
	Belongs _ -> array <- array + 1
      | _ -> ()

  method process_binop x =
    match x with
	PlusPI -> pointer_arith <- pointer_arith + 1
      | _ -> ()

  method process_lval x =
    let _ =
      match x with
	  Deref _ -> pointer_deref <- pointer_deref + 1
	| _ -> ()
    in
      true

  method process_fn x =
    let _ =
      match x with
	  FunId f -> count_call f
	| FunDeref (e, _) -> fpointer <- fpointer + 1
    in
      true

  method process_stmt (x, _) =
    instrs <- instrs + 1;
    let _ = 
      match x with
	  InfLoop body -> loop <- loop + 1
	| _ -> ()
    in
      true

  method process_fun _ (_, x) =
    let _ = 
      match x with
	  Some _ -> funct <- funct + 1
	| _ -> ()
    in
      true

  method process_gdecl (_, t, _) =
    globals <- globals + 1;
    this#incr_bytes (size_of ptr_sz t);
    true

  method to_string () = 
    let str = ref "" in
    let string_of_call f x = 
      str := !str^"Number of calls to "^f^": "^(string_of_int x)^"\n"
    in
      Hashtbl.iter string_of_call f_tbl;
      "Number of global variables: "^(string_of_int (globals))^"\n"
      ^"Total size of global variables (bytes): "^(string_of_int (bytes))^"\n"
      ^"Number of instructions: "^(string_of_int (instrs))^"\n"
      ^"Number of functions: "^(string_of_int (funct))^"\n"
      ^(!str)
      ^"Number of loops: "^(string_of_int (loop))^"\n"
      ^"Number of array operations: "^(string_of_int (array))^"\n"
      ^"Number of pointer deref: "
      ^(string_of_int (pointer_deref))^"\n"
      ^"Number of pointer arithmetic (+): "
      ^(string_of_int (pointer_arith))^"\n"
      ^"Number of function pointer call: "
      ^(string_of_int (fpointer))
end

let fname = ref ""

let anon_fun file =
  if !fname = ""
  then fname := file
  else invalid_arg "You can only get statistics on one file at a time."

let usage_msg = Sys.argv.(0)^" [options] [-help|--help] file.npk"

let _ = 

  try
    Arg.parse speclist anon_fun usage_msg;

    if !fname = "" 
    then invalid_arg ("no file specified. Try "^Sys.argv.(0)^" --help");

    let (_, prog, ptr_sz) = Newspeak.read !fname in
    let collector = new collector ptr_sz in
      Newspeak.visit (collector :> Newspeak.visitor) prog;
      print_endline (collector#to_string ())
  with Invalid_argument s -> 
    print_endline ("Fatal error: "^s);
    exit 0
      
