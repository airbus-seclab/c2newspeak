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

let verbose = ref false

let fun_to_count = ref []

let add_counted_call f = fun_to_count := f::!fun_to_count

let speclist = 
  [("--count-call", Arg.String add_counted_call, 
    "count the number of function calls");
    
   ("--verbose", Arg.Set verbose, 
    "prints out the detailed statistics for each function")]

type counters = 
    { mutable instrs: int; mutable loop: int; mutable array: int;
      mutable pointer_deref: int; mutable pointer_arith: int;
      mutable fpointer: int
    }

let init_counters () = 
  { instrs = 0; loop = 0; array = 0; pointer_deref = 0; pointer_arith = 0;
    fpointer = 0 }

let incr_counters dest src =
  dest.instrs <- dest.instrs + src.instrs;
  dest.loop <- dest.loop + src.loop;
  dest.array <- dest.array + src.array;
  dest.pointer_deref <- dest.pointer_deref + src.pointer_deref;
  dest.pointer_arith <- dest.pointer_arith + src.pointer_arith;
  dest.fpointer <- dest.fpointer + src.fpointer

let string_of_counters counters =
  "Number of instructions: "^(string_of_int counters.instrs)^"\n"
  ^"Number of loops: "^(string_of_int counters.loop)^"\n"
  ^"Number of array operations: "^(string_of_int counters.array)^"\n"
  ^"Number of pointer deref: "
  ^(string_of_int counters.pointer_deref)^"\n"
  ^"Number of pointer arithmetic (+): "
  ^(string_of_int counters.pointer_arith)^"\n"
  ^"Number of function pointer call: "
  ^(string_of_int counters.fpointer)
    
class collector ptr_sz =
object (this)
  inherit Newspeak.nop_visitor
    
  val mutable globals = 0
  val mutable bytes = 0
  val counters = init_counters ()
  val funstats = Hashtbl.create 0
  val mutable current_counters = init_counters ()
    
  method count_call f =
    let (nb_of_calls, counters) = Hashtbl.find funstats f in
      Hashtbl.replace funstats f (nb_of_calls + 1, counters)

  method incr_bytes i = 
    assert (i < max_int - bytes);
    bytes <- bytes + i

  method process_unop x =
    match x with
	Belongs _ -> current_counters.array <- current_counters.array + 1
      | _ -> ()

  method process_binop x =
    match x with
	PlusPI -> 
	  current_counters.pointer_arith <- current_counters.pointer_arith + 1
      | _ -> ()

  method process_lval x =
    let _ =
      match x with
	  Deref _ -> 
	    current_counters.pointer_deref <- current_counters.pointer_deref + 1
	| _ -> ()
    in
      true

  method process_fn x =
    let _ =
      match x with
	  FunId f -> this#count_call f
	| FunDeref (e, _) -> 
	    current_counters.fpointer <- current_counters.fpointer + 1
    in
      true

  method process_stmt (x, _) =
    current_counters.instrs <- current_counters.instrs + 1;
    let _ = 
      match x with
	  InfLoop body -> 
	    current_counters.loop <- current_counters.loop + 1
	| _ -> ()
    in
      true

  method process_fun f (_, x) =
    let _ = 
      match x with
	  Some _ -> Hashtbl.add funstats f (0, current_counters);
	| _ -> ()
    in
      true

  method process_fun_after () =
    incr_counters counters current_counters;
    current_counters <- init_counters ()

  method process_gdecl (_, t, _) =
    globals <- globals + 1;
    this#incr_bytes (size_of ptr_sz t);
    true

  method to_string () = 
    let res = ref 
      ("Number of global variables: "^(string_of_int globals)^"\n"
       ^"Total size of global variables (bytes): "^(string_of_int bytes)^"\n"
       ^"Number of functions: "^(string_of_int (Hashtbl.length funstats))^"\n"
       ^(string_of_counters counters))
    in
    let string_of_call f = 
      let (x, _) = Hashtbl.find funstats f in
	res := !res^"\n"^"Number of calls to "^f^": "^(string_of_int x)
    in
    let string_of_fun f (_, counters) =
      res := !res^"\n"^"Function: "^f^"\n"^(string_of_counters counters)
    in
      List.iter string_of_call !fun_to_count;
      if !verbose then Hashtbl.iter string_of_fun funstats;
      !res
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
      
