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

let debug = ref false

let verbose = ref false

let obfuscate = ref false

let fun_to_count = ref []

let graphs = ref false

let output = ref "a"

let add_counted_call f = fun_to_count := f::!fun_to_count

let speclist = 
  [("--count-call", Arg.String add_counted_call, 
    "count the number of function calls");

   ("--verbose", Arg.Set verbose, 
    "prints out the detailed statistics for each function");
  
   ("--obfuscate", Arg.Set obfuscate, "obfuscates output");
   
   ("--csv", Arg.Set graphs, "generates data into csv format");

   ("-o", Arg.Set_string output, "changes name of output, default is 'a'");

   ("--debug", Arg.Set debug, "debugging mode")
  ]

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

(*
let collect filter funstats =
  let stats = ref [] in
  let get_data _ counters = stats := (filter counters)::(!stats) in
    Hashtbl.iter get_data funstats;
    List.sort compare !stats

let plot f stats fname =
    let cout = open_out fname in
    let x = ref 0 in
    let y = ref 0 in
    let dump v = 
      f x y v;
      output_string cout ((string_of_int !x)^", "^(string_of_int !y)^"\n")
    in
      List.iter dump stats;
      close_out cout
*)

class collector ptr_sz fun_to_count =
object (this)
  inherit Newspeak.visitor
    
  val mutable globals = 0
  val mutable bytes = 0
  val counters = init_counters ()
  val funstats = Hashtbl.create 10
  val mutable current_counters = init_counters ()
  val callstats = Hashtbl.create 10
  
  method count_call f =
    try
      let nb_of_calls = Hashtbl.find callstats f in
	Hashtbl.replace callstats f (nb_of_calls + 1)
    with Not_found -> ()

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
	| FunDeref _ -> 
	    current_counters.fpointer <- current_counters.fpointer + 1
    in
      true

  method process_stmt (x, _) =
    current_counters.instrs <- current_counters.instrs + 1;
    let _ = 
      match x with
	  InfLoop _ -> 
	    current_counters.loop <- current_counters.loop + 1
	| _ -> ()
    in
      true

  method process_fun f _ =
    Hashtbl.add funstats f current_counters;
    true

  method process_fun_after () =
    incr_counters counters current_counters;
    current_counters <- init_counters ()

  method process_gdecl _ (t, _) =
    globals <- globals + 1;
    this#incr_bytes ((size_of ptr_sz t) / 8);
    true

(*
  method gen filter fname =
    let stats = collect filter funstats in
    let f x y v = x := !x + 1; y := v in
      plot f stats fname

  method gen_sz filter fname =
    let filter counters = 
      let v = filter counters in
      let density = (float_of_int v) /. (float_of_int counters.instrs) in
	(density, counters.instrs, v)
    in
    let stats = collect filter funstats in
    let f x y (_, sz, n) = x := !x + sz; y := !y + n in
      plot f stats fname
*)

  method gen_graphs fname =
    let cout = open_out (fname^".csv") in
    let cnt = ref 0 in
    let dump f counters =
      incr cnt;
      let f = if !obfuscate then "f"^(string_of_int !cnt) else f in
	output_string cout (f^"; ");
	output_string cout ((string_of_int counters.instrs)^"; ");
	output_string cout ((string_of_int counters.loop)^"; ");
	output_string cout ((string_of_int counters.array)^"; ");
	output_string cout ((string_of_int counters.pointer_deref)^"; ");
	output_string cout ((string_of_int counters.pointer_arith)^"; ");
	output_string cout ((string_of_int counters.fpointer)^"\n")
    in
      output_string cout "name; number of instructions; number of loops; ";
      output_string cout "number of array accesses; ";
      output_string cout "number of pointer dereference; ";
      output_string cout "number of pointer arithmetic; ";
      output_string cout "number of function pointer\n";
      Hashtbl.iter dump funstats;
      close_out cout


  method to_string verbose = 
    let res = Buffer.create 100 in
    let fun_counter = ref 0 in
    let string_of_call f x = 
      Buffer.add_string res 
	("\n"^"Number of calls to "^f^": "^(string_of_int x))
    in
    let string_of_fun f counters =
      let f = if !obfuscate then string_of_int !fun_counter else f in
	incr fun_counter;
	Buffer.add_string res 
	  ("\n"^"Function: "^f^"\n"^(string_of_counters counters))
    in
      Buffer.add_string res 
	("Number of global variables: "^(string_of_int globals)^"\n"
	 ^"Total size of global variables (bytes): "^(string_of_int bytes)^"\n"
	 ^"Number of functions: "
	 ^(string_of_int (Hashtbl.length funstats))^"\n");
      Buffer.add_string res (string_of_counters counters);
      Hashtbl.iter string_of_call callstats;
      if verbose then Hashtbl.iter string_of_fun funstats;
      Buffer.contents res

  initializer List.iter (fun f -> Hashtbl.add callstats f 0) fun_to_count
end

let input = ref ""

let anon_fun file =
  if !input = ""
  then input := file
  else invalid_arg "You can only get statistics on one file at a time."

let usage_msg = Sys.argv.(0)^" [options] [-help|--help] file.npk"

let _ = 
  try
    Arg.parse speclist anon_fun usage_msg;

    if !input = "" 
    then invalid_arg ("no file specified. Try "^Sys.argv.(0)^" --help");

    let (_, prog, ptr_sz) = Newspeak.read !input in
    let collector = new collector ptr_sz !fun_to_count in
    let max_stats = Maxcount.count !debug ptr_sz prog in
      Newspeak.visit (collector :> Newspeak.visitor) prog;
      print_endline (collector#to_string !verbose);
      Maxcount.print max_stats;
      if !graphs then collector#gen_graphs !output
  with Invalid_argument s -> 
    print_endline ("Fatal error: "^s);
    exit 0
      
