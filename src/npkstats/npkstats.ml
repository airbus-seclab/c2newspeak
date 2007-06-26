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


type counter = int ref

let globals = ref 0
let bytes = ref 0

let instrs = ref 0
let loop = ref 0
let array = ref 0
let pointer_deref = ref 0
let pointer_arith = ref 0
let fpointer = ref 0
let f_tbl = Hashtbl.create 10
let funct = ref 0

let to_string () = 
  let str = ref "" in
  let string_of_call f x = 
    str := !str^"Number of calls to "^f^": "^(string_of_int x)^"\n"
  in
    Hashtbl.iter string_of_call f_tbl;
    "Number of global variables: "^(string_of_int (!globals))^"\n"
    ^"Total size of global variables (bytes): "^(string_of_int (!bytes))^"\n"
    ^"Number of instructions: "^(string_of_int (!instrs))^"\n"
    ^"Number of functions: "^(string_of_int (!funct))^"\n"
    ^(!str)
    ^"Number of loops: "^(string_of_int (!loop))^"\n"
    ^"Number of array operations: "^(string_of_int (!array))^"\n"
    ^"Number of pointer deref: "
    ^(string_of_int (!pointer_deref))^"\n"
    ^"Number of pointer arithmetic (+): "
    ^(string_of_int (!pointer_arith))^"\n"
    ^"Number of function pointer call: "
    ^(string_of_int (!fpointer))

let count x = incr x

let incr_counter x i = 
  assert (i < max_int - !x);
  x := !x + i

let count_call f = 
  try 
    let n = Hashtbl.find f_tbl f in
      Hashtbl.replace f_tbl f (n+1)
  with Not_found -> ()

let add_counted_call f = Hashtbl.add f_tbl f 0
     
let args = ["--count-call", Arg.String add_counted_call, 
	    "count the number of function calls"]
