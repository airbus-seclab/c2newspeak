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

module Set = Set.Make(String)

let callgraph = Hashtbl.create 100

let command_table = Hashtbl.create 10

let print_info message = print_endline ("  "^message)

let input = ref ""

let config_file = ref ""

let set_config_file x = config_file := x

let speclist = 
  [("--file", Arg.String set_config_file, "");
   ("-f", Arg.String set_config_file, "input from file")]

let anon_fun filename = input := filename

let usage_msg = "npkalc options <file.npk>"

let add_call f g =
  try
    let callers = Hashtbl.find callgraph g in
      Hashtbl.replace callgraph g (Set.add f callers)
  with Not_found -> Hashtbl.add callgraph g (Set.singleton f)

let get_callers f =
  try Set.elements (Hashtbl.find callgraph f)
  with Not_found -> []

let compute_call_graph prog =
  let current_function = ref "" in

  let rec compute_blk x = List.iter compute_stmt x

  and compute_stmt (x, _) =
    match x with
	Decl (_, _, body) | InfLoop body | DoWith (body, _) -> 
	  compute_blk body
      | Select (branch1, branch2) ->
	  compute_blk branch1;
	  compute_blk branch2
      | Call (_, FunId g, _) -> add_call !current_function g
      | Call (_, FunDeref _, _) -> 
	  print_info "Function pointer dereference ignored"
      | _ -> ()
  in
  let compute_fundec f declaration =
    current_function := f;
    compute_blk declaration.body
  in

  Hashtbl.iter compute_fundec prog.fundecs

let execute_help _ =
  print_info "List of available commands:";
  Hashtbl.iter (fun command _ -> print_info ("- "^command)) command_table 

let print_path p =
  let rec print_path margin p =
    match p with
      | [] -> ()
      | f::tl -> 
	  print_info (margin^f);
	  print_path (margin^"  ") tl
  in
    print_path "" p

let build_paths_from f =
  let rec build f =
    let callers = get_callers f in
      if callers = [] then [f::[]]
      else begin
	let build_one_path g = List.map (fun p -> f::p) (build g) in
	let paths = List.map build_one_path callers in
	  List.flatten paths
      end
  in
    build f

let execute_exit _ = raise End_of_file

let execute_call arguments = 
  match arguments with
      f::_ -> 
	let paths = build_paths_from f in
	  List.iter print_path paths
    | _ -> ()

(* TODO: add command where global which shows all places where a global is 
   being used *)
let fill_command_table () =
  Hashtbl.add command_table "help" execute_help;
  Hashtbl.add command_table "exit" execute_exit;
  Hashtbl.add command_table "call" execute_call

let process () = 
  if !input = "" then StandardApplication.report_missing_file ();

  print_info "Welcome to the Newspeak calculator.";
  print_info ("Reading Newspeak file "^(!input)^"...");
  let prog = Newspeak.read !input in
    print_info ("Computing call graph...");

    compute_call_graph prog;

    fill_command_table ();
    
    print_info "Type help for a list of commands.";

    let read_line =
      if !config_file = "" then read_line
      else begin
	let input_channel = open_in !config_file in
	let read_line () = 
	  let line = input_line input_channel in
	    print_endline line;
	    line
	in
	  read_line
      end
    in
    
    try
      while true do
	print_string "$ ";
	let line = read_line () in
	let line = Str.split (Str.regexp "[ \t]+") line in
	  match line with
	      command::arguments -> begin
		try 
		  let execute = Hashtbl.find command_table command in
		    execute arguments
		with Not_found -> ()
	      end
	    | [] -> ()
      done
    with End_of_file -> 
      print_info "Thank you for using the Newspeak calculator.";
      print_info "Have a nice day..."
	
let _ =
  StandardApplication.launch speclist anon_fun usage_msg process
    
