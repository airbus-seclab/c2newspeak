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

module Set = Set.Make(String)

let command_table = Hashtbl.create 10

let config_file = ref ""

let set_config_file x = config_file := x

let speclist = 
  [("--file", Arg.String set_config_file, "");
   ("-f", Arg.String set_config_file, "input from file")]

let execute_help _ =
  Utils.print_info "List of available commands:";
  Hashtbl.iter (fun command _ -> Utils.print_info ("- "^command)) command_table 

let build_paths_from callgraph f =
  let result = Hashtbl.create 10 in
  let add_call f g =
    try
      let previous_calls = Hashtbl.find result f in
	Hashtbl.replace result f (Set.add g previous_calls)
    with Not_found -> Hashtbl.add result f (Set.singleton g)
  in
  let heads = ref Set.empty in
  let visited = ref Set.empty in
  let rec build f =
    if not (Set.mem f !visited) then begin
      visited := Set.add f !visited;
      let callers = CallGraph.get_callers callgraph f in
      let process_call g = 
	add_call g f;
	build g
      in
	if callers = [] then begin
	  heads := Set.add f !heads
	end;
	List.iter process_call callers
    end
  in
    build f;
    (Set.elements !heads, result)
    
let print_paths (heads, callgraph) =
  let visited = ref Set.empty in
  let rec print_path margin f =
    let function_visited = Set.mem f !visited in
    let calls = try Hashtbl.find callgraph f with Not_found -> Set.empty in
    let suffix =
      if function_visited && not (Set.is_empty calls) then "..."
      else ""
    in
      Utils.print_info (margin^f^suffix);
      if not function_visited then begin
	visited := Set.add f !visited;
	let print_subpath g = print_path (margin^"  ") g in
	  Set.iter print_subpath calls
      end
  in
    List.iter (print_path "") heads

let execute_exit _ = raise End_of_file

let execute_call callgraph arguments = 
  match arguments with
      f::_ -> 
	let paths = build_paths_from callgraph f in
	  print_paths paths
    | _ -> ()

let execute_where callgraph arguments =
  match arguments with
      f::_ ->
	let filename = CallGraph.get_position callgraph f in
	  Utils.print_info (f^" defined in file "^filename)
    | _ -> ()

(* TODO: add command where global which shows all places where a global is 
   being used *)
let fill_command_table callgraph =
  Hashtbl.add command_table "help" execute_help;
  Hashtbl.add command_table "exit" execute_exit;
  Hashtbl.add command_table "call" (execute_call callgraph);
  Hashtbl.add command_table "where" (execute_where callgraph)


let process input = 
  Utils.print_info "Welcome to the Newspeak calculator.";
  Utils.print_info ("Reading Newspeak file "^input^"...");
  let prog = Newspeak.read input in
    Utils.print_info ("Computing call graph...");

    let callgraph = CallGraph.compute prog in

      fill_command_table callgraph;
    
      Utils.print_info "Type help for a list of commands.";

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
		with Not_found -> 
		  Utils.print_info ("Unknown command '"^command^"'. Try help.")
	      end
	    | [] -> ()
      done
    with End_of_file -> 
      Utils.print_info "Thank you for using the Newspeak calculator.";
      Utils.print_info "Have a nice day..."
	
let _ =
  StandardApplication.launch_process_with_npk_argument "npkalc" speclist process
    
