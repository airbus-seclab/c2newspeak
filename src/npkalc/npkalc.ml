let command_table = Hashtbl.create 10

let print_info message = print_endline ("  "^message)

let input = ref ""

(* TODO: add options --file and -f to have the commands read from a file *)
let speclist = []

let anon_fun filename = input := filename

let usage_msg = ""

let execute_help _ =
  print_info "List of available commands:";
  Hashtbl.iter (fun command _ -> print_info ("- "^command)) command_table 

let execute_exit _ = raise Exit

let execute_call _ = ()

(* TODO: add command where global which shows all places where a global is 
   being used *)
let fill_command_table () =
  Hashtbl.add command_table "help" execute_help;
  Hashtbl.add command_table "exit" execute_exit;
  Hashtbl.add command_table "call" execute_call

let process () = 
  if !input = "" then StandardMain.report_missing_file ();

  print_info "Welcome to the Newspeak calculator.";
  print_info ("Reading Newspeak file "^(!input)^"...");
  let _ = Newspeak.read !input in

    fill_command_table ();
    
    print_info "Type help for a list of commands.";
    
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
    with Exit -> 
      print_info "Thank you for using the Newspeak calculator.";
      print_info "Have a nice day..."
	
let _ =
  StandardMain.launch speclist anon_fun usage_msg process
    
