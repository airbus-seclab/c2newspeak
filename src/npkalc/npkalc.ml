let input = ref ""

let speclist = []

let anon_fun filename = input := filename

let usage_msg = ""

let create_command_table () =
  let commands = Hashtbl.create 10 in
    Hashtbl.add commands "help" (fun () -> print_endline "");
    Hashtbl.add commands "exit" (fun () -> raise Exit);
    commands


let process () = 
  if !input = "" then StandardMain.report_missing_file ();

  let commands = create_command_table () in
  
  print_endline "  Welcome to the Newspeak calculator.";
  print_endline "  Type help for a list of commands.";
  
  try
    while true do
      print_string "$ ";
      let input = read_line () in
	try 
	  let execute = Hashtbl.find commands input in
	    execute ()
	with Not_found -> ()
    done
  with Exit -> 
    print_endline "Thank you for using the Newspeak calculator.";
    print_endline "Have a nice day..."

let _ =
  StandardMain.launch speclist anon_fun usage_msg process
