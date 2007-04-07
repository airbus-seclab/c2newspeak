let fname = ref ""

let anon_fun file =
  if !fname = ""
  then fname := file
  else invalid_arg "You can only get statistics on one file at a time."

let usage_msg = Sys.argv.(0)^" [options] [-help|--help] file.npk"

let speclist = 
  Npkstats.args
(*  @[("-v", Arg.Unit (Npkcontext.verbose true),
     "verbose mode: turn all verbose options on");
    
    ("-q", Arg.Unit (Npkcontext.verbose false), 
     "quiet mode: turn display off");
    
    ("--exit-code", Arg.Set Npkcontext.exit_code, "");		  
    ("-c", Arg.Set Npkcontext.exit_code, "returns 1 if an error occured")] *)
    
let _ = 
  Arg.parse speclist anon_fun usage_msg;

  if !fname = "" then begin
    print_endline ("Fatal error: no file specified. Try "
		   ^Sys.argv.(0)^" --help");
    exit 0;
  end;

  try
    let (_, npk) = Newspeak.read !fname in
      Collector.count npk;
      print_endline (Npkstats.to_string ())
  with Invalid_argument s -> 
    print_endline ("Fatal error: "^s);
    exit 0
      
