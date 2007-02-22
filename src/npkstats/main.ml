let fnames = ref []
  
let usage_msg = ""

let anon_fun x = fnames := x::!fnames

let speclist = 
  (* Newspeak options *)
  Npkcontext.argslist
  (* Verbose options *)
  @Npkcontext.verb_argslist
  @Npkstats.args
  @[("-v", Arg.Unit (Npkcontext.verbose true),
     "verbose mode: turn all verbose options on");
    
    ("-q", Arg.Unit (Npkcontext.verbose false), 
     "quiet mode: turn display off");
    
    ("--exit-code", Arg.Set Npkcontext.exit_code, "");		  
    ("-c", Arg.Set Npkcontext.exit_code, "returns 1 if an error occured")]
    
let _ = 
  Arg.parse speclist anon_fun usage_msg;
  
  if !fnames = [] then begin
    print_endline ("Fatal error: no file specified. Try "
		   ^Sys.argv.(0)^" --help");
    exit (if !Npkcontext.exit_code then 1 else 0);
  end;
  
  try
    let prog = Cil2newspeak.cil2newspeak !fnames in
      Collector.count prog;
      print_endline (Npkstats.to_string ())
  with Invalid_argument s -> 
    print_endline ("Fatal error: "^s);
    exit (if !Npkcontext.exit_code then 1 else 0)
      
