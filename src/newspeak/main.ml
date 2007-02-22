open Npkcontext

let default_output = ""
let list_of_files = ref []

let fnames = ref []
let incl_files = ref ""
let has_preprocess = ref true

let include_dir x = incl_files:=" -I "^x^(!incl_files)

let preprocess fname =
  let length = String.length fname in
    if (length <= 2) then invalid_arg ("file has not valid format: "^fname);
    
    let prf = String.sub fname 0 (length - 2) in
    let sff = String.sub fname (length - 2) 2 in
      if (String.compare sff ".c" <> 0) 
      then invalid_arg ("file has not valid format: "^fname);
      let resE = prf^"-E.c" in
	ignore (Unix.system ("gcc"^(!incl_files)^" -E  "^fname^" > "^resE));
	resE


let usage_msg = Sys.argv.(0)^" [options] [-help|--help] [file...]"

let anon_fun file = list_of_files:= file::!list_of_files


let  _ = 
  newspeak_output := default_output;
  let speclist = argslist@verb_argslist@[
    ("-v", Arg.Unit (verbose true),
     "verbose mode: turn all verbose options on");
    
    ("-q", Arg.Unit (verbose false), "quiet mode: turn display off");
    
    ("--exit-code", Arg.Set (exit_code), 
     "returns exit code 1 if an error occured");

    ("-I", Arg.String include_dir, 
     "For including one directory (must be repeated for each directory)");
    
    ("--no-preprocess", Arg.Clear has_preprocess,
     "skips the C preprocessing step (gcc -E)");
    
    ("-c", Arg.Set (compile_only), "compile only into a .cil file");
    
    ("-o", Arg.Set_string (newspeak_output), 
     "gives the name of Newspeak output (default is "^default_output^")")
  ] in

    verb_newspeak := true;
    Arg.parse speclist anon_fun usage_msg;
    if !has_preprocess then fnames := List.map preprocess !fnames;
    if !list_of_files = []
    then print_error ("no file specified. Try "^Sys.argv.(0)^" --help");
    try
      ignore (Cil2newspeak.cil2newspeak !list_of_files)
    with Invalid_argument s -> print_error s;
