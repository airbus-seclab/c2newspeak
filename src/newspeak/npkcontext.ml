open Params

(*----------------------*)
(* Command line options *)
(*----------------------*)


(* Translation options *)

let global_zero_init = ref true
let castor_allowed = ref false
let ignores_pragmas = ref false
let remove_temp = ref true
let accept_extern = ref false

let accept_mult_def = ref false

(* TODO: Handle assumptions correctly *)
(* let assumptions = ref [] *)


(* Verbose options *)

let verb_morewarns = ref false
let verb_debug = ref false
let verb_cil = ref false
let verb_npko = ref false
let verb_newspeak = ref false
let pretty_print = ref false

let verbose boolean () =
  verb_cil := boolean;
  verb_debug := boolean;
  verb_morewarns := boolean;
  verb_newspeak := boolean

let exit_code = ref false


(* Preprocessing options *)

let incl_files = ref ""
let include_dir x = incl_files:=" -I "^x^(!incl_files)

(* removed should be done by the tool user
let preprocess fname =
  if not (Filename.check_suffix fname c_suffix)
  then invalid_arg (fname^"is not a .c file");
  let ppd_file = (Filename.chop_extension fname)^"-E.c" in
    ignore (Unix.system ("gcc"^(!incl_files)^" -E  "^fname^" > "^ppd_file));
    ppd_file, fname
*)

(* File options *)

let input_files = ref []
let anon_fun file = input_files := file::!input_files
let compile_only = ref false
let output_file = ref ""


(* Version *)

let version = ref false


let usage_msg =
  version_string ^ "\nUsage: "^
    Sys.argv.(0)^" [options] [-help|--help] [file...]\n"

let argslist = [    
  ("--no-init", Arg.Clear (global_zero_init),
   "disables zero initialisation of the globals");

  ("--castor", Arg.Set castor_allowed,
   "allows horrible casts to be translated");
  
  ("--ignore-pragma", Arg.Set ignores_pragmas,
   "ignores any #pragma directive");

  ("--keep-unused-vars", Arg.Clear (remove_temp),
   "does not remove unused variables");

  ("--accept-extern", Arg.Set (accept_extern),
   "does not raise an error on variables declared but not defined\n");

  ("--accept-mult-def", Arg.Set (accept_mult_def),
   "does not raise an error multiple definitions of the same variables\n");

(* TODO: Handle assumptions correctly *)
(*  ("--assume", Arg.String (fun x -> assumptions := x::(!assumptions)),
   "adds an hypothesis to the analysis");*)


  ("--cil", Arg.Set (verb_cil),
   "verbose option: displays CIL output");
  
  ("--cil-printer", Arg.String (Cilutils.setCilPrinter),
   "verbose options: uses \"default\" or \"plain\" Cil output");

  ("--more-warnings", Arg.Set (verb_morewarns),
   "verbose options: displays more warnings");
  
  ("--debug", Arg.Set (verb_debug),
   "verbose options: displays more debugging info");
  
  ("--npko", Arg.Set (verb_npko),
   "verbose option: displays NewsPeak Object intermediate output");

  ("--newspeak", Arg.Set (verb_newspeak),
   "verbose option: displays Newspeak output");

  ("--pretty", Arg.Set (pretty_print),
   "verbose options: uses var names for Newspeak display");

  ("-v", Arg.Unit (verbose true),
   "verbose mode: turn all verbose options on");
    
  ("-q", Arg.Unit (verbose false),
   "quiet mode: turn display off");
    
  ("--exit-code", Arg.Set (exit_code), 
   "returns exit code 1 if an error occured\n");

(* Removed: should be done by the tool user
  ("--preprocess", Arg.Set has_preprocess,
   "enables the C preprocessing step (gcc -E)");
*)
  ("-I", Arg.String include_dir, 
  "includes a pre-processing directory\n "^
    "                    (must be repeated for each directory)\n");

  ("-c", Arg.Set compile_only,
  "compiles only into a .il file");
  
  ("-o", Arg.Set_string output_file, 
  "gives the name of Newspeak output\n");

  ("--version", Arg.Set version,
  "prints the version of the software");
]


let handle_cmdline_options () = 
  Arg.parse argslist anon_fun usage_msg;
  if !version then begin
    print_version ();
    exit 0
  end






(*-------------------*)
(* Location handling *)
(*-------------------*)

let cur_loc = ref ("", -1, -1)

let string_of_loc (file, line, _) =
  if (line > 0)
  then " in "^(file)^" line "^(string_of_int line)
  else ""





(*----------------------------------------*)
(* Warnings/errors generation and display *)
(*----------------------------------------*)


(* TODO: Watch this ! *)
module String_set = 
  Set.Make (struct type t = string let compare = Pervasives.compare end)

let old_warnings = ref (String_set.empty)

let print_warning where msg =
  let disp = if (!verb_debug && where <> "")
  then "Warning ("^where^"): "^msg^(string_of_loc !cur_loc)
  else "Warning: "^msg^(string_of_loc !cur_loc)
  in
    if not (String_set.mem disp !old_warnings)
    then  begin
      prerr_endline disp;
      old_warnings := String_set.add disp !old_warnings
    end

let print_morewarn where msg =
  if !verb_morewarns then print_warning where msg

let print_debug msg =
  if !verb_debug then 
    prerr_endline ("Debug: "^msg^(string_of_loc !cur_loc))


let error where msg =
  let disp = if (!verb_debug && where <> "")
  then "("^where^") "^msg^(string_of_loc !cur_loc)
  else msg^(string_of_loc !cur_loc)
  in
    invalid_arg disp

let print_error msg =
  prerr_endline ("Fatal error: "^msg);
  exit (if !exit_code then 1 else 0)



