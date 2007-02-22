(*----------------------*)
(* Command line options *)
(*----------------------*)

let assumptions = ref []

let global_zero_init = ref true
let remove_temp = ref true
let accept_extern = ref false
let castor_allowed = ref false
let ignores_pragmas = ref false
let ignores_cil_merge_errors = ref false
let mergecil = ref false

let verb_warnings = ref false
let verb_debug = ref false
let verb_cil = ref false
let verb_newspeak = ref false


let exit_code = ref false

let compile_only = ref false

let newspeak_output = ref ""

let verbose boolean () =
  verb_cil := boolean;
  verb_debug := boolean;
  verb_warnings := boolean;
  verb_newspeak := boolean


let argslist = [    
  ("--printer", Arg.String (Cilutils.setCilPrinter), "");
  
  ("--pretty", Arg.Set (Newspeak.pretty_print), "uses var names for display");
  
  ("--castor", Arg.Set castor_allowed, "");
  
  ("--no-init", Arg.Clear (global_zero_init),
   "Disables zero initialisation of the globals");

  ("--ignore-pragma", Arg.Set ignores_pragmas,
   "Ignores any #pragma directive");

  ("--ignore-cil-merge-errors", Arg.Set ignores_cil_merge_errors,
   "Ignores cil errors during merge phase");

  ("--keep-unused-vars", Arg.Clear (remove_temp),
   "Do not remove unused variables");

  ("--accept-extern", Arg.Set (accept_extern),
   "Do not raise an error on variables declared but not defined");

  ("--mergecil", Arg.Set (mergecil),
   "Uses Mergecil to merge files before translating them");
  
  ("--assume", Arg.String (fun x -> assumptions := x::(!assumptions)),
   "Add an hypothesis to the analysis")
]
  
let verb_argslist = [
  ("--cil", Arg.Set (verb_cil), "verbose option: displays CIL output");
  
  ("--warnings", Arg.Set (verb_warnings), "verbose options: displays more c2n warnings");
  
  ("--debug", Arg.Set (verb_debug), "verbose options: displays more c2n debugging info");
  
  ("--newspeak", Arg.Set (verb_newspeak), "verbose option: displays Newspeak output")
]



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

let old_warnings = Hashtbl.create 100

let print_warning msg =
  let disp = msg^(string_of_loc !cur_loc) in
    if not (Hashtbl.mem old_warnings disp)
    then  begin
      prerr_endline ("Warning: "^disp);
      Hashtbl.add old_warnings disp true
    end

let print_debug msg =
  if !verb_debug then
    prerr_endline ("Debug: "^msg^(string_of_loc !cur_loc))


let error msg = invalid_arg (msg^(string_of_loc !cur_loc))

let print_error msg =
  prerr_endline ("Fatal error: "^msg);
  exit (if !exit_code then 1 else 0)



