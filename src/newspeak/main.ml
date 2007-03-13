open Npkcontext
open Cil2newspeak

let _ =
  handle_cmdline_options ();
  if !input_files = []
  then print_error ("no file specified. Try "^Sys.argv.(0)^" --help");

(* TODO: Handle c and il files before compiling and linking *)

(*  try
    ignore (Cil2newspeak.cil2newspeak !list_of_files)
  with Invalid_argument s -> print_error s;*)
