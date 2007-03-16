open Npkcontext
open Cil2newspeak

let _ =
  handle_cmdline_options ();
  if !input_files = []
  then print_error ("no file specified. Try "^Sys.argv.(0)^" --help");

  let [file] = !input_files in
  let kernel = compile file in
    
    if (!verb_newspeak) then begin
      print_endline "Newspeak output";
      print_endline "---------------";
      Newspeak.dump kernel;
      print_newline ();
    end


(* TODO: Handle c and il files before compiling and linking *)

(*  let kernel = translate () in
    print_debug "Translation complete.";
    if !verb_newspeak then begin
      print_endline "Newspeak output";
      print_endline "---------------";
      K.dump kernel;
      print_newline ()
    end;
    
    if (!newspeak_output <> "") then begin
      let ch_out = open_out_bin !newspeak_output in
	print_debug ("Writing "^(!newspeak_output)^"...");
	Marshal.to_channel ch_out (fnames, kernel) [];
	print_debug ("Writing done.");
    end;
      
    kernel *)

(*  try
    ignore (Cil2newspeak.cil2newspeak !list_of_files)
  with Invalid_argument s -> print_error s;*)
