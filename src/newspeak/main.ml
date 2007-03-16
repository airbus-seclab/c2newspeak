open Npkcontext
open Cil2newspeak

let _ =
  handle_cmdline_options ();

  match !input_files with
      [] ->
	print_error ("no file specified. Try "^Sys.argv.(0)^" --help")
    | [file] when !compile_only
	&& (!output_file <> "") ->
	(* TODO: check that file is a .c file *)
	ignore (compile file !output_file)

    | _ when !compile_only
	&& (!output_file <> "") ->
	error ("You cannot specify the output filename for multiple "^
		 "files when only compiling");
    | files when !compile_only
	&& (!output_file = "") -> ()
	(* TODO: let npkos = List.map compile file ${file/.c/.no} *)
    | files (* when not !compile_only *) ->
	if (!output_file = "") then output_file := "a.npk";
	(* TODO: let npkos = List.map compile_or_unmarshallize file *)
	(* TODO: link *)
	()

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
