open Params

open Newspeak
open Npkcontext
open Npkcompile
open Npklink

let create_npko name = (Filename.chop_extension name) ^ npko_suffix

let extract_npko fname =
  if Filename.check_suffix fname npko_suffix then fname
  else begin
    let npko = create_npko fname in
      ignore (compile fname npko);
      npko
  end


let _ =
  handle_cmdline_options ();

  try
    match !input_files with
	[] ->
	  print_error ("no file specified. Try "^Sys.argv.(0)^" --help")
      | file::[] when !compile_only && (!output_file <> "") ->
	  ignore (compile file !output_file)
	    
      | _ when !compile_only && (!output_file <> "") ->
	  error "" ("You cannot specify the output filename (-o) for multiple "
		    ^"files when only compiling (-c)");

      | files when !compile_only && (!output_file = "") ->
	  let aux f = ignore (compile f (create_npko f)) in
	    List.iter aux files

      | files (* when not !compile_only *) ->
	  if (!output_file = "") then output_file := "a.npk";
	  let npkos = List.map extract_npko files in
	  let prog = link npkos in
	    print_debug ("Writing output to "^(!output_file)^"...");
	    Newspeak.write !output_file prog;
	    print_debug (!output_file^" written.")

  with Invalid_argument s -> print_error s




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

