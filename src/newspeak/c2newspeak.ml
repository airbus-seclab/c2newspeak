open Params

open Newspeak
open Npkcontext
open Npkcompile
open Npklink

let create_npko name = (Filename.chop_extension name) ^ npko_suffix

let extract_npko (n1, n2) =
  if Filename.check_suffix n2 npko_suffix then begin
    let ch_in = open_in_bin n2 in
      print_debug ("Importing "^n2^"...");
      let str = Marshal.from_channel ch_in in
	if str = "NPKO" then begin 
	  let res = Marshal.from_channel ch_in in
	    print_debug ("Importing done.");
	    close_in ch_in;
	    res;
	end else begin
	  close_in ch_in;
	  error "C2newspeak.extract_npko"
	    (n2^" is an invalid .npko file");
	end;
  end else compile n1 ""


let _ =
  handle_cmdline_options ();

  try
    match !input_files with
	[] ->
	  print_error ("no file specified. Try "^Sys.argv.(0)^" --help")
      | [file, _] when !compile_only && (!output_file <> "") ->
	  ignore (compile file !output_file)
	    
      | _ when !compile_only && (!output_file <> "") ->
	  error "" ("You cannot specify the output filename (-o) for multiple "
		    ^"files when only compiling (-c)");

      | files when !compile_only && (!output_file = "") ->
	  let aux (f1, f2) = ignore (compile f1 (create_npko f2)) in
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

