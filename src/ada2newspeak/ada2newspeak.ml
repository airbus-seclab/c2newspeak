(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans, Olivier Levillain
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.
  
  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

  Jasmine Duchon
  email : jasmine . duchon AT free . fr
  
*)

open Npkcontext

let compile fname =
  if not (Filename.check_suffix fname Params.ada_suffix)
  then error "Ada2newspeak.compile" (fname^" is not a .adb file");

  let prog = Compiler.compile fname
  in
    if (!Npkcontext.verb_npko) then begin
      print_endline "Newspeak Object output";
      print_endline "----------------------";
      Npkil.dump_npko prog;
      print_newline ();
    end;
    prog


let create_no name = (Filename.chop_extension name) ^ Params.npko_suffix

let _ =
  try
    Npkcontext.handle_cmdline_options 
      Params.version_string Params.comment_string;
    let extract_no fname =
      if Filename.check_suffix fname Params.npko_suffix then fname
      else begin
	let no = create_no fname in
	let prog = compile fname in
	  Npkil.write no prog;
	  no
      end
    in
      
      match !Npkcontext.input_files with
	  file::[] 
	    when !Npkcontext.compile_only && (!Npkcontext.output_file <> "") ->
	      let prog = compile file in
		Npkil.write !Npkcontext.output_file prog
		  
	| files ->
	    let nos = List.map extract_no files in
	      if not !Npkcontext.compile_only then begin
		Link.link nos [] !Npkcontext.output_file
	      end
		
  with Invalid_argument msg -> Npkcontext.exit_on_error msg
    
