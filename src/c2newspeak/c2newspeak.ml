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

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org

  Olivier Levillain
  email: olivier.levillain@penjili.org
*)

open Npkcontext

let compile fname =
  let prog =
    if !Npkcontext.use_cil then Cilcompiler.compile fname
    else Compiler.compile fname
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
    Npkcontext.handle_cmdline_options Params.version_string 
      Params.comment_string;
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
		let mem_zones = 
		  if !Npkcontext.config_file = "" then []
		  else Compiler.compile_config !Npkcontext.config_file
		in
		  Link.link nos mem_zones !Npkcontext.output_file
	      end
		
  with Invalid_argument msg -> Npkcontext.exit_on_error msg
    
