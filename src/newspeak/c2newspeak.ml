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

let compile = Cilcompiler.compile

let create_npko name = (Filename.chop_extension name) ^ Params.npko_suffix

let extract_npko fname =
  if Filename.check_suffix fname Params.npko_suffix then fname
  else begin
    let npko = create_npko fname in
      ignore (compile fname npko);
      npko
  end


let _ =
  Npkcontext.handle_cmdline_options ();

  try
    match !Npkcontext.input_files with
	[] -> 
	  Npkcontext.error "C2newspeak._" 
	    ("no file specified. Try "^Sys.argv.(0)^" --help")

      | file::[] 
	  when !Npkcontext.compile_only && (!Npkcontext.output_file <> "") ->
	  ignore (compile file !Npkcontext.output_file)
	    
      | _ when !Npkcontext.compile_only && (!Npkcontext.output_file <> "") ->
	  Npkcontext.error "" 
	    ("You cannot specify the output filename (-o) for multiple "
	      ^"files when only compiling (-c)");

      | files 
	  when !Npkcontext.compile_only && (!Npkcontext.output_file = "") ->
	  let aux f = ignore (compile f (create_npko f)) in
	    List.iter aux files

      | files ->
	  if (!Npkcontext.output_file = "") 
	  then Npkcontext.output_file := "a.npk";
	  let npkos = List.map extract_npko files in
	    Link.link npkos !Npkcontext.output_file

  with Invalid_argument msg -> Npkcontext.print_error msg
