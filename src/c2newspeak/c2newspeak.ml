(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans, Olivier Levillain, Sarah Zennou
  
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

  Sarah Zennou
  EADS Innovation Works - SE/IS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: sarah (dot) zennou (at) eads (dot) net
*)

let compile fname =
  let prog = Compiler.compile fname in
    if (!Npkcontext.verb_npko) then begin
      print_endline "Newspeak Object output";
      print_endline "----------------------";
      Npkil.dump prog;
      print_newline ();
    end;
    prog


let create_no name = (Filename.chop_extension name) ^ Params.npko_suffix

let extract_no fname =
  if Filename.check_suffix fname Params.npko_suffix then fname
  else begin
    let no = create_no fname in
    let prog = compile fname in
      Npkil.write no prog;
      Npkcontext.dump_xml_warns ();
      no
  end

let execute () =
  (* TODO: this code should be factored with ada2newspeak!! into x2newspeak *)
  match !Npkcontext.input_files with
      file::[] 
	when !Npkcontext.compile_only && (!Npkcontext.output_file <> "") ->
	  let prog = compile file in
	    Npkil.write !Npkcontext.output_file prog;
	    Npkcontext.dump_xml_warns ()
	      
    | files ->
	let nos = List.map extract_no files in
	  if not !Npkcontext.compile_only then begin 
	    Linker.link nos;
	    Npkcontext.dump_xml_warns ()
	  end
	    
let _ =
  X2newspeak.process Params.version_string Params.comment_string execute
    
