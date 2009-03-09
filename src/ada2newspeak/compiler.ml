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

  Jasmine Duchon
  email : jasmine . duchon AT free . fr
*)

let compile(fname:string):Npkil.t =
  let base_name = Filename.basename fname
  and dir_name = Filename.dirname fname
  and current_dir = Sys.getcwd () in
    if dir_name <> "."
    then
      begin
	Npkcontext.print_debug ("Changing directory : "^dir_name);
	Sys.chdir dir_name
      end;
    Npkcontext.print_debug ("Parsing "^fname^"...");
    let (ast:Syntax_ada.compilation_unit) =
      Ada_parse.parse base_name in
      if (!Npkcontext.verb_ast) then begin
	print_endline "Abstract Syntax Tree";
	print_endline "----------------------";
	Print_syntax_ada.print_ast [ast];
	print_newline ();
      end;
      Npkcontext.forget_loc ();
      Npkcontext.print_debug "Parsing done.";
      Npkcontext.print_debug "Running first pass...";
      let prog = Firstpass.translate ast
      in
	Npkcontext.forget_loc ();
	Npkcontext.print_debug "First pass done.";
	Npkcontext.print_debug ("Translating "^fname^"...");
	let tr_prog = Cir2npkil.translate prog [fname]
	in
	  Npkcontext.forget_loc ();
	  if dir_name <> "."
	  then
	    begin
	      Npkcontext.print_debug ("Changing directory : "^current_dir);
	      Sys.chdir current_dir
	    end;
	  tr_prog

