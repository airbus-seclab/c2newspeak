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
*)

open Csyntax

let parse fname =
  let cin = open_in fname in
  let lexbuf = Lexing.from_channel cin in
  let specbuf = Buffer.create 800 in
    Lexer.init fname lexbuf;
    Synthack.init_tbls ();
    try
      let (fnames, globals) = Parser.parse (Lexer.token specbuf) lexbuf in
      let specbuf = Lexing.from_string (Buffer.contents specbuf) in
      let spec = Spec_parser.parse Spec_lexer.token specbuf in
	close_in cin;
	(fnames, globals, spec)
    with Parsing.Parse_error -> 
      let lexeme = Lexing.lexeme lexbuf in
      let advice = 
(* TODO: think about architecture, put in npkcontext??? *)
	if (Gnuc.is_gnuc_token lexeme) && (not !Npkcontext.accept_gnuc) 
	then ", stick to standard ANSI C or try option --gnuc"
	else ", check that your code compiles with a standard compiler";
      in
      let advice =
	if !Npkcontext.accept_gnuc || (Gnuc.is_gnuc_token lexeme) then advice
	else advice^", if you use GNU C extensions try option --gnuc"
      in
	Npkcontext.error "Compiler.parse" 
	  ("syntax error: unexpected token: "^lexeme^advice)

let append_gnu_symbols globals =
  let lexbuf = Lexing.from_string Gnuc.builtins in
  let specbuf = Buffer.create 800 in
    Lexer.init "__gnuc_builtin_symbols" lexbuf;
    Synthack.init_tbls ();
    try 
      let (_, gnuc_symbols) = Parser.parse (Lexer.token specbuf) lexbuf in
	gnuc_symbols@globals
    with Parsing.Parse_error -> 
      Npkcontext.error "Compiler.append_gnu_symbols" 
	"unexpected error while parsing GNU C symbols"

let compile fname =
  Npkcontext.print_debug ("Parsing "^fname^"...");
  let (fnames, globals, spec) = parse fname in
  let globals = 
    if !Npkcontext.accept_gnuc then append_gnu_symbols globals else globals
  in
  let fnames = if fnames = [] then fname::[] else fnames in
    Npkcontext.forget_loc ();
    Npkcontext.print_debug "Parsing done.";
    Npkcontext.print_debug "Running first pass...";
    let prog = Firstpass.translate (globals, spec) in
      Npkcontext.forget_loc ();
      Npkcontext.print_debug "First pass done.";
      Npkcontext.print_debug ("Translating "^fname^"...");
      let tr_prog = Cir2npkil.translate prog fnames in
      Npkcontext.print_debug ("Translation done.");
	Npkcontext.forget_loc ();
	tr_prog
  
