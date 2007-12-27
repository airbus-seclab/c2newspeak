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

open Pp_syntax

let trim_newline str = 
  let i = 
    try String.index str '\r' 
    with Not_found -> 
      try String.index str '\n' 
      with Not_found -> 
	Npkcontext.error "Preprocess.trim_newline" "end of line expected"
  in
    String.sub str 0 i

let parse line =
  let lexbuf = Lexing.from_string line in
  let directive = Pp_parser.parse Pp_lexer.token lexbuf in
  let line = trim_newline line in
    match directive with
	Pragma when !Npkcontext.ignores_pragmas -> 
	  Npkcontext.print_warning "Preprocessor.parse" 
	    ("Directive ignored: "^line)
      | Pragma -> 
	  Npkcontext.error "Preprocessor.parse"
	    ("Directive not supported: "^line)
      | _ -> ()
