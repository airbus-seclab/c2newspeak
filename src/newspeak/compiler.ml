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

open Lexing 

let parse fname =
  let cin = open_in fname in
  let lexbuf = Lexing.from_channel cin in
  
    try
      let cprog = Parser.cprog Lexer.token lexbuf 
      in
	close_in cin;
	cprog
    with Parsing.Parse_error -> 
      let pos = Lexing.lexeme_start_p lexbuf in
      let line_nb = string_of_int pos.pos_lnum in
      let lexeme = Lexing.lexeme lexbuf in
	Npkcontext.error "Parser.parse_error" 
	  ("syntax error: line "^line_nb^", unexpected token: "^lexeme)

(* TODO: code cleanup: remove this: remove any use of this *)
let dummy_loc = ("", -1, -1)


let compile fname = 
  let globals = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in

  let translate_fundec (f, body) = 
    Hashtbl.add fundefs f ([], None, Some []) 
  in

  let translate_global x = translate_fundec x in

  let cprog = parse fname in
    
    List.iter translate_global cprog;
    (fname, globals, fundefs)