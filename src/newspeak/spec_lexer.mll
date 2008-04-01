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

{
open Spec_parser
open Lexing

(* TODO: factor this code with lexer.mll *)
let set_loc lexbuf pos = 
  lexbuf.lex_curr_p <- pos;
  Npkcontext.set_loc (pos.pos_fname, pos.pos_lnum, pos.pos_cnum)

let init fname lexbuf = 
  let pos = { lexbuf.lex_curr_p with pos_fname = fname } in
    set_loc lexbuf pos
  
let cnt_line lexbuf =
  let pos = 
    { lexbuf.lex_curr_p with pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1 }
  in
    set_loc lexbuf pos

let unknown_lexeme lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  let line = string_of_int pos.pos_lnum in
  let lexeme = Lexing.lexeme lexbuf in
  let err_msg = "Line: "^line^", unknown keyword: "^lexeme in
    Npkcontext.error "Lexer.unknown_lexeme" err_msg
}

let white_space = ' ' | '\t'
let new_line = '\r' | '\n' | "\r\n"
let line = [^'\r''\n']* new_line

let line_comment = "//" line

let letter = ['a'-'z'] | ['A'-'Z'] | '_'
let digit = ['0'-'9']
let oct_digit = ['0'-'7']
let hex_digit = digit | ['A'-'F']

let sign = "U" as sign
let length = ("L"|"LL") as length
let oct_integer = "0" (oct_digit+ as value) sign? length?
let hex_integer = "0x" (hex_digit+ as value) sign? length?
let integer = (digit+ as value) sign? length?
let float = 
  ((digit+ | digit+ '.' digit+) (('e'|'E') '-'? digit+)? as value)
  ("F" as suffix)?
let identifier = letter (letter|digit)*

rule token = parse
(* values *)
  | oct_integer         { INTEGER (Some "0", value, sign, length) }
  | integer             { INTEGER (None, value, sign, length) }
  | hex_integer         { INTEGER (Some "0x", value, sign, length) }
(* TODO: same as for lexer.mll, find a way to factor lexer code???? *)
  | float               { FLOATCST (value, suffix) }

  | identifier          { IDENTIFIER (Lexing.lexeme lexbuf) }

  | "/*!npk"            { START }
  | "*/"                { END }

  | white_space         { token lexbuf }
  | new_line            { cnt_line lexbuf; token lexbuf }

  | eof                 { EOF }
  | _ as c              { SYMBOL c }
