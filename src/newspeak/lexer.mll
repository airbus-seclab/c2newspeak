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
open Parser
open Lexing

let init fname lexbuf = 
  let pos = 
    { lexbuf.lex_curr_p with pos_fname = fname }
  in
    lexbuf.lex_curr_p <- pos
  
let cnt_line lexbuf =
  let pos = 
    { lexbuf.lex_curr_p with pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1 }
  in
    lexbuf.lex_curr_p <- pos

let unknown_lexeme lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  let line = string_of_int pos.pos_lnum in
    "Lexer.mll: line: "^line^", unknown keyword: "^(Lexing.lexeme lexbuf)
}

let white_space = ' ' | '\t'
let line_terminator = '\r' | '\n' | "\r\n"

let line_comment = "//" [^'\r''\n']* line_terminator

let letter = ['a'-'z'] | ['A'-'Z']
let digit = ['0'-'9']

let integer = digit+
let identifier = letter (letter|digit)*

rule token = parse

(* keywords *)
    "if"                { IF }
  | "return"            { RETURN }

(* types *)
  | "char"              { CHAR }
  | "int"               { INT }
  | "struct"            { STRUCT }
  | "union"             { UNION }
  | "unsigned"          { UNSIGNED }
  | "void"              { VOID }

(* values *)
  | integer             { INTEGER (Int64.of_string (Lexing.lexeme lexbuf)) }

(* punctuation *)
  | ","                 { COMMA }
  | "."                 { DOT }
  | "{"                 { LBRACE }
  | "}"                 { RBRACE }
  | "("                 { LPAREN }
  | ")"                 { RPAREN }
  | "["                 { LBRACKET }
  | "]"                 { RBRACKET }
  | "="                 { EQ }
  | ";"                 { SEMICOLON }

(* operators *)
  | "&"                 { AMPERSAND }
  | "+"                 { PLUS }
  | "++"                { PLUSPLUS }
  | "*"                 { STAR }
  | "<"                 { LT }

  | identifier          { IDENTIFIER (Lexing.lexeme lexbuf) }

  | "/*"                { comment lexbuf }
  | line_comment        { cnt_line lexbuf; token lexbuf }
  | line_terminator     { cnt_line lexbuf; token lexbuf }
  | white_space         { token lexbuf }

  | eof                 { EOF }
(* error fallback *)
  | _                   { invalid_arg (unknown_lexeme lexbuf) }


and comment = parse

  | "*/"                { token lexbuf }
  | line_terminator     { cnt_line lexbuf; comment lexbuf }
  | _                   { comment lexbuf }
