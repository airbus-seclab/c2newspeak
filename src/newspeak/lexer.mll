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

let set_loc pos = 
  Npkcontext.set_loc (pos.pos_fname, pos.pos_lnum, pos.pos_cnum)


let init fname lexbuf = 
  let pos = 
    { lexbuf.lex_curr_p with pos_fname = fname }
  in
    lexbuf.lex_curr_p <- pos;
    set_loc pos
  
let cnt_line lexbuf =
  let pos = 
    { lexbuf.lex_curr_p with pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1 }
  in
    lexbuf.lex_curr_p <- pos;
    set_loc pos

let unknown_lexeme lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  let line = string_of_int pos.pos_lnum in
  let lexeme = Lexing.lexeme lexbuf in
  let err_msg = "Line: "^line^", unknown keyword: "^lexeme in
    Npkcontext.error "Lexer.unknown_lexeme" err_msg

let int64_of_string lexbuf strip_cnt = 
  let lexeme = Lexing.lexeme lexbuf in
  let len = (String.length lexeme) - strip_cnt in
  let lexeme = String.sub lexeme 0 len in
  let i = Int64.of_string lexeme in
    (* checks that Int64.of_string does not silently overflows *)
  let str = Int64.to_string i in
    if str <> lexeme
    then begin
      Npkcontext.error "Lexer.int64_of_string"
	"integer too large: not representable"
    end;
    i

let int64_of_character str = Int64.of_int (int_of_char (str.[1]))

let extract_string s = String.sub s 1 (String.length s - 2)

let token_of_ident str = 
  try
    if Synthack.is_type str then TYPEDEF_NAME str 
    else INTEGER (Synthack.find_enum str)
  with Not_found -> IDENTIFIER str

}

let white_space = ' ' | '\t'
let new_line = '\r' | '\n' | "\r\n"
let line = [^'\r''\n']* new_line

let line_comment = "//" line

let letter = ['a'-'z'] | ['A'-'Z'] | '_'
let digit = ['0'-'9']

let string = '"' [^'"']* '"'

let integer = digit+
let float = digit+ '.' digit+
let ull_integer = digit+ "ULL"
let identifier = letter (letter|digit)*
let character = '\'' _ '\''
let backslash_character = "\'\\0\'"

rule token = parse

(* keywords *)
    "break"             { BREAK }
  | "case"              { CASE }
  | "const"             { CONST }
  | "continue"          { CONTINUE }
  | "default"           { DEFAULT }
  | "do"                { DO }
  | "else"              { ELSE }
  | "for"               { FOR }
  | "enum"              { ENUM }
  | "extern"            { EXTERN }
  | "if"                { IF }
  | "return"            { RETURN }
  | "sizeof"            { SIZEOF }
  | "static"            { STATIC }
  | "switch"            { SWITCH }
  | "typedef"           { TYPEDEF }
  | "while"             { WHILE }

(* types *)
  | "char"              { CHAR }
  | "double"            { DOUBLE }
  | "float"             { FLOAT }
  | "int"               { INT }
  | "short"             { SHORT }
  | "long"              { LONG }
  | "struct"            { STRUCT }
  | "union"             { UNION }
  | "unsigned"          { UNSIGNED }
  | "void"              { VOID }

(* values *)
  | integer             { INTEGER (int64_of_string lexbuf 0) }
  | ull_integer         { INTEGER (int64_of_string lexbuf 3) }
  | character           { INTEGER (int64_of_character (Lexing.lexeme lexbuf)) }
  | backslash_character { INTEGER Int64.zero }
  | float               { FLOATCST (Lexing.lexeme lexbuf) }
  | string              { STRING (extract_string (Lexing.lexeme lexbuf)) }

(* punctuation *)
  | "..."               { ELLIPSIS }
  | ","                 { COMMA }
  | ":"                 { COLON }
  | "."                 { DOT }
  | "{"                 { LBRACE }
  | "}"                 { RBRACE }
  | "("                 { LPAREN }
  | ")"                 { RPAREN }
  | "["                 { LBRACKET }
  | "]"                 { RBRACKET }
  | "!"                 { NOT }
  | "=="                { EQEQ }
  | "!="                { NOTEQ }
  | "="                 { EQ }
  | "+="                { PLUSEQ }
  | ";"                 { SEMICOLON }

(* operators *)
  | "&"                 { AMPERSAND }
  | "->"                { ARROW }
  | "+"                 { PLUS }
  | "-"                 { MINUS }
  | "/"                 { DIV }
  | "%"                 { MOD }
  | "++"                { PLUSPLUS }
  | "&&"                { AND }
  | "||"                { OR }
  | "*"                 { STAR }
  | "<"                 { LT }
  | "<="                { LTEQ }
  | ">"                 { GT }
  | ">="                { GTEQ }
  | "<<"                { SHIFTL }
  | ">>"                { SHIFTR }
  | "^"                 { BXOR }
  | "|"                 { BOR }
  | "~"                 { BNOT }

  | identifier          { token_of_ident (Lexing.lexeme lexbuf) }

  | "#" line            { Preprocess.parse (Lexing.lexeme lexbuf); 
			  cnt_line lexbuf; token lexbuf }

  | "/*"                { comment lexbuf }
  | line_comment        { cnt_line lexbuf; token lexbuf }
  | new_line            { cnt_line lexbuf; token lexbuf }
  | white_space         { token lexbuf }

  | eof                 { EOF }
(* error fallback *)
  | _                   { unknown_lexeme lexbuf }


and comment = parse

  | "*/"                { token lexbuf }
  | new_line            { cnt_line lexbuf; comment lexbuf }
  | _                   { comment lexbuf }

