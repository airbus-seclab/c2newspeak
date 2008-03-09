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
open Pp_syntax

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

let strip lexbuf before after = 
  let lexeme = Lexing.lexeme lexbuf in
  let len = String.length lexeme in
  let str = String.sub lexeme before (len-(before+after)) in
    str

let int_of_hex_character str =
  let len = String.length str in
  let str = String.sub str 3 (len - 4) in
  let str = "0x"^str in
    int_of_string str
      
let int_of_character str = int_of_char (str.[1])

let extract_string s = String.sub s 1 (String.length s - 2)

let token_of_ident str = 
  if Synthack.is_type str then TYPEDEF_NAME str 
  else IDENTIFIER str

let trim_newline str = 
  let i = 
    try String.index str '\r' 
    with Not_found -> 
      try String.index str '\n' 
      with Not_found -> 
	Npkcontext.error "Preprocess.trim_newline" "end of line expected"
  in
    String.sub str 0 i

let preprocess lexbuf =
  let line = Lexing.lexeme lexbuf in
  let directive = Pp_parser.parse Pp_lexer.token (Lexing.from_string line) in
  let line = trim_newline line in
    match directive with
      | Line (fname, line_nb) ->
	  Synthack.add_fname fname;
	  let line_nb = line_nb - 1 in (* Because we are then 
					  going to count a new line *)
	  let pos = { 
	    lexbuf.lex_curr_p with 
	      pos_fname = fname; pos_lnum = line_nb; pos_cnum = 0; 
	  } in
 	    set_loc lexbuf pos
      | Pragma when !Npkcontext.ignores_pragmas -> 
	  Npkcontext.print_warning "Preprocessor.parse" 
	    ("Directive ignored: "^line)
      | Pragma -> 
	  Npkcontext.error "Preprocessor.parse"
	    ("Directive not supported: "^line)
      | _ -> ()
}

let white_space = ' ' | '\t'
let new_line = '\r' | '\n' | "\r\n"
let line = [^'\r''\n']* new_line

let line_comment = "//" line

let letter = ['a'-'z'] | ['A'-'Z'] | '_'
let digit = ['0'-'9']
let oct_digit = ['0'-'7']
let hex_digit = digit | ['A'-'F']

let string = '"' [^'"']* '"'

let sign = "U" as sign
let length = ("L"|"LL") as length
let oct_integer = "0" (oct_digit+ as value) sign? length?
let hex_integer = "0x" (hex_digit+ as value) sign? length?
let integer = (digit+ as value) sign? length?
let float = (digit+ | digit+ '.' digit+) ('E' '-' digit+)?
let identifier = letter (letter|digit)*
let character = '\'' _ '\''
let hex_character = '\'' "\\x" hex_digit hex_digit '\''

rule token spec_buf = parse

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
  | "signed"            { SIGNED }
  | "unsigned"          { UNSIGNED }
  | "void"              { VOID }

(* values *)
  | oct_integer         { INTEGER (Some "0", value, sign, length) }
  | integer             { INTEGER (None, value, sign, length) }
  | hex_integer         { INTEGER (Some "0x", value, sign, length) }
  | character           { CHARACTER (int_of_character (Lexing.lexeme lexbuf)) }
  | hex_character       { CHARACTER (int_of_hex_character 
					(Lexing.lexeme lexbuf)) }
  | "\'\\0\'"           { CHARACTER 0 }
  | "\'\\n\'"           { CHARACTER 10 }
  | float               { FLOATCST  (Lexing.lexeme lexbuf) }
  | string              { STRING (extract_string (Lexing.lexeme lexbuf)) }

(* punctuation *)
  | "..."               { ELLIPSIS }
  | ","                 { COMMA }
  | ":"                 { COLON }
  | "?"                 { QMARK }
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
  | "|="                { OREQ }
  | "-="                { MINUSEQ }
  | "+="                { PLUSEQ }
  | "*="                { STAREQ }
  | "<<="               { SHIFTLEQ }
  | ";"                 { SEMICOLON }

(* operators *)
  | "&"                 { AMPERSAND }
  | "->"                { ARROW }
  | "+"                 { PLUS }
  | "-"                 { MINUS }
  | "/"                 { DIV }
  | "%"                 { MOD }
  | "++"                { PLUSPLUS }
  | "--"                { MINUSMINUS }
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

  | "#" line            { preprocess lexbuf; cnt_line lexbuf; 
			  token spec_buf lexbuf }

  | "/*!npk"            { Buffer.add_string spec_buf "/*!npk"; 
			  spec spec_buf lexbuf }
  | "/*"                { comment spec_buf lexbuf }
  | line_comment        { cnt_line lexbuf; token spec_buf lexbuf }
  | new_line            { cnt_line lexbuf; token spec_buf lexbuf }
  | white_space         { token spec_buf lexbuf }

  | eof                 { EOF }
(* error fallback *)
  | _                   { unknown_lexeme lexbuf }


and comment spec_buf = parse

  | "*/"                { token spec_buf lexbuf }
  | new_line            { cnt_line lexbuf; comment spec_buf lexbuf }
  | _                   { comment spec_buf lexbuf }

and spec spec_buf = parse
  | "*/"                { Buffer.add_string spec_buf "*/";
			  token spec_buf lexbuf }
  | new_line            { Buffer.add_string spec_buf (Lexing.lexeme lexbuf);
			  cnt_line lexbuf; token spec_buf lexbuf }
  | _ as c              { Buffer.add_char spec_buf c; 
			  spec spec_buf lexbuf }
