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

let int64_of_string base str = 
  let str = String.concat "" [base; str] in
    try 
      let i = Int64.of_string str in
	(* do this because Int64.of_string "Int64.max_int + 1 as a string" 
	   returns Int64.min_int!!! *)
	if Int64.compare i Int64.zero < 0 then raise Exit;
	i
    with _ ->
      Npkcontext.error "Lexer.int64_of_string"
	"integer too large: not representable"

let int64_of_hex_character str =
  let len = String.length str in
  let str = String.sub str 3 (len - 4) in
  let str = "0x"^str in
    Int64.of_string str
      
let int64_of_character str = Int64.of_int (int_of_char (str.[1]))

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
let hex_digit = digit | ['A'-'F']

let string = '"' [^'"']* '"'

let integer = digit+
let float = digit+ '.' digit+
let ull_integer = digit+ "ULL"
let hex_integer = "0x" hex_digit+
let oct_integer = "0" digit+
let identifier = letter (letter|digit)*
let character = '\'' _ '\''
let hex_character = '\'' "\\x" hex_digit hex_digit '\''

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
  | oct_integer         { INTEGER (int64_of_string "0o" (strip lexbuf 1 0)) }
  | integer             { INTEGER (int64_of_string "" (strip lexbuf 0 0)) }
  | ull_integer         { INTEGER (int64_of_string "" (strip lexbuf 0 3)) }
  | hex_integer         { INTEGER (int64_of_string "0x" (strip lexbuf 2 0)) }
  | character           { INTEGER (int64_of_character (Lexing.lexeme lexbuf)) }
  | hex_character       { INTEGER (int64_of_hex_character 
				     (Lexing.lexeme lexbuf)) }
  | "\'\\0\'"           { INTEGER Int64.zero }
  | "\'\\n\'"           { INTEGER (Int64.of_int 10) }
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
  | "|="                { OREQ }
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

  | "#" line            { preprocess lexbuf; cnt_line lexbuf; token lexbuf }

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

