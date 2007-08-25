{
open Parser

let line_cnt = ref 1

let unknown_lexeme lexeme =
  let line = string_of_int (!line_cnt) in
    "Lexer.mlll: line: "^line^", unknown keyword: "^lexeme
}

let digit = ['0'-'9']

let integer = digit+

rule token = parse

(* keywords *)
(* types *)
    "void"              { VOID }

(* punctuation *)
  | "{"                 { LBRACE }
  | "}"                 { RBRACE }
(* error fallback *)
  | _                   { invalid_arg (unknown_lexeme (Lexing.lexeme lexbuf)) }
