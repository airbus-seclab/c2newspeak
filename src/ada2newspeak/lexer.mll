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

  Jasmine Duchon
  email : jasmine . duchon AT free . fr
  
*)


{
  open Parser
  open Lexing
 
   
  (* supprime les guillemets qui entourent une chaîne *)
  let extrait_chaine s = String.sub s 1 (String.length s - 2)
  
  (* renvoie le code ascii du caractère en position 1*)
  let extrait_char s = 
    int_of_char (String.get s 1)

  let set_loc lexbuf pos = 
    lexbuf.lex_curr_p <- pos;
    Npkcontext.set_loc (pos.pos_fname, pos.pos_lnum, pos.pos_cnum)
  
  let newline lexbuf =
    let pos = lexbuf.lex_curr_p 
    in
    let new_pos = { pos with pos_lnum = pos.pos_lnum + 1; 
	  pos_bol = pos.pos_cnum };
    in set_loc lexbuf new_pos

  let init fname lexbuf = 
    let pos = { lexbuf.lex_curr_p with pos_fname = fname } in
      set_loc lexbuf pos

  let unknown_lexeme lexbuf =
    let start_pos = Lexing.lexeme_start_p lexbuf 
    and end_pos = Lexing.lexeme_end_p lexbuf 
    in
    let line = string_of_int start_pos.pos_lnum 
    and lexeme = Lexing.lexeme lexbuf 
    and start_col = start_pos.pos_cnum - start_pos.pos_bol
    and end_col = end_pos.pos_cnum - end_pos.pos_bol
    in
    let pos = "line "^line^", col "^(string_of_int start_col)
      ^(if start_col = end_col
	then ""
	else ("-"^(string_of_int end_col)))
    in
    let err_msg = pos^", unknown keyword: '"^lexeme^"'" in 
      Npkcontext.report_error "Lexer.unknown_lexeme" err_msg

}
(*à élargir : accent *)
 
let lettre = ['a'-'z' 'A'-'Z']
let chiffre = ['0'-'9']
let alphanum = lettre | chiffre
let ident = lettre ('_'? alphanum)*
let blanc = [' ' '\t']
let char = "'"_"'"
let chaine = '"' ([^ '"']|"""")* '"'

(*Nombres*)
(* à revoir : intégrer puissance et base *)
let entier = chiffre ('_'? chiffre)*
let reel = entier '.' entier
    
let litteral_entier = entier
let litteral_reel = reel
      
(*commentaires*)
let commentaire = "--" [^ '\n']*

(*identificateurs prédifinis*)
let id_is = "is"
let id_with = "with"
let id_package = "package"
let id_body = "body"
let id_procedure = "procedure"
let id_begin = "begin"
let id_end = "end"
let id_new = "new"
let id_type = "type"
let id_range = "range"
let id_function = "function"
let id_in = "in"
let id_out = "out"
let id_return = "return"
let id_if = "if"
let id_then = "then"
let id_else = "else"
let id_elsif = "elsif"
let id_loop = "loop"
let id_and = "and"
let id_or = "or"
let id_xor = "xor"
let id_use = "use"
let id_null = "null"
let id_not = "not"
let id_mod = "mod"
let id_rem = "rem"
let id_abs = "abs"
let id_while = "while"
let id_for = "for"
let id_exit = "exit"
let id_when = "when"
let id_package = "package"
let id_body = "body"
let id_constant = "constant"
let id_subtype = "subtype"
let id_array = "array"
let id_record = "record"
let id_of = "of"
let id_integer = "integer"
let id_float = "float"
let id_boolean = "boolean"
let id_character = "character"
let id_true = "true"
let id_false = "false"
(*WG*)
let id_last = "last"
let id_first = "first"
let id_length = "length"

rule token = parse
    
  (* identifiants composés *) 
  | id_and blanc* id_then {ANDTHEN}
  | id_or blanc* id_else {ORELSE}

  
  (*reconnaissance des identifiants réservés*)
  | id_is {IS}
  | id_with {WITH}
  | id_package {PACKAGE}
  | id_body {BODY}
  | id_procedure {PROCEDURE}
  | id_begin {BEGIN}
  | id_end {END}
  | id_new {NEW}
  | id_type {TYPE}
  | id_range {RANGE}
  | id_function {FUNCTION}
  | id_in {IN}
  | id_out {OUT}
  | id_return {RETURN}
  | id_if {IF}
  | id_then {THEN}
  | id_else {ELSE}
  | id_elsif {ELSIF}
  | id_loop {LOOP}
  | id_and {AND}
  | id_or {OR}
  | id_xor {XOR}
  | id_use {USE}
  | id_null {NULL}
  | id_not {NOT}
  | id_mod {MOD}
  | id_rem {REM}
  | id_abs {ABS}
  | id_while {WHILE}
  | id_for {FOR}
  | id_exit {EXIT}
  | id_when {WHEN}
  | id_package {PACKAGE}
  | id_body {BODY}
  | id_constant {CONSTANT}
  | id_subtype {SUBTYPE}
  | id_array {ARRAY}
  | id_record {RECORD}
  | id_of {OF}
  (* identifiants non réservés mais considérés comme tels*)
  | id_integer {INTEGER}
  | id_float {FLOAT}
  | id_boolean {BOOLEAN}
  | id_character {CHARACTER}
  | id_true {TRUE}
  | id_false {FALSE}

(*WG*)
  | id_last {LAST}
  | id_first {FIRST}
  | id_length {LENGTH}
  | '(' {PAR_G}
  | ')' {PAR_D}

  (* opérateurs arithmétiques *)
  | '+' {PLUS}
  | '-' {MOINS}
  | '*' {FOIS}
  | '/' {DIV}
  | "**" {PUISS}
  | '&' {CONCAT}

  (* opérateurs relationnels *)
  | "<=" {LE}
  | '<' {LT}
  | ">=" {GE}
  | '>' {GT}
  | "=" {EQ}
  | "/=" {NE}
  | "=" {EQ}

  | ":=" {AFFECT}

  (* ponctuation *)
  | ';' {POINT_VIR}
  | '.' {POINT}
  | ':' {DEUX_POINTS}
  | ".." {H_DEUX_POINTS}
  | ',' {VIR}
  | "'" {QUOTE}
  | "=>" {FLECHE}


  | '\n' {newline lexbuf; token lexbuf}

  (* caractères ignorés*)
  | blanc {token lexbuf}
  | commentaire {token lexbuf}

  (* caractères, chaines de caractères *)
  | chaine {STRING (extrait_chaine (Lexing.lexeme lexbuf)) }
  | char {CONST_CHAR (extrait_char (Lexing.lexeme lexbuf)) }

  (* constantes numériques *)
  | litteral_reel {CONST_FLOAT (Lexing.lexeme lexbuf)}
  | litteral_entier {CONST_INT (Lexing.lexeme lexbuf)}

  (*identifiant*)
  | ident {IDENT (Lexing.lexeme lexbuf)}
  | eof { EOF }

  | _ {unknown_lexeme lexbuf}
      

  
