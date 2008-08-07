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
    let err_msg = pos^", unknown keyword: '"^lexeme^"'"
    in Npkcontext.error "Lexer.unknown_lexeme" err_msg

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
let id_is = ['i' 'I']  ['s' 'S']
let id_with = ['w' 'W']  ['i' 'I']  ['t' 'T']  ['h' 'H']
let id_package = ['p' 'P']  ['a' 'A']  ['c' 'C']  ['k' 'K']  ['a' 'A']  ['g' 'G']  ['e' 'E']
let id_body = ['b' 'B']  ['o' 'O']  ['d' 'D']  ['y' 'Y']
let id_procedure = ['p' 'P']  ['r' 'R']  ['o' 'O']  ['c' 'C']  ['e' 'E']  ['d' 'D']  ['u' 'U']  ['r' 'R']  ['e' 'E']
let id_begin = ['b' 'B']  ['e' 'E']  ['g' 'G']  ['i' 'I']  ['n' 'N']
let id_end = ['e' 'E']  ['n' 'N']  ['d' 'D']
let id_new = ['n' 'N']  ['e' 'E']  ['w' 'W']
let id_type = ['t' 'T']  ['y' 'Y']  ['p' 'P']  ['e' 'E']
let id_range = ['r' 'R']  ['a' 'A']  ['n' 'N']  ['g' 'G']  ['e' 'E']
let id_function = ['f' 'F']  ['u' 'U']  ['n' 'N']  ['c' 'C']  ['t' 'T']  ['i' 'I']  ['o' 'O']  ['n' 'N']
let id_in = ['i' 'I']  ['n' 'N']
let id_out = ['o' 'O']  ['u' 'U']  ['t' 'T']
let id_return = ['r' 'R']  ['e' 'E']  ['t' 'T']  ['u' 'U']  ['r' 'R']  ['n' 'N']
let id_if = ['i' 'I']  ['f' 'F']
let id_then = ['t' 'T']  ['h' 'H']  ['e' 'E']  ['n' 'N']
let id_else = ['e' 'E']  ['l' 'L']  ['s' 'S']  ['e' 'E']
let id_elsif = ['e' 'E']  ['l' 'L']  ['s' 'S']  ['i' 'I']  ['f' 'F']
let id_loop = ['l' 'L']  ['o' 'O']  ['o' 'O']  ['p' 'P']
let id_and = ['a' 'A']  ['n' 'N']  ['d' 'D']
let id_or = ['o' 'O']  ['r' 'R']
let id_xor = ['x' 'X']  ['o' 'O']  ['r' 'R']
let id_use = ['u' 'U'] ['s' 'S'] ['e' 'E']
let id_null = ['n' 'N']  ['u' 'U']  ['l' 'L']  ['l' 'L']

let id_not = ['n' 'N']  ['o' 'O']  ['t' 'T']
let id_mod = ['m' 'M']  ['o' 'O']  ['d' 'D']
let id_rem = ['r' 'R']  ['e' 'E']  ['m' 'M']
let id_abs = ['a' 'A']  ['b' 'B']  ['s' 'S']
let id_while = ['w' 'W']  ['h' 'H']  ['i' 'I']  ['l' 'L']  ['e' 'E']
let id_for = ['f' 'F']  ['o' 'O']  ['r' 'R']
let id_exit = ['e' 'E']  ['x' 'X']  ['i' 'I']  ['t' 'T']
let id_when = ['w' 'W']  ['h' 'H']  ['e' 'E']  ['n' 'N']
let id_package = ['p' 'P']  ['a' 'A']  ['c' 'C']  ['k' 'K']  ['a' 'A']  ['g' 'G']  ['e' 'E']
let id_body = ['b' 'B']  ['o' 'O']  ['d' 'D']  ['y' 'Y']
let id_constant = ['c' 'C']  ['o' 'O']  ['n' 'N']  ['s' 'S']  ['t' 'T']  ['a' 'A']  ['n' 'N']  ['t' 'T']
let id_subtype = ['s' 'S']  ['u' 'U']  ['b' 'B']  ['t' 'T']  ['y' 'Y']  ['p' 'P']  ['e' 'E']


let id_integer = ['i' 'I']  ['n' 'N']  ['t' 'T']  ['e' 'E']  ['g' 'G']  ['e' 'E']  ['r' 'R']
let id_float = ['f' 'F']  ['l' 'L']  ['o' 'O']  ['a' 'A']  ['t' 'T']
let id_boolean = ['b' 'B']  ['o' 'O']  ['o' 'O']  ['l' 'L']  ['e' 'E']  ['a' 'A']  ['n' 'N']
let id_true = ['t' 'T']  ['r' 'R']  ['u' 'U']  ['e' 'E']
let id_false = ['f' 'F']  ['a' 'A']  ['l' 'L']  ['s' 'S']  ['e' 'E']
let id_character = ['c' 'C']  ['h' 'H']  ['a' 'A']  ['r' 'R']  ['a' 'A']  ['c' 'C']  ['t' 'T']  ['e' 'E']  ['r' 'R']


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

  (* identifiants non réservés mais considérés comme tels*)
  | id_integer {INTEGER}
  | id_float {FLOAT}
  | id_boolean {BOOLEAN}
  | id_character {CHARACTER}
  | id_true {TRUE}
  | id_false {FALSE}


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
  | ".." {DEUX_POINTS_H}
  | ',' {VIR}
  | "'" {QUOTE}

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
      

  
