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


  (* supprime les guillemets qui entourent une chaine *)
  let extrait_chaine s = String.sub s 1 (String.length s - 2)

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

    let rec expt_int pow x = match x with
        | y when y<0 -> failwith ("Internal error : expt_int should be called"
                                ^ " only with nonnegative integers");
        | 0 -> 1
        | _ -> pow * expt_int pow (x-1)

    (* Removes underscores from a string. *)
    let strip_underscores s =
        Str.global_replace (Str.regexp_string "_") "" s

    (* Computes the value of a based numeral *)
    let int_of_based_string base str =
        if (base < 2 || base > 16) then
            Npkcontext.report_error "Lexer.int_of_based_string"
                                    "A base b should be 2 <= b <= 16"
        else begin
            let value_of_char c = match c with
                    | '0' ->  0 | '1' ->  1 | '2' ->  2 | '3' ->  3
                    | '4' ->  4 | '5' ->  5 | '6' ->  6 | '7' ->  7
                    | '8' ->  8 | '9' ->  9 | 'a' -> 10 | 'b' -> 11
                    | 'c' -> 12 | 'd' -> 13 | 'e' -> 14 | 'f' -> 15
                    | 'A' -> 10 | 'B' -> 11 | 'C' -> 12 | 'D' -> 13
                    | 'E' -> 14 | 'F' -> 15
                    | _ -> invalid_arg ("Internal error : input '"
                                        ^(String.make 1 c)^"' in value_of_char"
                                        ^" (based litterals)")
            in
            let rec aux start acc =
                if start = (String.length str) then acc
                else begin
                    let value = value_of_char str.[start] in
                        if value >= base then
                            Npkcontext.report_error "Lexer.int_of_based_string"
                                "In base X, digits should be < X"
                        else aux (start+1) (base * acc + value)
                end
            in
            aux 0 0
        end

}
(*a elargir : accent *)

let lettre = ['a'-'z' 'A'-'Z']
let chiffre = ['0'-'9']
let alphanum = lettre | chiffre
let ident = lettre ('_'? alphanum)*
let blanc = [' ' '\t']
let char = "'"_"'"
let chaine = '"' ([^ '"']|"""")* '"'

(*Nombres*)
let entier = chiffre ('_'? chiffre)*
let reel = entier '.' entier

let extended_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let based_numeral = extended_digit ('_'? extended_digit)*

let litteral_reel = reel

(*commentaires*)
let commentaire = "--" [^ '\n']*

(*identificateurs predefinis*)
let id_integer   = ['i''I']['n''N']['t''T']['e''E']['g''G']['e''E']['r''R']
let id_float     = ['f''F']['l''L']['o''O']['a''A']['t''T']
let id_boolean   = ['b''B']['o''O']['o''O']['l''L']['e''E']['a''A']['n''N']
let id_character = ['c''C']['h''H']['a''A']['r''R']
                           ['a''A']['c''C']['t''T']['e''E']['r''R']
let id_true      = ['t''T']['r''R']['u''U']['e''E']
let id_false     = ['f''F']['a''A']['l''L']['s''S']['e''E']

rule token = parse

    (* Reserved words. As there is no way to specify ocamllex to make a single
       rule case-insensitive, the alternative should be made explicit.
       Rationale : RM95, 2.9.(2) *)

    (* Perl generator : filter through s/([a-z])/"['$1''".uc $1."']"/eg;
                        and remove quotes. *)


  | ['a''A']['b''B']['s''S']                      {ABS}
  | ['a''A']['n''N']['d''D']                      {AND}
  | ['a''A']['r''R']['r''R']['a''A']['y''Y']      {ARRAY}
  | ['b''B']['e''E']['g''G']['i''I']['n''N']      {BEGIN}
  | ['b''B']['o''O']['d''D']['y''Y']              {BODY}
  | ['c''C']['a''A']['s''S']['e''E']              {CASE}
  | ['c''C']['o''O']['n''N']['s''S']
            ['t''T']['a''A']['n''N']['t''T']      {CONSTANT}

  | ['d''D']['e''E']['c''C']['l''L']['a''A']
            ['r''R']['e''E']                      {DECLARE}
  | ['e''E']['l''L']['s''S']['i''I']['f''F']      {ELSIF}
  | ['e''E']['l''L']['s''S']['e''E']              {ELSE}
  | ['e''E']['n''N']['d''D']                      {END}
  | ['e''E']['x''X']['i''I']['t''T']              {EXIT}
  | ['f''F']['o''O']['r''R']                      {FOR}
  | ['f''F']['u''U']['n''N']['c''C']['t''T']
            ['i''I']['o''O']['n''N']              {FUNCTION}
  | ['i''I']['f''F']                              {IF}
  | ['i''I']['n''N']                              {IN}
  | ['i''I']['s''S']                              {IS}
  | ['l''L']['o''O']['o''O']['p''P']              {LOOP}
  | ['m''M']['o''O']['d''D']                      {MOD}
  | ['n''N']['e''E']['w''W']                      {NEW}
  | ['n''N']['o''O']['t''T']                      {NOT}
  | ['n''N']['u''U']['l''L']['l''L']              {NULL}
  | ['o''O']['f''F']                              {OF}
  | ['o''O']['r''R']                              {OR}
  | ['o''O']['t''T']['h''H']['e''E']['r''R']
            ['s''S']                              {OTHERS}
  | ['o''O']['u''U']['t''T']                      {OUT}
  | ['p''P']['a''A']['c''C']['k''K']['a''A']
            ['g''G']['e''E']                      {PACKAGE}
  | ['p''P']['r''R']['a''A']['g''G']['m''M']
            ['a''A']                              {PRAGMA}
  | ['p''P']['r''R']['o''O']['c''C']['e''E']
            ['d''D']['u''U']['r''R']['e''E']      {PROCEDURE}
  | ['r''R']['a''A']['n''N']['g''G']['e''E']      {RANGE}
  | ['r''R']['e''E']['c''C']['o''O']['r''R']
            ['d''D']                              {RECORD}
  | ['r''R']['e''E']['m''M']                      {REM}
  | ['r''R']['e''E']['t''T']['u''U']['r''R']
            ['n''N']                              {RETURN}
  | ['r''R']['e''E']['v''V']['e''E']['r''R']
            ['s''S']['e''E']                      {REVERSE}
  | ['s''S']['u''U']['b''B']['t''T']['y''Y']
            ['p''P']['e''E']                      {SUBTYPE}
  | ['t''T']['h''H']['e''E']['n''N']              {THEN}
  | ['t''T']['y''Y']['p''P']['e''E']              {TYPE}
  | ['u''U']['s''S']['e''E']                      {USE}
  | ['w''W']['h''H']['e''E']['n''N']              {WHEN}
  | ['w''W']['h''H']['i''I']['l''L']['e''E']      {WHILE}
  | ['w''W']['i''I']['t''T']['h''H']              {WITH}
  | ['x''X']['o''O']['r''R']                      {XOR}

(* Unrecognized tokens *)

    (* Task-related *)
        (* abort        *)
        (* accept       *)
        (* delay        *)
        (* entry        *)
        (* requeue      *)
        (* select       *)
        (* task         *)
        (* terminate    *)
        (* do           *)
        (* until        *)

    (* Compilation-related  *)
        (* abstract     *)
        (* generic      *)
        (* renames      *)
        (* separate     *)

    (* Type-related  *)
        (* at           *)
        (* tagged       *)
        (* delta        *)
        (* digits       *)
        (* limited      *)
        (* private      *)

    (* Pointer-related  *)
        (* access       *)
        (* aliased      *)
        (* all          *)

    (* Execution flow-related  *)
        (* goto         *)
        (* raise        *)
        (* exception    *)

    (* Misc *)
        (* protected    *)


  | "or"  blanc+ "else" {ORELSE}
  | "and" blanc+ "then" {ANDTHEN}


  (* identifiants non reserves mais consideres comme tels*)
  | id_integer     {INTEGER}
  | id_float       {FLOAT}
  | id_boolean     {BOOLEAN}
  | id_character   {CHARACTER}
  | id_true        {TRUE}
  | id_false       {FALSE}

  | '('            {LPAR}
  | ')'            {RPAR}

  (* operateurs arithmetiques *)
  | '+'            {PLUS}
  | '-'            {MINUS}
  | '*'            {MULT}
  | '/'            {DIV}
  | "**"           {POW}
  | '&'            {CONCAT}

  (* operateurs relationnels *)
  | "<="           {LE}
  | '<'            {LT}
  | ">="           {GE}
  | '>'            {GT}
  | "="            {EQ}
  | "/="           {NE}
  | "="            {EQ}

  | ":="           {ASSIGN}

  (* ponctuation *)
  | ';'            {SEMICOLON}
  | '.'            {DOT}
  | ':'            {COLON}
  | ".."           {DOUBLE_DOT}
  | ','            {COMMA}
  | "'"            {QUOTE}
  | "=>"           {ARROW}
  | "|"            {VBAR}


  | '\n' {newline lexbuf; token lexbuf}

  (* caracteres ignores*)
  | blanc {token lexbuf}
  | commentaire {token lexbuf}

  (* caracteres, chaines de caracteres *)
  | chaine {STRING (extrait_chaine (Lexing.lexeme lexbuf)) }
  | char {CONST_CHAR (int_of_char (Lexing.lexeme lexbuf).[1]) }

  (* constantes numeriques *)
  | litteral_reel {CONST_FLOAT (Lexing.lexeme lexbuf)}
  | entier {CONST_INT (Newspeak.Nat.of_string(
                                strip_underscores (Lexing.lexeme lexbuf)))}
  | entier as main_part ['e' 'E'] '+'? (entier as expo) {CONST_INT (
            Newspeak.Nat.of_int( (int_of_string (strip_underscores main_part))
                    * (expt_int 10 (int_of_string (strip_underscores expo)))
            ))}
  | entier as base '#' (based_numeral as main_part) '#' (entier as exponent)? {
                CONST_INT (Newspeak.Nat.of_int ( (int_of_based_string
                                        (int_of_string (strip_underscores base))
                                            (strip_underscores main_part))
                           * expt_int (int_of_string (strip_underscores base))
                                            (int_of_string (strip_underscores
                                                (Ada_utils.with_default
                                                            exponent "0")))))}

  (*identifiant*)
  | ident {IDENT (String.lowercase (Lexing.lexeme lexbuf))}
  | eof { EOF }

  | _ {unknown_lexeme lexbuf}



