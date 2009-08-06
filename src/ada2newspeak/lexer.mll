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

  let loc_here lexbuf =
    (lexbuf.lex_curr_p.pos_fname
    ,lexbuf.lex_curr_p.pos_lnum
    ,lexbuf.lex_curr_p.pos_cnum
    -lexbuf.lex_curr_p.pos_bol
  )

  let set_loc lexbuf pos =
    lexbuf.lex_curr_p <- pos;
    Npkcontext.set_loc (loc_here lexbuf)

  let newline lexbuf =
    let pos =
      { lexbuf.lex_curr_p with
            pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1;
            pos_bol  = lexbuf.lex_curr_p.pos_cnum;
      }
    in
    set_loc lexbuf pos

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

  (*
   * Read a "word" and translate it into a keyword or,
   * as a fallback solution, an identifier.
   *)
  let lex_word (w:string) (l:Newspeak.location) :token =
    let lw = String.lowercase w in
    match lw with
    | "abs"        -> ABS       l | "access"     -> ACCESS    l
    | "all"        -> ALL       l | "and"        -> AND       l
    | "array"      -> ARRAY     l | "at"         -> AT        l
    | "begin"      -> BEGIN     l | "body"       -> BODY      l
    | "case"       -> CASE      l | "constant"   -> CONSTANT  l
    | "declare"    -> DECLARE   l | "digits"     -> DIGITS    l
    | "elsif"      -> ELSIF     l | "else"       -> ELSE      l
    | "end"        -> END       l | "exit"       -> EXIT      l
    | "for"        -> FOR       l | "function"   -> FUNCTION  l
    | "if"         -> IF        l | "in"         -> IN        l
    | "is"         -> IS        l | "loop"       -> LOOP      l
    | "mod"        -> MOD       l | "new"        -> NEW       l
    | "not"        -> NOT       l | "null"       -> NULL      l
    | "of"         -> OF        l | "or"         -> OR        l
    | "others"     -> OTHERS    l | "out"        -> OUT       l
    | "package"    -> PACKAGE   l | "pragma"     -> PRAGMA    l
    | "procedure"  -> PROCEDURE l | "range"      -> RANGE     l
    | "record"     -> RECORD    l | "rem"        -> REM       l
    | "renames"    -> RENAMES   l | "return"     -> RETURN    l
    | "reverse"    -> REVERSE   l | "subtype"    -> SUBTYPE   l
    | "then"       -> THEN      l | "type"       -> TYPE      l
    | "use"        -> USE       l | "when"       -> WHEN      l
    | "while"      -> WHILE     l | "with"       -> WITH      l
    | "xor"        -> XOR       l
    |_             -> IDENT (l,lw)

(* Unrecognized tokens *)

    (* Task-related *)
        (* abort        *)
        (* accept       *)
        (* delay        *)
        (* entry        *)
        (* requeue      *) (* Ada95 *)
        (* select       *)
        (* task         *)
        (* terminate    *)
        (* do           *)
        (* until        *) (* Ada95 *)

    (* Compilation-related  *)
        (* abstract     *) (* Ada95 *)
        (* generic      *)
        (* separate     *)

    (* Type-related  *)
        (* tagged       *) (* Ada95 *)
        (* delta        *)
        (* limited      *)
        (* private      *)

    (* Pointer-related  *)
        (* access       *)
        (* aliased      *) (* Ada95 *)
        (* all          *)

    (* Execution flow-related  *)
        (* goto         *)
        (* raise        *)
        (* exception    *)

    (* Misc *)
        (* protected    *) (* Ada95 *)


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
let real = entier '.' entier

let extended_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let based_numeral = extended_digit ('_'? extended_digit)*

(*commentaires*)
let commentaire = "--" [^ '\n']*

rule token = parse
  | '('  {LPAR       (loc_here lexbuf)}
  | ')'  {RPAR       (loc_here lexbuf)}
  | '+'  {PLUS       (loc_here lexbuf)}
  | '-'  {MINUS      (loc_here lexbuf)}
  | '*'  {MULT       (loc_here lexbuf)}
  | '/'  {DIV        (loc_here lexbuf)}
  | "**" {POW        (loc_here lexbuf)}
  | "<=" {LE         (loc_here lexbuf)}
  | '<'  {LT         (loc_here lexbuf)}
  | ">=" {GE         (loc_here lexbuf)}
  | '>'  {GT         (loc_here lexbuf)}
  | "="  {EQ         (loc_here lexbuf)}
  | "/=" {NE         (loc_here lexbuf)}
  | "="  {EQ         (loc_here lexbuf)}
  | ":=" {ASSIGN     (loc_here lexbuf)}
  | ';'  {SEMICOLON  (loc_here lexbuf)}
  | '.'  {DOT        (loc_here lexbuf)}
  | ':'  {COLON      (loc_here lexbuf)}
  | ".." {DOUBLE_DOT (loc_here lexbuf)}
  | ','  {COMMA      (loc_here lexbuf)}
  | "'"  {QUOTE      (loc_here lexbuf)}
  | "=>" {ARROW      (loc_here lexbuf)}
  | "|"  {VBAR       (loc_here lexbuf)}

  | '\n' {newline lexbuf; token lexbuf}

  (* caracteres ignores*)
  | blanc {token lexbuf}
  | commentaire {token lexbuf}

  (* caracteres, chaines de caracteres *)
  | chaine {CONST_STRING (loc_here lexbuf
                         ,extrait_chaine (Lexing.lexeme lexbuf))}
  | char   {CONST_CHAR   (loc_here lexbuf
                         ,int_of_char (Lexing.lexeme lexbuf).[1]) }

  (* constantes numeriques *)
  | real   {CONST_FLOAT (loc_here lexbuf,Lexing.lexeme lexbuf)}
  | real ['e' 'E'] (['+''-']?) entier
          {CONST_FLOAT (loc_here lexbuf,Lexing.lexeme lexbuf)}
  | entier {CONST_INT (loc_here lexbuf,Newspeak.Nat.of_string(
                                strip_underscores (Lexing.lexeme lexbuf)))}
  | entier as main_part ['e' 'E'] '+'? (entier as expo)
      {CONST_INT (loc_here lexbuf,
            Newspeak.Nat.of_int( (int_of_string (main_part))
                    * (expt_int 10 (int_of_string (expo)))
            ))
      }
  | entier as base '#' (based_numeral as main_part) '#' (entier as exponent)? {
                CONST_INT (loc_here lexbuf
                          ,Newspeak.Nat.of_int ( (int_of_based_string
                                        (int_of_string (base))
                                            (strip_underscores main_part))
                           * expt_int (int_of_string (base))
                                            (int_of_string (
                                                (Ada_utils.with_default
                                                            exponent "0")))))}

  (*identifiant*)
  | ident {lex_word (Lexing.lexeme lexbuf) (loc_here lexbuf)}
  | eof { EOF }

  | _ {unknown_lexeme lexbuf}

