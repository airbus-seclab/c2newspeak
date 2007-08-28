/*
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
*/

%{
open Csyntax
open Lexing

let get_loc () =
  let pos = symbol_start_pos () in
    (pos.pos_fname, pos.pos_lnum, pos.pos_cnum)

let append_decl (x, t) body = (Decl ((x, t), body), get_loc ())::[]

%}

%token CHAR INT STRUCT UNION UNSIGNED VOID
%token COMMA LBRACE RBRACE LBRACKET RBRACKET LPAREN RPAREN EQ SEMICOLON STAR

%token <string> IDENTIFIER
%token <Int64.t> INTEGER


%left STAR
%left LPAREN
%left LBRACKET

%type <Csyntax.prog> cprog
%start cprog

%%

cprog:
  VOID IDENTIFIER LPAREN RPAREN block          { ($2, $5)::[] }
;;

block:
  LBRACE statement_list RBRACE                 { $2 }
;;

statement_list:
  assignment statement_list                    { ($1, get_loc ())::$2 }
| declaration statement_list                   { append_decl $1 $2 } 
|                                              { [] }
;;

declaration:
  base_typ var_modifier SEMICOLON              { ($1, $2) }
;;

assignment:
  left_value EQ expression SEMICOLON           { Set ($1, $3) }
;;

left_value:
  IDENTIFIER                                   { Var $1 }
;;

expression:
  INTEGER                                      { Const $1 }
;;

// carefull not to have any empty rule: this deceives line number location
base_typ:
  ityp                                         { Integer (Signed, $1) }
| UNSIGNED ityp                                { Integer (Unsigned, $2) }
| STRUCT LBRACE declaration_list RBRACE        { Struct $3 }
| UNION LBRACE declaration_list RBRACE         { Union $3 }
;;

declaration_list:
  declaration declaration_list                 { $1::$2 }
| declaration                                  { $1::[] }
;;

var_modifier:
  IDENTIFIER                                   { Variable $1 }
| var_modifier LBRACKET INTEGER RBRACKET       { Array ($1, $3) }
| STAR var_modifier                            { Pointer $2 }
| var_modifier LPAREN base_typ_list RPAREN     { Function ($1, $3) }
| LPAREN var_modifier RPAREN                   { $2 }
;;

base_typ_list:
  non_empty_base_typ_list                      { $1 }
|                                              { [] }
;;

non_empty_base_typ_list:
  base_typ COMMA base_typ_list                 { $1::$3 }
| base_typ                                     { $1::[] }
;;

ityp:
  CHAR                                         { Char }
| INT                                          { Int }
;;
