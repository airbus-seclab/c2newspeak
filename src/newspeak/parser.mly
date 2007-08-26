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

(*
let parse_error _ = 
*)
%}

%token VOID INT
%token LBRACE RBRACE LBRACKET RBRACKET EQ SEMICOLON

%token <string> IDENTIFIER
%token <Int64.t> INTEGER

%type <Csyntax.prog> cprog

%start cprog

%%

cprog:
  VOID IDENTIFIER LBRACKET RBRACKET block        { ($2, $5)::[] }
;;

block:
  LBRACE statement_list RBRACE                   { $2 }
;;

statement_list:
  statement statement_list                       { $1::$2 }
|                                                { [] }
;;

statement:
  declaration SEMICOLON                          { Decl $1 }
| assignment SEMICOLON                           { Set $1 }
;;

declaration:
  ctyp IDENTIFIER                                { ($2, $1) }
;;

assignment:
  left_value EQ expression                       { ($1, $3) }
;;

left_value:
  IDENTIFIER                                     { Var $1 }
;;

expression:
  INTEGER                                        { Const $1 }
;;

ctyp:
  INT                                            { Int }
;;