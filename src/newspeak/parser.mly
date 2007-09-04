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

let flatten_decl b m = 
  let (v, l) = m in 
    (b, v, l)

let build_fundef b v body = FunctionDef (b, v, get_loc (), body)

let build_decl b m =
  let build (v, l) = Declaration (b, v, l) in
    List.map build m

%}

%token IF RETURN
%token CHAR INT STRUCT UNION UNSIGNED VOID
%token COMMA DOT LBRACE RBRACE LBRACKET RBRACKET LPAREN RPAREN EQ SEMICOLON
%token AMPERSAND PLUS PLUSPLUS STAR LT
%token EOF

%token <string> IDENTIFIER
%token <Int64.t> INTEGER

/*
%left LT GT EQ NE LE GE
%left PLUS MINUS
%left MULTIPLY DIVIDE
*/

%left LT
%left PLUS
%left STAR

%type <Csyntax.prog> cprog
%start cprog

%%

cprog:
  declaration_list                            { $1 }
;;

declaration_list:
  declaration declaration_list                { $1@$2 }
|                                             { [] }
;;

declaration:
  base_typ var_modifier_list SEMICOLON      { build_decl $1 $2 }
| base_typ var_modifier block                 { (build_fundef $1 $2 $3)::[] }
;;

var_modifier_list:
  var_modifier COMMA var_modifier_list        { ($1, get_loc ())::$3 }
| var_modifier                                { ($1, get_loc ())::[] }
;;

block:
  LBRACE declaration_list statement_list 
  RBRACE                                      { ($2, $3) }
;;

statement_list:
  statement statement_list                    { ($1, get_loc ())::$2 }
|                                             { [] }
;;

statement:
  left_value EQ expression SEMICOLON          { Set ($1, $3) }
| left_value PLUSPLUS SEMICOLON               { Set ($1, 
						    Binop (Plus,
							  Lval $1, 
							  Const Int64.one)) }
| IF LPAREN expression RPAREN block           { If ($3, $5) }
| RETURN expression SEMICOLON                 { Return $2 }
;;

left_value:
  IDENTIFIER                                  { Var $1 }
| left_value DOT IDENTIFIER                   { Field ($1, $3) }
| left_value LBRACKET expression RBRACKET     { Index ($1, $3) }
| STAR expression                             { Deref $2 }
;;

expression:
  INTEGER                                     { Const $1 }
| left_value                                  { Lval $1 }
| expression PLUS expression                  { Binop (Plus, $1, $3) }
| expression STAR expression                  { Binop (Mult, $1, $3) }
| expression LT expression                    { Binop (Gt, $3, $1) }
| AMPERSAND left_value                        { AddrOf $2 }
;;

// carefull not to have any empty rule: this deceives line number location
base_typ:
  VOID                                        { Void }
| ityp                                        { Integer (Signed, $1) }
| UNSIGNED ityp                               { Integer (Unsigned, $2) }
| STRUCT LBRACE declaration_list RBRACE       { Struct $3 }
| UNION LBRACE declaration_list RBRACE        { Union $3 }
;;

var_modifier:
  IDENTIFIER                                  { Variable $1 }
| IDENTIFIER LPAREN RPAREN                    { FunctionName $1 }
| var_modifier LBRACKET INTEGER RBRACKET      { Array ($1, $3) }
| STAR var_modifier                           { Pointer $2 }
| var_modifier LPAREN base_typ_list RPAREN    { FunctionProto ($1, $3) }
| LPAREN var_modifier RPAREN                  { $2 }
;;

base_typ_list:
  non_empty_base_typ_list                     { $1 }
|                                             { [] }
;;

non_empty_base_typ_list:
  base_typ COMMA base_typ_list                { $1::$3 }
| base_typ                                    { $1::[] }
;;

ityp:
  CHAR                                        { Char }
| INT                                         { Int }
;;
