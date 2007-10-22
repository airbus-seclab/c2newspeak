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

let build_fundef b v body = (FunctionDef ((b, v), body), get_loc ())::[]

let build_typedef b v = (Typedef (b, v), get_loc ())::[]

let build_decl (b, m) =
  let build (v, _) = (b, v) in
    List.map build m

let build_stmtdecl (b, m) =
  let build ((v, init), l) = (Decl ((b, v), init), l) in
    List.map build m

let build_glbdecl is_extern (b, m) = 
  let build ((v, init), _) = (GlbDecl (is_extern, (b, v), init), get_loc ()) in
    List.map build m
%}

%token BREAK CASE DEFAULT DO ELSE EXTERN IF RETURN SWITCH TYPEDEF WHILE
%token CHAR INT SHORT LONG STRUCT UNION UNSIGNED VOID
%token COLON COMMA DOT LBRACE RBRACE 
%token LBRACKET RBRACKET LPAREN RPAREN EQ EQEQ NOTEQ SEMICOLON
%token AMPERSAND AND PLUS PLUSPLUS STAR LT
%token EOF

%token <string> IDENTIFIER
%token <Int64.t> INTEGER

/*
%left LT GT EQ NE LE GE
%left PLUS MINUS
%left MULTIPLY DIVIDE
*/

%left LT EQEQ NOTEQ
%left AND
%left PLUS
%left STAR

%type <Csyntax.prog> cprog
%start cprog

%%

cprog:
  global_list                              { $1 }
;;

global_list:
  global global_list                       { $1@$2 }
|                                          { [] }
;;

global:
  declaration SEMICOLON                    { build_glbdecl false $1 }
| EXTERN declaration SEMICOLON             { build_glbdecl true $2 }
| base_typ var_modifier block              { build_fundef $1 $2 $3 }
| TYPEDEF base_typ var_modifier SEMICOLON  { build_typedef $2 $3 }
;;

var_modifier_list:
  var_modifier COMMA var_modifier_list     { ($1, get_loc ())::$3 }
| var_modifier                             { ($1, get_loc ())::[] }
;;

init_var_modifier_list:
  init_var_modifier COMMA 
  init_var_modifier_list                   { ($1, get_loc ())::$3 }
| init_var_modifier                        { ($1, get_loc ())::[] }
;;

block:
  LBRACE statement_list RBRACE             { $2 }
;;
/* TODO: simplify code by generalizing!!! 
try to remove multiple occurence of same pattern: factor as much as possible
*/
field_list:
  base_typ var_modifier_list
  SEMICOLON field_list                     { (build_decl ($1, $2))@$4 }
|                                          { [] }
;;

declaration:
  base_typ init_var_modifier_list          { ($1, $2) }
;;

statement_list:
  statement statement_list                 { $1@$2 }
|                                          { [] }
;;

statement:
  declaration SEMICOLON                    { build_stmtdecl $1 }
| left_value EQ expression SEMICOLON       { [Set ($1, $3), get_loc ()] }
| left_value PLUSPLUS SEMICOLON            { [Set ($1, 
						  Binop (Plus,
							Lval $1, 
							Const Int64.one)),
					     get_loc ()]}
| IF LPAREN expression RPAREN block
    elsif_list                             { [If (($3, $5, get_loc ())::$6), 
					     get_loc ()] }
| SWITCH LPAREN expression RPAREN LBRACE
  case_list
  RBRACE                                   { [Switch ($3, $6), get_loc ()] }
| WHILE LPAREN expression RPAREN block     { [While ($3, $5), get_loc ()] }
| DO block 
  WHILE LPAREN expression RPAREN SEMICOLON { [DoWhile ($2, $5), get_loc ()] }
| RETURN expression SEMICOLON              { [Return $2, get_loc ()] }
| IDENTIFIER 
  LPAREN expression_list RPAREN SEMICOLON  { [Exp (Call ($1, $3)), 
					     get_loc ()] }
| BREAK SEMICOLON                          { [Break, get_loc ()] }
;;

elsif_list:
  ELSE IF LPAREN expression RPAREN block
  elsif_list                               { ($4, $6, get_loc ())::$7 }
|                                          { [] }
;;

case_list:
  case case_list                           { $1::$2 }
| case                                     { $1::[] }
;;

case:
  CASE expression COLON statement_list     { (Some $2, $4, get_loc ()) }
| DEFAULT COLON statement_list             { (None, $3, get_loc ()) }
;;

expression_list:
  non_empty_expression_list                { $1 }
|                                          { [] }
;;

non_empty_expression_list:
  expression COMMA 
  non_empty_expression_list                { $1::$3 }
| expression                               { $1::[] }
;;

left_value:
  IDENTIFIER                               { Var $1 }
| left_value DOT IDENTIFIER                { Field ($1, $3) }
| left_value LBRACKET expression RBRACKET  { Index ($1, $3) }
| STAR expression                          { Deref $2 }
;;

expression:
  INTEGER                                  { Const $1 }
| left_value                               { Lval $1 }
| expression AND expression                { And ($1, $3) }
| expression PLUS expression               { Binop (Plus, $1, $3) }
| expression STAR expression               { Binop (Mult, $1, $3) }
| expression LT expression                 { Binop (Gt, $3, $1) }
| IDENTIFIER LPAREN expression_list RPAREN { Call ($1, $3) }
| AMPERSAND left_value                     { AddrOf $2 }
| LPAREN expression RPAREN                 { $2 }
| expression EQEQ expression               { Binop (Eq, $1, $3) }
| expression NOTEQ expression              { Unop (Not, Binop (Eq, $1, $3)) }
;;

// carefull not to have any empty rule: this deceives line number location
base_typ:
  VOID                                     { Void }
| ityp                                     { Integer (Signed, $1) }
| UNSIGNED ityp                            { Integer (Unsigned, $2) }
| STRUCT LBRACE field_list RBRACE          { Struct $3 }
| UNION LBRACE field_list RBRACE           { Union $3 }
| IDENTIFIER                               { Name $1 }
;;

init_var_modifier:
  var_modifier                             { ($1, None) }
| var_modifier EQ expression               { ($1, Some $3) }
;;

var_modifier:
  IDENTIFIER                               { Variable $1 }
| var_modifier LBRACKET INTEGER RBRACKET   { Array ($1, $3) }
| STAR var_modifier                        { Pointer $2 }
| var_modifier LPAREN arg_list RPAREN      { Function ($1, $3) }
| var_modifier LPAREN RPAREN               { Function ($1, []) }
| LPAREN var_modifier RPAREN               { $2 }
;;

arg_list:
  arg COMMA arg_list                       { $1::$3 }
| arg                                      { $1::[]}
;;

arg:
  base_typ var_modifier                    { ($1, $2) }
| base_typ                                 { ($1, Absent) }
;;

ityp:
  CHAR                                     { Char }
| SHORT                                    { Short }
| INT                                      { Int }
| LONG                                     { Long }
| LONG LONG                                { LongLong }
;;
