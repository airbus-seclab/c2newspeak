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
open Bare_csyntax
open Lexing
open Synthack

let struct_cnt = ref 0

let gen_struct_id () = 
  incr struct_cnt;
  "anon_struct"^(string_of_int !struct_cnt)
  
(* TODO: write checks for all the syntax that is thrown away in these functions
   !! *)

let get_loc () =
  let pos = symbol_start_pos () in
    (pos.pos_fname, pos.pos_lnum, pos.pos_cnum)

let process_decls f (b, m) =
  let b = Synthack.normalize_base_typ b in
  let process (v, init) =
    let (t, x, loc) = Synthack.normalize_var_modifier b v in
      f (t, x, init, loc)
  in
    List.map process m

let build_glbdecl is_extern d = 
  let build (t, x, init, loc) = (GlbDecl (x, t, is_extern, init), loc) in
    process_decls build d

let build_fundef d body = 
  let (t, x, loc) = Synthack.normalize_decl d in
    (FunctionDef (x, t, body), loc)
      
let build_typedef d =
  let build (t, x, _, _) = Synthack.define_type x t in
  let _ = process_decls build d in
    []

let build_field (b, m) =
  let build ((v, _), _) = 
    let (t, x, _) = Synthack.normalize_decl (b, v) in
      (t, x)
  in
    List.map build m

let build_stmtdecl d =
  let build (t, x, init, loc) = (Decl (x, t, init), loc) in
    process_decls build d

let build_type_decl d =
  let (t, _, _) = Synthack.normalize_decl d in
    t

%}

%token BREAK CONST CONTINUE CASE DEFAULT DO ELSE EXTERN FOR IF RETURN SIZEOF 
%token SWITCH TYPEDEF WHILE
%token CHAR INT SHORT LONG STRUCT UNION UNSIGNED VOID
%token COLON COMMA DOT LBRACE RBRACE 
%token LBRACKET RBRACKET LPAREN RPAREN EQ EQEQ NOTEQ SEMICOLON
%token AMPERSAND AND MINUS PLUS PLUSPLUS STAR LT GT
%token EOF

%token <string> IDENTIFIER
%token <string> TYPEDEF_NAME
%token <string> STRING
%token <Int64.t> INTEGER

/*
%left LT GT EQ NE LE GE
%left PLUS MINUS
%left MULTIPLY DIVIDE
*/

%left AND
%left LT GT EQEQ NOTEQ
%left PLUS MINUS
%left STAR

%type <Bare_csyntax.prog> parse
%start parse

%%
/* TODO: simplify code by generalizing!!! 
try to remove multiple occurence of same pattern: factor as much as possible
*/
// carefull not to have any empty rule: this deceives line number location

parse:
  translation_unit                         { let prog = $1 in 
					       Synthack.clean (); prog }
;;

translation_unit:
  external_declaration translation_unit    { $1@$2 }
|                                          { [] }
;;

external_declaration:
  declaration                              { build_glbdecl false $1 }
| EXTERN declaration                       { build_glbdecl true $2 }
| declaration_specifiers declarator 
  statement                                { (build_fundef ($1, $2) $3)::[] }
| TYPEDEF declaration                      { build_typedef $2 }
;;

declaration:
  declaration_specifiers init_declarator_list 
  SEMICOLON                                { ($1, $2) }
| declaration_specifiers SEMICOLON         { ($1, []) }
;;

field_declaration:
  declaration_specifiers declarator        { ($1, $2) }
;;

// TODO: careful, this is a bit of a hack
parameter_declaration:
  declaration_specifiers declarator        { ($1, $2) }
| declaration_specifiers 
  abstract_declarator                      { ($1, $2) }
| declaration_specifiers                   { ($1, Abstract) }
;;

type_declaration:
  declaration_specifiers 
  abstract_declarator                      { ($1, $2) }
| declaration_specifiers                   { ($1, Abstract) }
;;


declaration_specifiers:
  type_qualifier_list type_specifier 
  type_qualifier_list                      { $2 }
;;

type_qualifier_list:
  type_qualifier type_qualifier_list       { }
|                                          { }
;;

type_qualifier:
  CONST                                    { }
;;

init_declarator_list:
  init_declarator COMMA 
  init_declarator_list                     { $1::$3 }
| init_declarator                          { $1::[] }
;;

block:
  LBRACE statement_list RBRACE             { $2 }
;;

statement_list:
  statement statement_list                 { $1@$2 }
|                                          { [] }
;;

statement:
  declaration                              { build_stmtdecl $1 }
| assignment SEMICOLON                     { [Set $1, get_loc ()] }
| IF LPAREN expression RPAREN statement    { [If ($3, $5, []), get_loc ()] }
| IF LPAREN expression RPAREN statement
  ELSE statement                           { [If ($3, $5, $7), get_loc ()] }
| SWITCH LPAREN expression RPAREN LBRACE
  case_list
  RBRACE                                   { [Switch ($3, $6), get_loc ()] }
| FOR LPAREN assignment SEMICOLON 
      expression SEMICOLON 
      assignment RPAREN
      statement                            { let loc = get_loc () in
                                               (For ((Set $3, loc)::[], 
						    $5, $9, 
						    (Set $7, loc)::[]), loc)
					       ::[] }
| WHILE LPAREN expression RPAREN statement { [While ($3, $5), get_loc ()] }
| DO statement
  WHILE LPAREN expression RPAREN SEMICOLON { [DoWhile ($2, $5), get_loc ()] }
| RETURN expression SEMICOLON              { [Return (Some $2), get_loc ()] }
| RETURN SEMICOLON                         { [Return None, get_loc ()] }
| call SEMICOLON                           { [Exp $1, get_loc ()] }
| BREAK SEMICOLON                          { [Break, get_loc ()] }
| CONTINUE SEMICOLON                       { [Continue, get_loc ()] }
| block                                    { [Block $1, get_loc ()] }
;;

assignment:
  left_value EQ expression                 { ($1, $3) }
| left_value PLUSPLUS                      { ($1, Binop (Plus, Lval $1, 
							Cst Int64.one)) }
;;

case_list:
  case case_list                           { $1::$2 }
| case                                     { $1::[] }
;;

case:
  CASE expression COLON statement_list     { (Some $2, $4, get_loc ()) }
| DEFAULT COLON statement_list             { (None, $3, get_loc ()) }
;;

call:
| left_value LPAREN RPAREN                 { Call ($1, []) }
| left_value LPAREN expression_list RPAREN { Call ($1, $3) }
;;

expression_list:
  expression COMMA expression_list         { $1::$3 }
| expression                               { $1::[] }
;;

left_value:
  IDENTIFIER                               { Var $1 }
| left_value DOT IDENTIFIER                { Field ($1, $3) }
| left_value LBRACKET expression RBRACKET  { Index ($1, $3) }
| STAR expression                          { Deref $2 }
;;

expression:
  INTEGER                                  { Cst $1 }
| MINUS expression                         { negate $2 }
| left_value                               { Lval $1 }
| expression AND expression                { And ($1, $3) }
| expression PLUS expression               { Binop (Plus, $1, $3) }
| expression MINUS expression              { Binop (Minus, $1, $3) }
| expression STAR expression               { Binop (Mult, $1, $3) }
| expression GT expression                 { Binop (Gt, $1, $3) }
| expression LT expression                 { Binop (Gt, $3, $1) }
| call                                     { $1 }
| AMPERSAND left_value                     { AddrOf $2 }
| LPAREN expression RPAREN                 { $2 }
| expression EQEQ expression               { Binop (Eq, $1, $3) }
| expression NOTEQ expression              { Unop (Not, Binop (Eq, $1, $3)) }
| SIZEOF LPAREN IDENTIFIER RPAREN          { SizeofV $3 }
| SIZEOF LPAREN type_declaration RPAREN    { Sizeof (build_type_decl $3) }
| STRING                                   { Str $1 }
;;

init_declarator:
  declarator                               { ($1, None) }
| declarator EQ init                       { ($1, Some $3) }
;;

init:
  expression                               { Data $1 }
| LBRACE init_list RBRACE                  { Sequence $2 }
| LBRACE RBRACE                            { Sequence [] }
;;

init_list:
  init COMMA init_list                     { $1::$3 }
| init                                     { $1::[] }
;;

abstract_declarator:
| pointer abstract_declarator              { Pointer $2 }
| LPAREN abstract_declarator RPAREN        { $2 }
| LBRACKET RBRACKET                        { Array (Abstract, None) }
| LBRACKET INTEGER RBRACKET                { Array (Abstract, Some $2) }
| abstract_declarator 
  LPAREN parameter_list RPAREN             { Function ($1, $3) }
| abstract_declarator LPAREN RPAREN        { Function ($1, []) }
;;

declarator:
| pointer declarator                       { Pointer $2 }
| LPAREN declarator RPAREN                 { $2 }
| IDENTIFIER                               { Variable ($1, get_loc ()) }
| declarator LBRACKET INTEGER RBRACKET     { Array ($1, Some $3) }
| declarator LBRACKET RBRACKET             { Array ($1, None) }
| declarator LPAREN parameter_list RPAREN  { Function ($1, $3) }
| declarator LPAREN RPAREN                 { Function ($1, []) }
;;

pointer:
  STAR type_qualifier_list                 {  }
;;

field_list:
  field_declaration SEMICOLON field_list   { $1::$3 } 
| field_declaration SEMICOLON              { $1::[] }
;;

parameter_list:
  parameter_declaration COMMA 
  parameter_list                           { $1::$3 }
| parameter_declaration                    { $1::[]}
;;

type_specifier:
  VOID                                   { Void }
| ityp                                   { Integer (Newspeak.Signed, $1) }
| UNSIGNED ityp                          { Integer (Newspeak.Unsigned, $2) }
| STRUCT field_blk                       { Struct (gen_struct_id (), Some $2) }
| STRUCT IDENTIFIER                      { Struct ($2, None) }
| STRUCT IDENTIFIER field_blk            { Struct ($2, Some $3) }
| UNION field_blk                        { Union (gen_struct_id (), Some $2) }
| UNION IDENTIFIER                       { Union ($2, None) }
| UNION IDENTIFIER field_blk             { Union ($2, Some $3) }
| TYPEDEF_NAME                           { Name $1 }
;;

field_blk:
  LBRACE field_list RBRACE                 { $2 }
;;  

ityp:
  CHAR                                     { Char }
| SHORT                                    { Short }
| INT                                      { Int }
| LONG                                     { Long }
| LONG LONG                                { LongLong }
;;
