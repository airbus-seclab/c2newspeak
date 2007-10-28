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
(* TODO: write checks for all the syntax that is thrown away in these functions
   !! *)
let get_loc () =
  let pos = symbol_start_pos () in
    (pos.pos_fname, pos.pos_lnum, pos.pos_cnum)

let build_fundef (b, m) body = 
  let loc = get_loc () in
  let build ((v, _), _)  = (FunctionDef ((b, v), body), loc) in
    List.map build m

let build_typedef (b, m) =
  let loc = get_loc () in
  let build ((v, _), _) = (Typedef (b, v), loc) in
    List.map build m

let build_decl (b, m) =
  let build ((v, _), _) = (b, v) in
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
%token AMPERSAND AND PLUS PLUSPLUS STAR LT GT
%token EOF

%token <string> IDENTIFIER
%token <Int64.t> INTEGER

/*
%left LT GT EQ NE LE GE
%left PLUS MINUS
%left MULTIPLY DIVIDE
*/

%left LT GT EQEQ NOTEQ
%left AND
%left PLUS
%left STAR

%type <Bare_csyntax.prog> cprog
%start cprog

%%
/* TODO: simplify code by generalizing!!! 
try to remove multiple occurence of same pattern: factor as much as possible
*/
// carefull not to have any empty rule: this deceives line number location

cprog:
  global cprog                             { $1@$2 }
|                                          { [] }
;;

global:
  declaration SEMICOLON                    { build_glbdecl false $1 }
| EXTERN declaration SEMICOLON             { build_glbdecl true $2 }
| declaration block                        { build_fundef $1 $2 }
| TYPEDEF declaration SEMICOLON            { build_typedef $2 }
;;

declaration:
  base_typ init_var_modifier_list          { ($1, $2) }
;;

init_var_modifier_list:
  init_var_modifier COMMA 
  init_var_modifier_list                   { ($1, get_loc ())::$3 }
| init_var_modifier                        { ($1, get_loc ())::[] }
;;

block:
  LBRACE statement_list RBRACE             { $2 }
;;

field_list:
  declaration SEMICOLON field_list         { (build_decl $1)@$3 }
|                                          { [] }
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
| call SEMICOLON                           { [Exp $1, get_loc ()] }
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

call:
| IDENTIFIER LPAREN RPAREN                 { Call ($1, []) }
| IDENTIFIER LPAREN expression_list RPAREN { Call ($1, $3) }
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
  INTEGER                                  { Const $1 }
| left_value                               { Lval $1 }
| expression AND expression                { And ($1, $3) }
| expression PLUS expression               { Binop (Plus, $1, $3) }
| expression STAR expression               { Binop (Mult, $1, $3) }
| expression GT expression                 { Binop (Gt, $1, $3) }
| expression LT expression                 { Binop (Gt, $3, $1) }
| call                                     { $1 }
| AMPERSAND left_value                     { AddrOf $2 }
| LPAREN expression RPAREN                 { $2 }
| expression EQEQ expression               { Binop (Eq, $1, $3) }
| expression NOTEQ expression              { Unop (Not, Binop (Eq, $1, $3)) }
;;

base_typ:
  VOID                                     { Void }
| ityp                                     { Integer (Newspeak.Signed, $1) }
| UNSIGNED ityp                            { Integer (Newspeak.Unsigned, $2) }
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

// TODO: careful, this is a bit of a hack
arg:
  base_typ var_modifier                    { ($1, $2) }
| base_typ                                 { ($1, Variable "undefined!") }
;;

ityp:
  CHAR                                     { Char }
| SHORT                                    { Short }
| INT                                      { Int }
| LONG                                     { Long }
| LONG LONG                                { LongLong }
;;


/*
// TODO: use this new grammar instead
From here: http://www.quut.com/c/ANSI-C-grammar-y.html
ANSI C Yacc grammar
In 1985, Jeff Lee published his Yacc grammar (which is accompanied by a matching Lex specification) for the April 30, 1985 draft version of the ANSI C standard.  Tom Stockfisch reposted it to net.sources in 1987; that original, as mentioned in the answer to question 17.25 of the comp.lang.c FAQ, can be ftp'ed from ftp.uu.net, file usenet/net.sources/ansi.c.grammar.Z. 
The version you see here has been updated based on an 1998 draft of the standards document. It allows for restricted pointers, variable arrays, "inline", and designated initializers. The previous version's lex and yacc files (ANSI C as of ca 1995) are still around as archived copies. 

I intend to keep this version as close to the current C Standard grammar as possible; please let me know if you discover discrepancies.
(If you feel like it, read the FAQ first.) 

Jutta Degener, December 2006 


%token IDENTIFIER CONSTANT STRING_LITERAL SIZEOF
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN TYPE_NAME

%token TYPEDEF EXTERN STATIC AUTO REGISTER INLINE RESTRICT
%token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
%token BOOL COMPLEX IMAGINARY
%token STRUCT UNION ENUM ELLIPSIS

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%start translation_unit
%%

primary_expression
	: IDENTIFIER
	| CONSTANT
	| STRING_LITERAL
	| '(' expression ')'
	;

postfix_expression
	: primary_expression
	| postfix_expression '[' expression ']'
	| postfix_expression '(' ')'
	| postfix_expression '(' argument_expression_list ')'
	| postfix_expression '.' IDENTIFIER
	| postfix_expression PTR_OP IDENTIFIER
	| postfix_expression INC_OP
	| postfix_expression DEC_OP
	| '(' type_name ')' '{' initializer_list '}'
	| '(' type_name ')' '{' initializer_list ',' '}'
	;

argument_expression_list
	: assignment_expression
	| argument_expression_list ',' assignment_expression
	;

unary_expression
	: postfix_expression
	| INC_OP unary_expression
	| DEC_OP unary_expression
	| unary_operator cast_expression
	| SIZEOF unary_expression
	| SIZEOF '(' type_name ')'
	;

unary_operator
	: '&'
	| '*'
	| '+'
	| '-'
	| '~'
	| '!'
	;

cast_expression
	: unary_expression
	| '(' type_name ')' cast_expression
	;

multiplicative_expression
	: cast_expression
	| multiplicative_expression '*' cast_expression
	| multiplicative_expression '/' cast_expression
	| multiplicative_expression '%' cast_expression
	;

additive_expression
	: multiplicative_expression
	| additive_expression '+' multiplicative_expression
	| additive_expression '-' multiplicative_expression
	;

shift_expression
	: additive_expression
	| shift_expression LEFT_OP additive_expression
	| shift_expression RIGHT_OP additive_expression
	;

relational_expression
	: shift_expression
	| relational_expression '<' shift_expression
	| relational_expression '>' shift_expression
	| relational_expression LE_OP shift_expression
	| relational_expression GE_OP shift_expression
	;

equality_expression
	: relational_expression
	| equality_expression EQ_OP relational_expression
	| equality_expression NE_OP relational_expression
	;

and_expression
	: equality_expression
	| and_expression '&' equality_expression
	;

exclusive_or_expression
	: and_expression
	| exclusive_or_expression '^' and_expression
	;

inclusive_or_expression
	: exclusive_or_expression
	| inclusive_or_expression '|' exclusive_or_expression
	;

logical_and_expression
	: inclusive_or_expression
	| logical_and_expression AND_OP inclusive_or_expression
	;

logical_or_expression
	: logical_and_expression
	| logical_or_expression OR_OP logical_and_expression
	;

conditional_expression
	: logical_or_expression
	| logical_or_expression '?' expression ':' conditional_expression
	;

assignment_expression
	: conditional_expression
	| unary_expression assignment_operator assignment_expression
	;

assignment_operator
	: '='
	| MUL_ASSIGN
	| DIV_ASSIGN
	| MOD_ASSIGN
	| ADD_ASSIGN
	| SUB_ASSIGN
	| LEFT_ASSIGN
	| RIGHT_ASSIGN
	| AND_ASSIGN
	| XOR_ASSIGN
	| OR_ASSIGN
	;

expression
	: assignment_expression
	| expression ',' assignment_expression
	;

constant_expression
	: conditional_expression
	;

declaration
	: declaration_specifiers ';'
	| declaration_specifiers init_declarator_list ';'
	;

declaration_specifiers
	: storage_class_specifier
	| storage_class_specifier declaration_specifiers
	| type_specifier
	| type_specifier declaration_specifiers
	| type_qualifier
	| type_qualifier declaration_specifiers
	| function_specifier
	| function_specifier declaration_specifiers
	;

init_declarator_list
	: init_declarator
	| init_declarator_list ',' init_declarator
	;

init_declarator
	: declarator
	| declarator '=' initializer
	;

storage_class_specifier
	: TYPEDEF
	| EXTERN
	| STATIC
	| AUTO
	| REGISTER
	;

type_specifier
	: VOID
	| CHAR
	| SHORT
	| INT
	| LONG
	| FLOAT
	| DOUBLE
	| SIGNED
	| UNSIGNED
	| BOOL
	| COMPLEX
	| IMAGINARY
	| struct_or_union_specifier
	| enum_specifier
	| TYPE_NAME
	;

struct_or_union_specifier
	: struct_or_union IDENTIFIER '{' struct_declaration_list '}'
	| struct_or_union '{' struct_declaration_list '}'
	| struct_or_union IDENTIFIER
	;

struct_or_union
	: STRUCT
	| UNION
	;

struct_declaration_list
	: struct_declaration
	| struct_declaration_list struct_declaration
	;

struct_declaration
	: specifier_qualifier_list struct_declarator_list ';'
	;

specifier_qualifier_list
	: type_specifier specifier_qualifier_list
	| type_specifier
	| type_qualifier specifier_qualifier_list
	| type_qualifier
	;

struct_declarator_list
	: struct_declarator
	| struct_declarator_list ',' struct_declarator
	;

struct_declarator
	: declarator
	| ':' constant_expression
	| declarator ':' constant_expression
	;

enum_specifier
	: ENUM '{' enumerator_list '}'
	| ENUM IDENTIFIER '{' enumerator_list '}'
	| ENUM '{' enumerator_list ',' '}'
	| ENUM IDENTIFIER '{' enumerator_list ',' '}'
	| ENUM IDENTIFIER
	;

enumerator_list
	: enumerator
	| enumerator_list ',' enumerator
	;

enumerator
	: IDENTIFIER
	| IDENTIFIER '=' constant_expression
	;

type_qualifier
	: CONST
	| RESTRICT
	| VOLATILE
	;

function_specifier
	: INLINE
	;

declarator
	: pointer direct_declarator
	| direct_declarator
	;


direct_declarator
	: IDENTIFIER
	| '(' declarator ')'
	| direct_declarator '[' type_qualifier_list assignment_expression ']'
	| direct_declarator '[' type_qualifier_list ']'
	| direct_declarator '[' assignment_expression ']'
	| direct_declarator '[' STATIC type_qualifier_list assignment_expression ']'
	| direct_declarator '[' type_qualifier_list STATIC assignment_expression ']'
	| direct_declarator '[' type_qualifier_list '*' ']'
	| direct_declarator '[' '*' ']'
	| direct_declarator '[' ']'
	| direct_declarator '(' parameter_type_list ')'
	| direct_declarator '(' identifier_list ')'
	| direct_declarator '(' ')'
	;

pointer
	: '*'
	| '*' type_qualifier_list
	| '*' pointer
	| '*' type_qualifier_list pointer
	;

type_qualifier_list
	: type_qualifier
	| type_qualifier_list type_qualifier
	;


parameter_type_list
	: parameter_list
	| parameter_list ',' ELLIPSIS
	;

parameter_list
	: parameter_declaration
	| parameter_list ',' parameter_declaration
	;

parameter_declaration
	: declaration_specifiers declarator
	| declaration_specifiers abstract_declarator
	| declaration_specifiers
	;

identifier_list
	: IDENTIFIER
	| identifier_list ',' IDENTIFIER
	;

type_name
	: specifier_qualifier_list
	| specifier_qualifier_list abstract_declarator
	;

abstract_declarator
	: pointer
	| direct_abstract_declarator
	| pointer direct_abstract_declarator
	;

direct_abstract_declarator
	: '(' abstract_declarator ')'
	| '[' ']'
	| '[' assignment_expression ']'
	| direct_abstract_declarator '[' ']'
	| direct_abstract_declarator '[' assignment_expression ']'
	| '[' '*' ']'
	| direct_abstract_declarator '[' '*' ']'
	| '(' ')'
	| '(' parameter_type_list ')'
	| direct_abstract_declarator '(' ')'
	| direct_abstract_declarator '(' parameter_type_list ')'
	;

initializer
	: assignment_expression
	| '{' initializer_list '}'
	| '{' initializer_list ',' '}'
	;

initializer_list
	: initializer
	| designation initializer
	| initializer_list ',' initializer
	| initializer_list ',' designation initializer
	;

designation
	: designator_list '='
	;

designator_list
	: designator
	| designator_list designator
	;

designator
	: '[' constant_expression ']'
	| '.' IDENTIFIER
	;

statement
	: labeled_statement
	| compound_statement
	| expression_statement
	| selection_statement
	| iteration_statement
	| jump_statement
	;

labeled_statement
	: IDENTIFIER ':' statement
	| CASE constant_expression ':' statement
	| DEFAULT ':' statement
	;

compound_statement
	: '{' '}'
	| '{' block_item_list '}'
	;

block_item_list
	: block_item
	| block_item_list block_item
	;

block_item
	: declaration
	| statement
	;

expression_statement
	: ';'
	| expression ';'
	;

selection_statement
	: IF '(' expression ')' statement
	| IF '(' expression ')' statement ELSE statement
	| SWITCH '(' expression ')' statement
	;

iteration_statement
	: WHILE '(' expression ')' statement
	| DO statement WHILE '(' expression ')' ';'
	| FOR '(' expression_statement expression_statement ')' statement
	| FOR '(' expression_statement expression_statement expression ')' statement
	| FOR '(' declaration expression_statement ')' statement
	| FOR '(' declaration expression_statement expression ')' statement
	;

jump_statement
	: GOTO IDENTIFIER ';'
	| CONTINUE ';'
	| BREAK ';'
	| RETURN ';'
	| RETURN expression ';'
	;

translation_unit
	: external_declaration
	| translation_unit external_declaration
	;

external_declaration
	: function_definition
	| declaration
	;

function_definition
	: declaration_specifiers declarator declaration_list compound_statement
	| declaration_specifiers declarator compound_statement
	;

declaration_list
	: declaration
	| declaration_list declaration
	;


*/
