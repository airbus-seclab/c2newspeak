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

let process_decls build (b, m) =
  let (enumdecls, b) = Synthack.normalize_base_typ b in
  let process (v, init) =
    let d = Synthack.normalize_var_modifier b v in
      build d init
  in
    (enumdecls, List.map process m)

let build_glbdecl (static, extern) d =
  let build_edecl (d, loc) = (GlbEDecl d, loc) in
  let build_vdecl (t, x, loc) init = 
    (GlbVDecl ((x, t, static, init), extern), loc) 
  in
  let (edecls, vdecls) = process_decls build_vdecl d in
  let edecls = List.map build_edecl edecls in
    (edecls@vdecls)

let build_fundef (b, m, body) = 
  let (enumdecls, (t, x, loc)) = Synthack.normalize_decl (b, m) in
    (FunctionDef (x, t, body), loc)::[]
      
let build_glbtypedef d =
  let build_edecl (d, loc) = (GlbEDecl d, loc) in
  let build_vdecl (t, x, _) _ = Synthack.define_type x t in
  let (edecls, _) = process_decls build_vdecl d in
    List.map build_edecl edecls

let build_typedef d =
  let build_edecl (d, loc) = (EDecl d, loc) in
  let build_vdecl (t, x, _) _ = Synthack.define_type x t in
  let (edecls, _) = process_decls build_vdecl d in
    List.map build_edecl edecls

let build_stmtdecl static d =
  let build_edecl (d, loc) = (EDecl d, loc) in
  let build_vdecl (t, x, loc) init = (VDecl (x, t, static, init), loc) in
  let (edecls, vdecls) = process_decls build_vdecl d in
  let edecls = List.map build_edecl edecls in
    edecls@vdecls

let build_type_decl d =
  let (edecls, (t, _, _)) = Synthack.normalize_decl d in
    if (edecls <> []) then begin 
      Npkcontext.error "Parser.build_type_decl" "unexpected enum declaration"
    end;
    t

let flatten_field_decl (b, x) = List.map (fun (v, i) -> (b, v, i)) x

%}

%token BREAK CONST CONTINUE CASE DEFAULT DO ELSE ENUM STATIC 
%token EXTERN FOR IF RETURN SIZEOF 
%token SWITCH TYPEDEF WHILE
%token CHAR DOUBLE FLOAT INT SHORT LONG STRUCT UNION UNSIGNED VOID
%token ELLIPSIS COLON COMMA DOT LBRACE RBRACE 
%token LBRACKET RBRACKET LPAREN RPAREN NOT EQ PLUSEQ EQEQ NOTEQ SEMICOLON
%token AMPERSAND ARROW AND OR MINUS DIV MOD PLUS PLUSPLUS STAR LT LTEQ GT GTEQ
%token SHIFTL SHIFTR BXOR BOR BNOT
%token EOF

%token <string> IDENTIFIER
%token <string> TYPEDEF_NAME
%token <string> STRING
%token <Int64.t> INTEGER
%token <string> FLOATCST

%type <string list * Bare_csyntax.prog> parse
%start parse

%%
/* TODO: simplify code by generalizing!!! 
try to remove multiple occurence of same pattern: factor as much as possible
*/
// carefull not to have any empty rule: this deceives line number location

// TODO: simplify parser and link it to C standard sections!!!

parse:
  translation_unit                         { (Synthack.get_fnames (), 
					     (Synthack.get_compdefs (), $1)) }
;;

translation_unit:
  external_declaration translation_unit    { $1@$2 }
|                                          { [] }
;;

external_declaration:
  declaration                              { build_glbdecl (false, false) $1 }
| EXTERN declaration                       { build_glbdecl (false, true) $2 }
| STATIC declaration                       { build_glbdecl (true, false) $2 }
| function_definition                      { build_fundef $1 }
| TYPEDEF declaration                      { build_glbtypedef $2 }
;;

function_definition:
  declaration_specifiers declarator 
  compound_statement                       { ($1, $2, $3) }
;;

declaration:
  declaration_specifiers SEMICOLON         { ($1, []) }
| declaration_specifiers 
  init_declarator_list SEMICOLON           { ($1, $2) }
;;

field_declaration:
  declaration_specifiers 
  struct_declarator_list                   { flatten_field_decl ($1, $2) }
;;

struct_declarator_list:
  struct_declarator COMMA 
  struct_declarator_list                   { $1::$3 }
| struct_declarator                        { $1::[] }
;;

// TODO: careful, this is a bit of a hack
parameter_declaration:
  declaration_specifiers declarator        { ($1, $2) }
| declaration_specifiers 
  abstract_declarator                      { ($1, $2) }
| declaration_specifiers                   { ($1, Abstract) }
;;

type_name:
  declaration_specifiers                   { ($1, Abstract) }
| declaration_specifiers
  abstract_declarator                      { ($1, $2) }
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

compound_statement:
  LBRACE statement_list RBRACE             { $2 }
;;

statement_list:
  statement statement_list                 { $1@$2 }
|                                          { [] }
;;

statement:
  declaration                              { build_stmtdecl false $1 }
| STATIC declaration                       { build_stmtdecl true $2 }
| TYPEDEF declaration                      { build_typedef $2 }
| IF LPAREN expression RPAREN statement    { [If ($3, $5, []), get_loc ()] }
| IF LPAREN expression RPAREN statement
  ELSE statement                           { [If ($3, $5, $7), get_loc ()] }
| SWITCH LPAREN expression RPAREN LBRACE
  case_list
  RBRACE                                   { [Switch ($3, $6), get_loc ()] }
| FOR LPAREN assignment_expression SEMICOLON 
      expression SEMICOLON 
      assignment_expression RPAREN
      statement                            { let loc = get_loc () in
                                               (For ((Exp $3, loc)::[], 
						    $5, $9, 
						    (Exp $7, loc)::[]), loc)
					       ::[] }
| WHILE LPAREN expression RPAREN statement { [While ($3, $5), get_loc ()] }
| DO statement
  WHILE LPAREN expression RPAREN SEMICOLON { [DoWhile ($2, $5), get_loc ()] }
| RETURN expression SEMICOLON              { [Return (Some $2), get_loc ()] }
| RETURN SEMICOLON                         { [Return None, get_loc ()] }
| assignment_expression SEMICOLON          { [Exp $1, get_loc ()] }
| BREAK SEMICOLON                          { [Break, get_loc ()] }
| CONTINUE SEMICOLON                       { [Continue, get_loc ()] }
| compound_statement                       { [Block $1, get_loc ()] }
;;

case_list:
  case case_list                           { $1::$2 }
| case                                     { $1::[] }
;;

case:
  CASE expression COLON statement_list     { (Some $2, $4, get_loc ()) }
| DEFAULT COLON statement_list             { (None, $3, get_loc ()) }
;;

primary_expression:
  IDENTIFIER                               { Var $1 }
| INTEGER                                  { Cst (CInt $1) }
| FLOATCST                                 { Cst (CFloat $1) }
| STRING                                   { Str $1 }
| LPAREN expression RPAREN                 { $2 }
;;

postfix_expression:
  primary_expression                       { $1 }
| postfix_expression 
  LBRACKET expression RBRACKET             { Index ($1, $3) }
| postfix_expression LPAREN RPAREN         { Call ($1, []) }
| postfix_expression 
  LPAREN argument_expression_list RPAREN   { Call ($1, $3) }
| postfix_expression DOT IDENTIFIER        { Field ($1, $3) }
| postfix_expression ARROW IDENTIFIER      { Field (Deref $1, $3) }
| postfix_expression PLUSPLUS              { ExpPlusPlus $1 }
;;

unary_expression:
  postfix_expression                       { $1 }
| AMPERSAND cast_expression                { AddrOf $2 }
| STAR cast_expression                     { Deref $2 }
| BNOT cast_expression                     { Unop (BNot, $2) }
| MINUS cast_expression                    { Unop (Neg, $2) }
| NOT cast_expression                      { Unop (Not, $2) }
| SIZEOF LPAREN unary_expression RPAREN    { SizeofE $3 }
| SIZEOF LPAREN type_name RPAREN           { Sizeof (build_type_decl $3) }
;;

cast_expression:
  unary_expression                         { $1 }
| LPAREN type_name RPAREN 
  cast_expression                          { Cast ($4, build_type_decl $2) }
;;

multiplicative_expression:
  cast_expression                          { $1 }
| multiplicative_expression STAR 
  cast_expression                          { Binop (Mult, $1, $3) }
| multiplicative_expression DIV 
  cast_expression                          { Binop (Div, $1, $3) }
| multiplicative_expression MOD 
  cast_expression                          { Binop (Mod, $1, $3) }
;;

additive_expression:
  multiplicative_expression                { $1 }
| additive_expression PLUS 
  multiplicative_expression                { Binop (Plus, $1, $3) }
| additive_expression MINUS 
  multiplicative_expression                { Binop (Minus, $1, $3) }
;;

shift_expression:
  additive_expression                      { $1 }
| shift_expression SHIFTL 
  additive_expression                      { Binop (Shiftl, $1, $3) }
| shift_expression SHIFTR
  additive_expression                      { Binop (Shiftr, $1, $3) }
;;

relational_expression:
  shift_expression                         { $1 }
| relational_expression GT 
  shift_expression                         { Binop (Gt, $1, $3) }
| relational_expression GTEQ 
  shift_expression                         { Unop (Not, Binop (Gt, $3, $1)) }
| relational_expression LT 
  shift_expression                         { Binop (Gt, $3, $1) }
| relational_expression LTEQ 
  shift_expression                         { Unop (Not, Binop (Gt, $1, $3)) }
;;

equality_expression:
  relational_expression                    { $1 }
| equality_expression EQEQ 
  relational_expression                    { Binop (Eq, $1, $3) }
| equality_expression NOTEQ 
  relational_expression                    { Unop (Not, Binop (Eq, $1, $3)) }
;;

and_expression:
  equality_expression                      { $1 }
| and_expression AMPERSAND 
  equality_expression                      { Binop (BAnd, $1, $3) }
;;

exclusive_or_expression:
  and_expression                           { $1 }
| exclusive_or_expression BXOR 
  and_expression                           { Binop (BXor, $1, $3) }
;;

inclusive_or_expression:
  exclusive_or_expression                  { $1 }
| inclusive_or_expression BOR 
  exclusive_or_expression                  { Binop (BOr, $1, $3) }
;;

logical_and_expression:
  inclusive_or_expression                  { $1 }
| logical_and_expression AND 
  inclusive_or_expression                  { And ($1, $3) }
;;

logical_or_expression:
  logical_and_expression                   { $1 }
| logical_or_expression OR
  logical_and_expression                   { Or ($1, $3) }
;;

conditional_expression:
  logical_or_expression                    { $1 }
;;

expression:
  conditional_expression                   { $1 }
;;

assignment_expression:
  conditional_expression                   { $1 }
| unary_expression EQ expression           { Set ($1, $3) }
| unary_expression PLUSEQ expression       { Set ($1, Binop (Plus, $1, $3)) }
;;

argument_expression_list:
  expression                               { $1::[] }
| expression 
  COMMA argument_expression_list           { $1::$3 }
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
| pointer                                  { Pointer Abstract }
| pointer abstract_declarator              { Pointer $2 }
| LPAREN abstract_declarator RPAREN        { $2 }
| LBRACKET RBRACKET                        { Array (Abstract, None) }
| LBRACKET expression RBRACKET             { Array (Abstract, Some $2) }
| abstract_declarator 
  LPAREN parameter_list RPAREN             { let (args, va_list) = $3 in
					       Function ($1, args, va_list) }
| abstract_declarator LPAREN RPAREN        { Function ($1, [], false) }
;;

declarator:
| pointer declarator                       { Pointer $2 }
| LPAREN declarator RPAREN                 { $2 }
| IDENTIFIER                               { Variable ($1, get_loc ()) }
| declarator LBRACKET expression RBRACKET  { Array ($1, Some $3) }
| declarator LBRACKET RBRACKET             { Array ($1, None) }
| declarator 
  LPAREN parameter_list RPAREN             { let (args, va_list) = $3 in
					       Function ($1, args, va_list) }
| declarator LPAREN RPAREN                 { Function ($1, [], false) }
;;

struct_declarator:
  declarator                               { ($1, None) }
| declarator COLON INTEGER                 { ($1, Some $3) }
;;

pointer:
  STAR type_qualifier_list                 {  }
;;

field_list:
  field_declaration SEMICOLON field_list   { $1@$3 } 
| field_declaration SEMICOLON              { $1 }
;;

parameter_list:
  parameter_declaration COMMA 
  parameter_list                           { let (tl, va_list) = $3 in 
					       ($1::tl, va_list) }
| parameter_declaration                    { ($1::[], false) }
| ELLIPSIS                                 { ([], true) }
;;

type_specifier:
  VOID                                   { Void }
| ityp                                   { Integer (Newspeak.Signed, $1) }
| UNSIGNED ityp                          { Integer (Newspeak.Unsigned, $2) }
| UNSIGNED                               { 
    if (not !Npkcontext.dirty_syntax) 
    then begin
      Npkcontext.error "Parser.type_specifier" 
	"Integer kind should be specified"
    end;
    Integer (Newspeak.Unsigned, Config.size_of_int) 
  }
| ftyp                                   { Float $1 }
| STRUCT field_blk                       { Struct (gen_struct_id (), Some $2) }
| STRUCT IDENTIFIER                      { Struct ($2, None) }
| STRUCT IDENTIFIER field_blk            { Struct ($2, Some $3) }
| UNION field_blk                        { Union (gen_struct_id (), Some $2) }
| UNION IDENTIFIER                       { Union ($2, None) }
| UNION IDENTIFIER field_blk             { Union ($2, Some $3) }
| TYPEDEF_NAME                           { Name $1 }
| ENUM LBRACE enum_list RBRACE           { Enum (Some ($3, get_loc ())) }
| ENUM IDENTIFIER                        { Enum None }
| ENUM IDENTIFIER 
  LBRACE enum_list RBRACE                { Enum (Some ($4, get_loc ())) }
;;

enum_list:
  enum                                   { $1::[] }
| enum COMMA enum_list                   { $1::$3 }
;;

enum:
  IDENTIFIER                             { ($1, None) }
| IDENTIFIER EQ INTEGER                  { ($1, Some $3) }
;;

field_blk:
  LBRACE field_list RBRACE               { $2 }
;;  

ityp:
  CHAR                                   { Config.size_of_char }
| SHORT                                  { Config.size_of_short }
| INT                                    { Config.size_of_int }
| LONG                                   { Config.size_of_long }
| LONG LONG                              { Config.size_of_longlong }
;;

ftyp:
  FLOAT                                  { Config.size_of_float }
| DOUBLE                                 { Config.size_of_double }
| LONG DOUBLE                            { Config.size_of_longdouble }
;;
