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
open Cir
open Csyntax
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
  let build_vdecl (t, x, loc) init = 
    (GlbVDecl ((x, t, static, init), extern), loc) 
  in
  let (edecls, vdecls) = process_decls build_vdecl d in
  let build_edecl (d, loc) = (GlbEDecl d, loc) in
  let edecls = List.map build_edecl edecls in
    (edecls@vdecls)

let build_fundef static (b, m, body) = 
  let (_, (t, x, loc)) = Synthack.normalize_decl (b, m) in
  let x =
    match x with
      | Some x -> x
      | None -> 
	  (* TODO: code cleanup remove these things !!! *)
	  Npkcontext.error "Firstpass.translate_global" "unknown function name"
  in
    (FunctionDef (x, t, static, body), loc)::[]
      
let build_glbtypedef d =
  let build_edecl (d, loc) = (GlbEDecl d, loc) in
  let build_vdecl (t, x, loc) init = 
    let x =
      match x with
	  Some x -> x
	| None -> 
	    (* TODO: code cleanup remove these things !!! *)
	    Npkcontext.error "Firstpass.translate_global" "type name"
    in
      Synthack.define_type x t;
      (* TODO: clean this up *)
      (GlbVDecl ((None, t, false, init), false), loc) 
  in
  let (edecls, vdecls) = process_decls build_vdecl d in
  let edecls = List.map build_edecl edecls in
    edecls@vdecls

let build_typedef d =
  let build_edecl (d, loc) = (EDecl d, loc) in
  let build_vdecl (t, x, loc) init = 
    let x =
      match x with
	  Some x -> x
	| None -> 
	    (* TODO: code cleanup remove these things !!! *)
	    Npkcontext.error "Firstpass.translate_global" "type name"
    in
      Synthack.define_type x t;
      (VDecl (None, t, false, init), loc)
  in
  let (edecls, vdecls) = process_decls build_vdecl d in
  let edecls = List.map build_edecl edecls in
    edecls@vdecls

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
%token SWITCH TYPEDEF WHILE GOTO
%token CHAR DOUBLE FLOAT INT SHORT LONG STRUCT UNION SIGNED UNSIGNED VOID
%token ELLIPSIS COLON COMMA DOT LBRACE RBRACE 
%token LBRACKET RBRACKET LPAREN RPAREN NOT 
%token EQ OREQ SHIFTLEQ SHIFTREQ MINUSEQ PLUSEQ EQEQ NOTEQ STAREQ DIVEQ
%token SEMICOLON
%token AMPERSAND ARROW AND OR MINUS DIV MOD PLUS MINUSMINUS QMARK
%token PLUSPLUS STAR LT LTEQ GT GTEQ
%token SHIFTL SHIFTR BXOR BOR BNOT
%token ATTRIBUTE EXTENSION VA_LIST FORMAT PRINTF SCANF CDECL NORETURN DLLIMPORT
%token INLINE ALWAYS_INLINE ASM CDECL_ATTR FORMAT_ARG RESTRICT
%token EOF

%token <string> IDENTIFIER
%token <string> TYPEDEF_NAME
%token <string> STRING
%token <string option * string * char option * string option> INTEGER
%token <int> CHARACTER
%token <string * char option> FLOATCST

%type <string list * Csyntax.prog> parse
%start parse
%type <Csyntax.prog> translation_unit
%start translation_unit

%%
/* TODO: simplify code by generalizing!!! 
try to remove multiple occurence of same pattern: factor as much as possible
*/
// carefull not to have any empty rule: this deceives line number location

// TODO: simplify parser and link it to C standard sections!!!

parse:
  translation_unit                         { (Synthack.get_fnames (), $1) }
;;

translation_unit:
  external_declaration translation_unit    { $1@$2 }
| SEMICOLON translation_unit               { 
    Npkcontext.report_dirty_warning "Parser.translation_unit" 
      "unnecessary semicolon";
    $2 
  }
|                                          { [] }
;;

function_definition:
  declaration_specifiers declarator 
  compound_statement                       { ($1, $2, $3) }
;;

declaration:
  declaration_specifiers                   { ($1, (Abstract, None)::[]) }
| declaration_specifiers 
  init_declarator_list                     { ($1, $2) }
;;

init_declarator:
  declarator                               { ($1, None) }
| declarator EQ init                       { ($1, Some $3) }
;;

declarator:
| pointer declarator                       { Pointer $2 }
| LPAREN declarator RPAREN                 { $2 }
| IDENTIFIER                               { Variable ($1, get_loc ()) }
| declarator LBRACKET expression RBRACKET  { Array ($1, Some $3) }
| declarator LBRACKET 
             type_qualifier_list RBRACKET  { Array ($1, None) }
| declarator 
  LPAREN parameter_list RPAREN             { Function ($1, $3) }
| declarator LPAREN RPAREN                 { Function ($1, []) }
;;

struct_declarator_list:
  struct_declarator COMMA 
  struct_declarator_list                   { $1::$3 }
| struct_declarator                        { $1::[] }
;;

struct_declarator:
  declarator                               { ($1, None) }
| declarator COLON conditional_expression  { ($1, Some $3) }
| COLON conditional_expression             { 
    Npkcontext.report_dirty_warning "Parser.struct_declarator"
      "anonymous field declaration in structure";
    (Abstract, Some $2) 
  }
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

// TODO: this part should be rewritten!!!, using for instance
// the grammar from here http://www.quut.com/c/ANSI-C-grammar-y.html
declaration_specifiers:
  type_qualifier_list type_specifier 
  type_qualifier_list                      { $2 }
;;

type_qualifier_list:
  type_qualifier type_qualifier_list       { }
|                                          { }
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
  IDENTIFIER COLON statement               { (Label $1, get_loc ())::$3 }
| declaration SEMICOLON                    { build_stmtdecl false $1 }
| STATIC declaration SEMICOLON             { build_stmtdecl true $2 }
| TYPEDEF declaration SEMICOLON            { build_typedef $2 }
| IF LPAREN expression RPAREN statement    { [If ($3, $5, []), get_loc ()] }
| IF LPAREN expression RPAREN statement
  ELSE statement                           { [If ($3, $5, $7), get_loc ()] }
| switch_stmt                              { [CSwitch $1, get_loc ()] }
| iteration_statement                      { [For $1, get_loc ()] }
| RETURN expression SEMICOLON              { [Return (Some $2), get_loc ()] }
| RETURN SEMICOLON                         { [Return None, get_loc ()] }
| assignment_expression SEMICOLON          { [Exp $1, get_loc ()] }
| BREAK SEMICOLON                          { [Break, get_loc ()] }
| CONTINUE SEMICOLON                       { [Continue, get_loc ()] }
| GOTO IDENTIFIER SEMICOLON                { 
    let report =
      if !Npkcontext.forward_goto then Npkcontext.print_warning
      else Npkcontext.error
    in
      report "Parser.statement"
	"goto statements are error-prone, they should be avoided at all costs";
    [Goto $2, get_loc ()] 
  }
| compound_statement                       { [Block $1, get_loc ()] }
| SEMICOLON                                { [] }
;;

iteration_statement:
| FOR LPAREN assignment_expression_list SEMICOLON 
      expression_statement
      assignment_expression_list RPAREN
      statement                            { ($3, $5, $8, $6) }
| FOR LPAREN SEMICOLON 
      expression_statement
      assignment_expression_list RPAREN
      statement                            { 
	Npkcontext.print_warning "Parser.iteration_statement" 
	  "init statement expected";
	([], $4, $7, $5) 
      }
| FOR LPAREN assignment_expression_list SEMICOLON 
      expression_statement RPAREN
      statement                            { 
	Npkcontext.print_warning "Parser.iteration_statement" 
	  "increment statement expected";
	($3, $5, [], $7) 
      }
| FOR LPAREN SEMICOLON expression_statement RPAREN
      statement                            { 
	Npkcontext.print_warning "Parser.iteration_statement" 
	  "init statement expected";
	([], $4, $6, []) 
      }
| WHILE LPAREN expression RPAREN statement { ([], $3, $5, []) }
| DO statement
  WHILE LPAREN expression RPAREN SEMICOLON { ($2, $5, $2, []) }
;;

expression_statement:
  SEMICOLON                                { 
    Npkcontext.print_warning "Parser.expression_statement" 
      "halting condition should be explicit";
    exp_of_int 1
  }
| expression SEMICOLON                     { $1 }
;;

switch_stmt:
  SWITCH LPAREN expression RPAREN LBRACE
    case_list
  RBRACE                                   { ($3, $6, []) }
| SWITCH LPAREN expression RPAREN LBRACE
    case_list
    DEFAULT COLON statement_list
  RBRACE                                   { ($3, $6, $9) }
;;

case_list:
  CASE expression COLON statement_list 
  case_list                                { ($2, $4, get_loc ())::$5 }
|                                          { [] }
;;

assignment_expression_list:
  assignment_expression COMMA 
  assignment_expression_list               { (Exp $1, get_loc ())::$3 }
| assignment_expression                    { (Exp $1, get_loc ())::[] }
;;

primary_expression:
  IDENTIFIER                               { Var $1 }
| CHARACTER                                { 
    Cst (Csyntax.char_cst_of_lexeme $1) 
  }
| INTEGER                                  { 
    Cst (Csyntax.int_cst_of_lexeme $1) 
  }
| FLOATCST                                 { 
    Cst (Csyntax.float_cst_of_lexeme $1) 
  }
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
| postfix_expression PLUSPLUS              { OpExp (Plus, $1, true) }
| postfix_expression MINUSMINUS            { OpExp (Minus, $1, true) }
;;

unary_expression:
  postfix_expression                       { $1 }
| PLUSPLUS unary_expression                { OpExp (Plus, $2, false) }
| MINUSMINUS unary_expression              { OpExp (Minus, $2, false) }
| AMPERSAND cast_expression                { AddrOf $2 }
| STAR cast_expression                     { Deref $2 }
| BNOT cast_expression                     { Unop (BNot, $2) }
| MINUS cast_expression                    { Unop (Neg, $2) }
| NOT cast_expression                      { Unop (Not, $2) }
| SIZEOF unary_expression                  { SizeofE $2 }
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
  inclusive_or_expression                  { IfExp ($1, $3, exp_of_int 0) }
;;

logical_or_expression:
  logical_and_expression                   { $1 }
| logical_or_expression OR
  logical_and_expression                   { IfExp ($1, exp_of_int 1, $3) }
;;

conditional_expression:
  logical_or_expression                    { $1 }
| logical_or_expression QMARK 
  expression COLON conditional_expression  {
    Npkcontext.report_dirty_warning "Parser.type_specifier" 
      "conditional expression are ugly: use if else instead";
    IfExp ($1, $3, $5)
  }
;;

// I do not want to have expression be assignment_expression
// this would allow assignments within expressions 
// it is error-prone, for instance typos may make you write
// if (x = 0) instead of if (x == 0)
expression:
  assignment_expression                   { $1 }
;;

assignment_expression:
  conditional_expression                   { $1 }
| unary_expression EQ expression           { Set ($1, $3) }
| unary_expression assignment_operator
  expression                               { SetOp ($1, $2, $3) }
;;

assignment_operator:
  PLUSEQ                                   { Plus }
| MINUSEQ                                  { Minus }
| STAREQ                                   { Mult }
| DIVEQ                                    { Div }
| OREQ                                     { BOr }
| SHIFTLEQ                                 { Shiftl }
| SHIFTREQ                                 { Shiftr }
;;

argument_expression_list:
  expression                               { $1::[] }
| expression 
  COMMA argument_expression_list           { $1::$3 }
;;

init:
  expression                               { Data $1 }
| LBRACE init_list RBRACE                  { Sequence $2 }
| LBRACE named_init_list RBRACE            { Sequence $2 }
;;

named_init_list:
  named_init COMMA named_init_list         { $1::$3 }
| named_init                               { $1::[] }
;;

named_init:
  DOT IDENTIFIER EQ expression             { (Some $2, Data $4) }
;;

init_list:
  init COMMA init_list                     { (None, $1)::$3 }
| init                                     { (None, $1)::[] }
|                                          {
  Npkcontext.report_dirty_warning "Parser.type_specifier" 
    "ugly initializer syntax";
  []
  }
;;

abstract_declarator:
| pointer                                  { Pointer Abstract }
| pointer abstract_declarator              { Pointer $2 }
| LPAREN abstract_declarator RPAREN        { $2 }
| LBRACKET type_qualifier_list RBRACKET    { Array (Abstract, None) }
| LBRACKET expression RBRACKET             { Array (Abstract, Some $2) }
| abstract_declarator 
  LBRACKET expression RBRACKET             { Array ($1, Some $3) }
| abstract_declarator 
  LPAREN parameter_list RPAREN             { Function ($1, $3) }
| abstract_declarator LPAREN RPAREN        { Function ($1, []) }
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
  parameter_list                           { $1::$3 }
| parameter_declaration                    { $1::[] }
| ELLIPSIS                                 {
    let loc = get_loc () in
      (Va_arg, Variable ("__builtin_newspeak_va_arg", loc))::[] 
  }
;;

/*
From ANSI C norm
4 There are five standard signed integer types, designated as signed 
char, short int, int, long int, and long long int. (These and other 
types may be designated in several additional ways, as described in 
6.7.2.)
*/
ityp:
  CHAR                                   { Config.size_of_char }
| SHORT INT                              { Config.size_of_short }
| INT                                    { Config.size_of_int }
| LONG INT                               { Config.size_of_long }
| LONG LONG INT                          { Config.size_of_longlong }
| SHORT                                  { 
    Npkcontext.report_strict_warning "Parser.ityp" 
      "'short' is not normalized: use 'short int' instead";
    Config.size_of_short 
  }
| LONG                                   { 
    Npkcontext.report_strict_warning "Parser.ityp" 
      "'long' is not normalized: use 'long int' instead";
    Config.size_of_long 
  }
| LONG LONG                              { 
    Npkcontext.report_strict_warning "Parser.ityp" 
      "'long long' is not standard: use 'long long int' instead";
    Config.size_of_longlong 
  }
;;


// ident_or_tname necessary because the namespace of structure and typedefs
// are not disjoint
ident_or_tname:
  IDENTIFIER                             { $1 }
| TYPEDEF_NAME                           {
    Npkcontext.print_warning "Parser.ident_or_tname" 
      ("identifier "^$1^" is defined as a type, avoid using it for "
	^"another purpose");
    $1 
  }
;;

enum_list:
  enum                                   { $1::[] }
| enum COMMA enum_list                   { $1::$3 }
;;

enum:
  IDENTIFIER                             { ($1, None) }
| IDENTIFIER EQ expression               { ($1, Some $3) }
;;

field_blk:
  LBRACE field_list RBRACE               { $2 }
;;

ftyp:
  FLOAT                                  { Config.size_of_float }
| DOUBLE                                 { Config.size_of_double }
| LONG DOUBLE                            { Config.size_of_longdouble }
;;

type_specifier:
  VOID                                   { Void }
| ityp                                   { Integer (Newspeak.Signed, $1) }
| SIGNED ityp                            {
    Npkcontext.report_dirty_warning "Parser.type_specifier" 
      "signed specifier not necessary";
    Integer (Newspeak.Signed, $2)
  }
| UNSIGNED ityp                          { Integer (Newspeak.Unsigned, $2) }
| UNSIGNED                               { 
    Npkcontext.report_dirty_warning "Parser.type_specifier" 
      "Integer kind should be specified";
    Integer (Newspeak.Unsigned, Config.size_of_int) 
  }
| ftyp                                   { Float $1 }
| STRUCT field_blk                       { Struct (gen_struct_id (), Some $2) }
| STRUCT ident_or_tname                  { Struct ($2, None) }
| STRUCT ident_or_tname field_blk        { Struct ($2, Some $3) }
| UNION field_blk                        { Union (gen_struct_id (), Some $2) }
| UNION ident_or_tname                   { Union ($2, None) }
| UNION ident_or_tname field_blk         { Union ($2, Some $3) }
| TYPEDEF_NAME                           { Name $1 }
| ENUM LBRACE enum_list RBRACE           { Enum (Some ($3, get_loc ())) }
| ENUM IDENTIFIER                        { Enum None }
| ENUM IDENTIFIER 
  LBRACE enum_list RBRACE                { Enum (Some ($4, get_loc ())) }
| SHORT UNSIGNED INT                     { 
    Npkcontext.report_strict_warning "Parser.type_specifier" 
      ("'short unsigned int' is not normalized: "
	^"use 'usigned short int' instead");
    Integer (Newspeak.Unsigned, Config.size_of_short) 
  }
| LONG UNSIGNED INT                     { 
    Npkcontext.report_strict_warning "Parser.type_specifier" 
      ("'long unsigned int' is not normalized: "
	^"use 'usigned long int' instead");
    Integer (Newspeak.Unsigned, Config.size_of_long) 
  }
| VA_LIST                                { Va_arg }
;;


//Section that is dependent on version of the compiler (standard ANSI or GNU)
//TODO: find a way to factor some of these, possible!!!
external_declaration:
  STATIC declaration SEMICOLON             { build_glbdecl (true, false) $2 }
| function_definition                      { build_fundef false $1 }
| STATIC function_definition               { build_fundef true $2 }
| EXTERN function_definition               { 
    Npkcontext.report_dirty_warning "Parser.external_declaration" 
      "defined functions should not be extern";
    build_fundef false $2 
}
| TYPEDEF declaration SEMICOLON            { build_glbtypedef $2 }
// GNU C extension
| EXTENSION TYPEDEF declaration SEMICOLON  { build_glbtypedef $3 }
| declaration attribute_list SEMICOLON     { build_glbdecl (false, false) $1 }
| EXTERN declaration attribute_list 
  SEMICOLON                                { build_glbdecl (false, true) $2 }
;;

attribute_list:
  attribute attribute_list                { }
|                                         { }
;;

type_qualifier:
  CONST                                    { }
| attribute                                { }
;;

field_declaration:
  declaration_specifiers 
  struct_declarator_list                   { flatten_field_decl ($1, $2) }
| declaration_specifiers                   { 
    Npkcontext.report_dirty_warning "Parser.field_declaration"
      "anonymous field declaration in structure";
    flatten_field_decl ($1, (Abstract, None)::[]) 
  }
// GNU C extension
| EXTENSION declaration_specifiers
  struct_declarator_list                   { flatten_field_decl ($2, $3) }
| EXTENSION declaration_specifiers         { 
    Npkcontext.report_dirty_warning "Parser.field_declaration"
      "anonymous field declaration in structure";
    flatten_field_decl ($2, (Abstract, None)::[]) 
  }
;;

attribute:
  ATTRIBUTE LPAREN LPAREN attribute_name
  RPAREN RPAREN                            { }
| ASM LPAREN STRING RPAREN                 { 
    let report = 
      if !Npkcontext.ignores_asm then Npkcontext.print_warning
      else Npkcontext.error
    in
      report "Parser.attribute" ("ignoring asm directive '"^$3^"'")
  }
| ASM LPAREN STRING STRING RPAREN          { 
    let report = 
      if !Npkcontext.ignores_asm then Npkcontext.print_warning
      else Npkcontext.error
    in
      report "Parser.attribute" ("ignoring asm directive '"^$3^"' '"^$4^"'")
  }
| INLINE                                   { }
| CDECL                                    { }
| RESTRICT                                 { }
;;

attribute_name:
  DLLIMPORT                                {
    Npkcontext.print_warning "Parser.attribute" 
      "ignoring attribute dllimport"
  }
| CDECL_ATTR                               { }
| NORETURN                                 { }
| FORMAT LPAREN 
    format_fun COMMA INTEGER COMMA INTEGER 
  RPAREN                                   { }
| FORMAT_ARG LPAREN INTEGER RPAREN         { }
| ALWAYS_INLINE                            { }
;;

format_fun:
  PRINTF                                   { }
| SCANF                                    { }
;;
