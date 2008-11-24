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
let cur_fun = ref ""

let gen_struct_id () = 
  incr struct_cnt;
  "anon_struct"^(string_of_int !struct_cnt)
  
(* TODO: write checks for all the syntax that is thrown away in these functions
   !! *)

let get_loc () =
  let pos = symbol_start_pos () in
    (pos.pos_fname, pos.pos_lnum, pos.pos_cnum-pos.pos_bol)

(* TODO: code not so nice: simplify? *)
let process_decls (build_edecl, build_cdecl, build_vdecl) (b, m) =
  let ((edecls, cdecls), b) = Synthack.normalize_base_typ b in
  let edecls = List.map build_edecl edecls in
  let cdecls = List.map build_cdecl cdecls in
  let build_vdecl (v, init) res =
    let (t, x, loc) = Synthack.normalize_var_modifier b v in
      match x with
	| None -> res
	| Some x -> build_vdecl res (t, x, loc, init)
  in
  let vdecls = List.fold_right build_vdecl m [] in
    edecls@cdecls@vdecls
      
let build_glbdecl (static, extern) d =
  let build_vdecl l (t, x, loc, init) = 
    (GlbVDecl (x, t, static, extern, init), loc)::l
  in
  let loc = get_loc () in
  let build_edecl x = (GlbEDecl x, loc) in
  let build_cdecl x = (GlbCDecl x, loc) in
    process_decls (build_edecl, build_cdecl, build_vdecl) d

(* TODO: clean this code and find a way to factor with previous function *)
let build_glbtypedef d =
  let build_vdecl l (t, x, _, _) = 
    Synthack.define_type x t;
    l
  in
  let loc = get_loc () in
  let build_edecl x = (GlbEDecl x, loc) in
  let build_cdecl x = (GlbCDecl x, loc) in
    process_decls (build_edecl, build_cdecl, build_vdecl) d

let build_stmtdecl static extern d =
(* TODO: think about cleaning this location thing up!!! *)
(* for enum decls it seems the location is in double *)
  let build_vdecl l (t, x, loc, init) = 
    (VDecl (x, t, static, extern, init), loc)::l 
  in
  let loc = get_loc () in
  let build_edecl x = (EDecl x, loc) in
  let build_cdecl x = (CDecl x, loc) in
    process_decls (build_edecl, build_cdecl, build_vdecl) d

(* TODO: clean this code and find a way to factor with previous function *)
let build_typedef d =
  let build_vdecl l (t, x, _, _) = 
    Synthack.define_type x t;
    l
  in
  let loc = get_loc () in
  let build_edecl x = (EDecl x, loc) in
  let build_cdecl x = (CDecl x, loc) in
    process_decls (build_edecl, build_cdecl, build_vdecl) d

let normalize_fun_prologue b m =
  let (_, (t, x, loc)) = Synthack.normalize_decl (b, m) in
  let x =
    match x with
      | Some x -> x
      | None -> 
	  (* TODO: code cleanup remove these things !!! *)
	  Npkcontext.error "Firstpass.translate_global" "unknown function name"
  in
    cur_fun := x;
    (t, x, loc)

let build_fundef static ((t, x, loc), body) =
  (FunctionDef (x, t, static, body), loc)::[]

(*
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
*)    
let build_type_decl d =
  let (symbdecls, (t, _, _)) = Synthack.normalize_decl d in
    if (symbdecls <> ([], [])) then begin 
      Npkcontext.error "Parser.build_type_decl" "unexpected enum declaration"
    end;
    t

let flatten_field_decl (b, x) = List.map (fun (v, i) -> (b, v, i)) x

(* TODO: simplify and put in synthack so as to optimize?? *)
let build_funparams params types =
  let has_name x d =
    match Synthack.normalize_decl d with
	(_, (_, Some y, _)) when x = y -> true
      | _ -> false
  in
  let add_param_type x = List.find (has_name x) types in
  List.map add_param_type params

let report_asm tokens =
  let loc = "Parser.report_asm" in
  let tokens = List_utils.to_string (fun x -> x) "' '" tokens in
  let msg = "asm directive '"^tokens^"'" in
    Npkcontext.report_ignore_warning loc msg Npkcontext.Asm

let apply_attrs attrs t =
  match (attrs, t) with
      ([], _) -> t
    | (new_sz::[], Integer (sign, _)) -> Integer (sign, new_sz)
    | (_::[], _) -> 
	Npkcontext.error "Parser.apply_attr" "wrong type, integer expected"
    | _ -> 
	Npkcontext.error "Parser.apply_attr" 
	  "more than one attribute not handled yet"

let rec normalize_bexp e =
  match e with
      Var _ | Field _ | Index _ | Deref _ | Call _ | OpExp _ 
    | Set _ | Str _ -> Unop (Not, Binop (Eq, e, exp_of_int 0))
    | Unop (Not, e) -> Unop (Not, normalize_bexp e)
    | IfExp (c, e1, e2) -> 
	IfExp (normalize_bexp c, normalize_bexp e1, normalize_bexp e2)
    | _ -> e
%}

%token BREAK CONST CONTINUE CASE DEFAULT DO ELSE ENUM STATIC 
%token EXTERN FOR IF REGISTER RETURN SIZEOF VOLATILE
%token SWITCH TYPEDEF WHILE GOTO
%token CHAR DOUBLE FLOAT INT SHORT LONG STRUCT UNION SIGNED UNSIGNED VOID
%token ELLIPSIS COLON COMMA DOT LBRACE RBRACE 
%token LBRACKET RBRACKET LPAREN RPAREN NOT 
%token EQ OREQ SHIFTLEQ SHIFTREQ MINUSEQ PLUSEQ EQEQ NOTEQ STAREQ DIVEQ MODEQ 
%token AMPERSANDEQ
%token SEMICOLON
%token AMPERSAND ARROW AND OR MINUS DIV MOD PLUS MINUSMINUS QMARK
%token PLUSPLUS STAR LT LTEQ GT GTEQ
%token SHIFTL SHIFTR BXOR BOR BNOT
%token ATTRIBUTE EXTENSION VA_LIST FORMAT PRINTF SCANF CDECL NORETURN DLLIMPORT
%token INLINE ALWAYS_INLINE GNU_INLINE ASM CDECL_ATTR FORMAT_ARG RESTRICT 
%token NONNULL DEPRECATED MALLOC NOTHROW PURE BUILTIN_CONSTANT_P MODE 
%token WARN_UNUSED_RESULT QI HI SI DI PACKED FUNNAME TRANSPARENT_UNION TYPEOF
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
    Npkcontext.report_accept_warning "Parser.translation_unit" 
      "unnecessary semicolon" Npkcontext.DirtySyntax;
    $2 
  }
|                                          { [] }
;;

function_prologue:
  declaration_specifiers declarator        { normalize_fun_prologue $1 $2 }
;;

function_definition:
  function_prologue compound_statement      { ($1, $2) }
;;

parameter_declaration_list:
  parameter_declaration SEMICOLON 
  parameter_declaration_list                { $1::$3 }
| parameter_declaration SEMICOLON           { $1::[] }
;;

declaration:
  declaration_specifiers 
  init_declarator_list 
  extended_attribute_list                  { 
    (apply_attrs $3 $1, $2) 
  }
;;

init_declarator_list:
                                           { (Abstract, None)::[] }
| non_empty_init_declarator_list           { $1 }
;;

non_empty_init_declarator_list:
  init_declarator COMMA 
  non_empty_init_declarator_list           { $1::$3 }
| init_declarator                          { $1::[] }
;;

init_declarator:
  declarator                               { ($1, None) }
| declarator EQ init                       { ($1, Some $3) }
;;

declarator:
| pointer declarator                       { Pointer $2 }
| LPAREN declarator RPAREN                 { $2 }
| ident_or_tname                           { Variable ($1, get_loc ()) }
| declarator LBRACKET expression RBRACKET  { Array ($1, Some $3) }
| declarator LBRACKET 
             type_qualifier_list RBRACKET  { Array ($1, None) }
| declarator 
  LPAREN parameter_list RPAREN             { Function ($1, $3) }
| declarator LPAREN RPAREN                 { Function ($1, []) }
| declarator LPAREN identifier_list RPAREN
  parameter_declaration_list               { 
    Npkcontext.report_accept_warning "Parser.declarator"
      "deprecated style of function definition" Npkcontext.DirtySyntax;
    Function ($1, build_funparams $3 $5) 
  }
;;

identifier_list:
  IDENTIFIER COMMA identifier_list         { $1::$3 }
| IDENTIFIER                               { $1::[] }
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
    Npkcontext.report_accept_warning "Parser.struct_declarator"
      "anonymous field declaration in structure" Npkcontext.DirtySyntax;
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

compound_statement:
  LBRACE statement_list RBRACE             { $2 }
;;

statement_list:
  statement statement_list                 { $1@$2 }
|                                          { [] }
;;

// TODO: factor declarations??
statement:
  IDENTIFIER COLON statement               { (Label $1, get_loc ())::$3 }
| declaration SEMICOLON                    { build_stmtdecl false false $1 }
| REGISTER declaration SEMICOLON           { build_stmtdecl false false $2 }
| STATIC declaration SEMICOLON             { build_stmtdecl true false $2 }
| EXTERN declaration SEMICOLON             { build_stmtdecl false true $2 }
| TYPEDEF declaration SEMICOLON            { build_typedef $2 }
| IF LPAREN expression RPAREN statement    { 
    [If (normalize_bexp $3, $5, []), get_loc ()] 
  }
| IF LPAREN expression RPAREN statement
  ELSE statement                           { 
    [If (normalize_bexp $3, $5, $7), get_loc ()] 
  }
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
| asm SEMICOLON                            { [] }
;;

asm:
  ASM LPAREN asm_statement_list RPAREN     { report_asm $3 }
;;

asm_statement_list:
  asm_statement                            { $1::[] }
| asm_statement COLON asm_statement_list   { $1::$3 }
;;

asm_statement:
  string_literal                           { $1 }
| string_literal LPAREN IDENTIFIER RPAREN  { $1 }
;;

// TODO: this could be simplified a lot by following the official grammar
// but there should be another way to issue warnings
iteration_statement:
| FOR LPAREN assignment_expression_list SEMICOLON 
      expression_statement
      assignment_expression_list RPAREN
      statement                            { ($3, normalize_bexp $5, $8, $6) }
| FOR LPAREN SEMICOLON 
      expression_statement
      assignment_expression_list RPAREN
      statement                            { 
	Npkcontext.print_warning "Parser.iteration_statement" 
	  "init statement expected";
	([], normalize_bexp $4, $7, $5) 
      }
| FOR LPAREN assignment_expression_list SEMICOLON 
      expression_statement RPAREN
      statement                            { 
	Npkcontext.print_warning "Parser.iteration_statement" 
	  "increment statement expected";
	($3, normalize_bexp $5, $7, []) 
      }
| FOR LPAREN SEMICOLON expression_statement RPAREN
      statement                            { 
	Npkcontext.print_warning "Parser.iteration_statement" 
	  "init statement expected";
	([], normalize_bexp $4, $6, []) 
      }
| WHILE LPAREN expression RPAREN statement { ([], normalize_bexp $3, $5, []) }
| DO statement
  WHILE LPAREN expression RPAREN SEMICOLON { ($2, normalize_bexp $5, $2, []) }
;;

expression_statement:
  SEMICOLON                                { 
    Npkcontext.print_warning "Parser.expression_statement" 
      "halting condition should be explicit";
    exp_of_int 1
  }
| assignment_expression SEMICOLON           { $1 }
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
| string_literal                           { Str $1 }
| FUNNAME                                  { Str !cur_fun }
| LPAREN expression RPAREN                 { $2 }
;;

string_literal:
  STRING                                   { $1 }
| STRING string_literal                    { $1^$2 }
;;

postfix_expression:
  primary_expression                       { $1 }
| postfix_expression 
  LBRACKET expression RBRACKET             { Index ($1, $3) }
| postfix_expression LPAREN RPAREN         { Call ($1, []) }
| postfix_expression 
  LPAREN argument_expression_list RPAREN   { Call ($1, $3) }
| postfix_expression DOT ident_or_tname    { Field ($1, $3) }
| postfix_expression ARROW ident_or_tname  { Field (Deref $1, $3) }
| postfix_expression PLUSPLUS              { OpExp (Plus, $1, true) }
| postfix_expression MINUSMINUS            { OpExp (Minus, $1, true) }
| BUILTIN_CONSTANT_P 
  LPAREN expression RPAREN                 { 
     Npkcontext.print_warning "Parser.assignment_expression"
       "__builtin_constant_p ignored, assuming value 0";
    exp_of_int 0
  }
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
| LPAREN type_name RPAREN
  composite                                { 
    let loc = get_loc () in
    let t = build_type_decl $2 in
    let decl = (VDecl ("tmp", t, false, false, Some (Sequence $4)), loc) in
    let e = (Exp (Var "tmp"), loc) in
      Npkcontext.report_accept_warning "Parser.cast_expression" 
	"local composite ceation" Npkcontext.DirtySyntax;
      BlkExp (decl::e::[])
  }
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
| EXTENSION 
  LPAREN relational_expression RPAREN      { $3 }
| compound_statement                       { 
    Npkcontext.report_accept_warning "Parser.relational_expression"
      "block within expression" Npkcontext.DirtySyntax;
    BlkExp $1
  }
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
  inclusive_or_expression                  { 
    IfExp (normalize_bexp $1, normalize_bexp $3, exp_of_int 0) 
}
;;

logical_or_expression:
  logical_and_expression                   { $1 }
| logical_or_expression OR
  logical_and_expression                   { 
    IfExp (normalize_bexp $1, exp_of_int 1, normalize_bexp $3) 
  }
;;

conditional_expression:
  logical_or_expression                    { $1 }
| logical_or_expression QMARK 
  expression COLON conditional_expression  {
    Npkcontext.report_strict_warning "Parser.conditional_expression"
      "conditional expression";
    IfExp (normalize_bexp $1, $3, $5)
  }
;;

// I do not want to have expression be assignment_expression
// this would allow assignments within expressions 
// it is error-prone, for instance typos may make you write
// if (x = 0) instead of if (x == 0)
expression:
  assignment_expression                   { $1 }
| expression COMMA assignment_expression  { 
    Npkcontext.report_accept_warning "Parser.expression"
      "comma in expression" Npkcontext.DirtySyntax;
    let loc = get_loc () in
      BlkExp ((Exp $1, loc)::(Exp $3, loc)::[]) 
  }
;;

assignment_expression:
  conditional_expression                   { $1 }
| unary_expression 
  EQ assignment_expression                 { Set ($1, None, $3) }
| unary_expression assignment_operator
  assignment_expression                    { Set ($1, Some $2, $3) }
| EXTENSION 
  LPAREN assignment_expression RPAREN      { $3 }
;;

assignment_operator:
  PLUSEQ                                   { Plus }
| MINUSEQ                                  { Minus }
| STAREQ                                   { Mult }
| DIVEQ                                    { Div }
| MODEQ                                    { Mod }
| OREQ                                     { BOr }
| AMPERSANDEQ                              { BAnd }
| SHIFTLEQ                                 { Shiftl }
| SHIFTREQ                                 { Shiftr }
;;

argument_expression_list:
  expression                               { $1::[] }
| expression 
  COMMA argument_expression_list           { $1::$3 }
;;

init:
  assignment_expression                    { Data $1 }
| composite                                { Sequence $1 }
;;

composite:
  LBRACE init_list RBRACE                  { $2 }
| LBRACE named_init_list RBRACE            { $2 }
;;

named_init_list:
  named_init COMMA named_init_list         { $1::$3 }
| named_init                               { $1::[] }
;;

named_init:
  DOT IDENTIFIER EQ 
  assignment_expression                    { (Some $2, Data $4) }
;;

init_list:
  init COMMA init_list                     { (None, $1)::$3 }
| init                                     { (None, $1)::[] }
|                                          {
    Npkcontext.report_strict_warning "Parser.init_list"
      "comma terminated initializer";
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
  gnuc_field_declaration SEMICOLON 
  field_list                               { $1@$3 } 
| gnuc_field_declaration SEMICOLON         { $1 }
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
| enum COMMA                             { 
    Npkcontext.report_strict_warning "Parser.enum_list"   
      "unnecessary comma";  
    $1::[] 
  }
;;

enum:
  IDENTIFIER                             { ($1, None) }
| IDENTIFIER EQ assignment_expression    { ($1, Some $3) }
;;

field_blk:
  LBRACE field_list RBRACE               { $2 }
| LBRACE RBRACE                          { 
    Npkcontext.report_accept_warning "Parser.field_blk"
      "empty struct or union" Npkcontext.DirtySyntax;
    [] 
  }
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
    Npkcontext.report_strict_warning "Parser.type_specifier" 
      "signed specifier not necessary";
    Integer (Newspeak.Signed, $2)
  }
| UNSIGNED ityp                          { Integer (Newspeak.Unsigned, $2) }
| UNSIGNED                               { 
    Npkcontext.report_accept_warning "Parser.type_specifier"
      "unspecified integer kind" Npkcontext.DirtySyntax;
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
| ENUM LBRACE enum_list RBRACE           { Enum (Some $3) }
| ENUM IDENTIFIER                        { Enum None }
| ENUM IDENTIFIER 
  LBRACE enum_list RBRACE                { Enum (Some $4) }
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
| TYPEOF LPAREN IDENTIFIER RPAREN        { Typeof $3 }
| VA_LIST                                { Va_arg }
;;


//Section that is dependent on version of the compiler (standard ANSI or GNU)
//TODO: find a way to factor some of these, possible!!!
external_declaration:
  STATIC declaration SEMICOLON             { build_glbdecl (true, false) $2 }
| function_definition                      { build_fundef false $1 }
| STATIC function_definition               { build_fundef true $2 }
// GNU C extension
| optional_extension 
  EXTERN function_definition               { 
    Npkcontext.report_accept_warning "Parser.external_declaration" 
      "extern function definition" Npkcontext.DirtySyntax;
    build_fundef false $3
}
| optional_extension TYPEDEF 
  declaration SEMICOLON                    { build_glbtypedef $3 }
| EXTENSION declaration SEMICOLON          { build_glbdecl (false, false) $2 }
| declaration SEMICOLON                    { build_glbdecl (false, false) $1 }
| optional_extension
  EXTERN declaration SEMICOLON             { build_glbdecl (false, true) $3 }
| optional_extension
  EXTERN declaration SEMICOLON             { build_glbdecl (false, true) $3 }
;;

optional_extension:
  EXTENSION                                { }
|                                          { }
;;

attribute_list:
  attribute attribute_list                 { $1@$2 }
|                                          { [] }
;;

extended_attribute_list:
  attribute extended_attribute_list        { $1@$2 }
| asm extended_attribute_list              { $2 }
|                                          { [] }
;;

type_qualifier:
  CONST                                    { }
| attribute                                { }
| VOLATILE                                 { 
    Npkcontext.report_ignore_warning "Parser.type_qualifier" 
      "type qualifier 'volatile'" Npkcontext.Volatile;
    }
;;

gnuc_field_declaration:
// GNU C extension
  optional_extension field_declaration 
  attribute_list                           { $2 }
;;

field_declaration:
  declaration_specifiers
  struct_declarator_list                   { flatten_field_decl ($1, $2) }
| declaration_specifiers                   { 
    Npkcontext.report_accept_warning "Parser.field_declaration"
      "anonymous field declaration in structure" Npkcontext.DirtySyntax;
    flatten_field_decl ($1, (Abstract, None)::[]) 
  }
;;

attribute:
  ATTRIBUTE LPAREN LPAREN attribute_name 
  RPAREN RPAREN                            { $4 }
| INLINE                                   { [] }
| CDECL                                    { [] }
| RESTRICT                                 { [] }
;;

attribute_name:
  DLLIMPORT                                {
    Npkcontext.print_warning "Parser.attribute" 
      "ignoring attribute dllimport";
    []
  }
| CDECL_ATTR                               { [] }
| NORETURN                                 { [] }
| ALWAYS_INLINE                            { [] }
| NOTHROW                                  { [] }
| PURE                                     { [] }
| DEPRECATED                               { [] }
| MALLOC                                   { [] }
| FORMAT LPAREN 
    format_fun COMMA INTEGER COMMA INTEGER 
  RPAREN                                   { [] }
| FORMAT_ARG LPAREN INTEGER RPAREN         { [] }
| NONNULL LPAREN integer_list RPAREN       { [] }
| CONST                                    { [] }
| GNU_INLINE                               { [] }
| WARN_UNUSED_RESULT                       { [] }
| PACKED                                   { 
    Npkcontext.report_ignore_warning "Parser.attribute_name" 
      "packed attribute" Npkcontext.Pack;
    []
  }
| TRANSPARENT_UNION                        { 
    Npkcontext.report_ignore_warning "Parser.attribute_name" 
      "transparent union" Npkcontext.TransparentUnion;
    [];
  }
| MODE LPAREN imode RPAREN                 { $3::[] }
;;

integer_list:
  INTEGER                                  { }
| INTEGER COMMA integer_list               { }
;;

format_fun:
  PRINTF                                   { }
| SCANF                                    { }
;;

imode:
  QI                                       { Config.size_of_byte }
| HI                                       { Config.size_of_byte*2 }
| SI                                       { Config.size_of_byte*4 }
| DI                                       { Config.size_of_byte*8 }
;;
