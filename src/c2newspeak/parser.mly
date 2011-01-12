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
open Synthack

let struct_cnt = ref 0

let new_id =
  let c = ref 0 in
  fun _ ->
    incr c;
    !c

let gen_struct_id () = 
  incr struct_cnt;
  "anon_struct"^(string_of_int !struct_cnt)
  
(* TODO: write checks for all the syntax that is thrown away in these functions
   !! *)

let get_loc () =
  let pos = Parsing.symbol_start_pos () in
    (pos.pos_fname, pos.pos_lnum, pos.pos_cnum-pos.pos_bol)

let apply_attrs attrs t =
  match (attrs, t) with
      ([], _) -> t
    | (new_sz::[], Int (sign, _)) -> Int (sign, new_sz)
    | (_::[], _) -> 
	Npkcontext.report_error "Parser.apply_attr" 
	  "wrong type, integer expected"
    | _ -> 
	Npkcontext.report_error "Parser.apply_attr" 
	  "more than one attribute not handled yet"

(* TODO: simplify by having just a function build_decl??? *)
let process_decls (build_sdecl, build_vdecl) (b, m) =
  let (sdecls, b) = Synthack.normalize_base_typ b in
  let build_vdecl ((v, attrs), init) res =
    let b = apply_attrs attrs b in
    let (t, x, loc) = Synthack.normalize_var_modifier b v in
      match x with
	| None -> res
	| Some x -> build_vdecl res (t, x, loc, init)
  in
  let sdecls = List.map build_sdecl sdecls in
  let vdecls = List.fold_right build_vdecl m [] in
    sdecls@vdecls
      
let build_glbdecl (static, extern) d =
  let build_vdecl l (t, x, loc, init) = 
    let d = 
      { t = t; is_static = static; is_extern = extern; initialization = init }
    in
    (GlbDecl (x, VDecl d), loc)::l
  in
  let loc = get_loc () in
  let build_sdecl x = (GlbDecl x, loc) in
    process_decls (build_sdecl, build_vdecl) d

(* TODO: clean this code and find a way to factor with previous function *)
let build_glbtypedef d =
  let build_vdecl l (t, x, _, _) = 
    Synthack.define_type x t;
    l
  in
  let loc = get_loc () in
  let build_sdecl x = (GlbDecl x, loc) in
    process_decls (build_sdecl, build_vdecl) d

let build_stmtdecl static extern d =
(* TODO: think about cleaning this location thing up!!! *)
(* for enum decls it seems the location is in double *)
  let build_vdecl l (t, x, loc, init) = 
(* TODO: factor the various VDecl creations!! *)
    let d = 
      { t = t; is_static = static; is_extern = extern; initialization = init }
    in
      (LocalDecl (x, VDecl d), loc)::l 
  in
  let loc = get_loc () in
  let build_sdecl x = (LocalDecl x, loc) in
    process_decls (build_sdecl, build_vdecl) d

(* TODO: clean this code and find a way to factor with previous function *)
let build_typedef d =
  let build_vdecl l (t, x, _, _) = 
    Synthack.define_type x t;
    l
  in
  let loc = get_loc () in
  let build_sdecl x = (LocalDecl x, loc) in
    process_decls (build_sdecl, build_vdecl) d

(* TODO: remove code?? *)
let normalize_fun_prologue b m =
  let (_, (t, x, loc)) = Synthack.normalize_decl (b, m) in
  let x =
    match x with
      | Some x -> x
      | None -> 
	  (* TODO: code cleanup remove these things !!! *)
	  Npkcontext.report_error "Firstpass.translate_global" 
	    "unknown function name"
  in
    (t, x, loc)

let build_fundef static ((b, m), body) =
  let (_, (t, x, loc)) = Synthack.normalize_decl (b, m) in
  let x =
    match x with
      | Some x -> x
      | None -> 
	  (* TODO: code cleanup remove these things !!! *)
	  Npkcontext.report_error "Firstpass.translate_global" 
	    "unknown function name"
  in
  let t = Csyntax.ftyp_of_typ t in
    (FunctionDef (x, t, static, body), loc)::[]

let build_type_decl d =
  let (sdecls, (t, _, _)) = Synthack.normalize_decl d in
    if (sdecls <> []) then begin 
      Npkcontext.report_error "Parser.build_type_decl" 
	"unexpected enum or composite declaration"
    end;
    t

let build_type_blk loc d =
  let (sdecls, (t, _, _)) = Synthack.normalize_decl d in
  let sdecls = List.map (fun x -> (LocalDecl x, loc)) sdecls in
    (sdecls, t)

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
  let tokens = ListUtils.to_string (fun x -> x) "' '" tokens in
  let msg = "asm directive '"^tokens^"'" in
    Npkcontext.report_ignore_warning loc msg Npkcontext.Asm
%}

%token BREAK CONST CONTINUE CASE DEFAULT DO ELSE ENUM STATIC 
%token EXTERN FOR IF REGISTER AUTO RETURN VOLATILE 
%token SWITCH TYPEDEF WHILE GOTO
%token CHAR DOUBLE FLOAT INT SHORT LONG STRUCT UNION SIGNED UNSIGNED VOID
%token ELLIPSIS COLON COMMA DOT LBRACE RBRACE 
%token LBRACKET RBRACKET LPAREN RPAREN NOT 
%token EQEQ NOTEQ
%token EQ OREQ SHIFTLEQ SHIFTREQ MINUSEQ PLUSEQ STAREQ DIVEQ MODEQ BXOREQ AMPERSANDEQ
%token SEMICOLON
%token AMPERSAND ARROW AND OR MINUS DIV MOD PLUS MINUSMINUS QMARK
%token PLUSPLUS STAR LT LTEQ GT GTEQ
%token SHIFTL SHIFTR BXOR BOR BNOT
%token ATTRIBUTE EXTENSION VA_LIST CDECL
%token INLINE ASM RESTRICT 
%token BUILTIN_CONSTANT_P
%token FUNNAME 
%token OFFSETOF SIZEOF TYPEOF
%token EOF

%token <Csyntax.assertion> NPK
%token <char> SYMBOL

%token <string> IDENTIFIER
%token <string> TYPEDEF_NAME
%token <string> STRING
%token <string option * string * char option * string option> INTEGER
%token <int> CHARACTER
%token <string * char option> FLOATCST

%nonassoc below_ELSE
%nonassoc ELSE

%right    EQ PLUSEQ MINUSEQ STAREQ DIVEQ MODEQ OREQ AMPERSANDEQ SHIFTLEQ SHIFTREQ BXOREQ
%right    QMARK
%left     OR
%left     AND
%left     BOR
%left     BXOR
%left     AMPERSAND
%left     EQEQ NOTEQ
%left     GT GTEQ LT LTEQ
%left     SHIFTL SHIFTR
%left     PLUS MINUS
%left     STAR DIV MOD
%nonassoc prefix_OP
%right    PLUSPLUS MINUSMINUS
%left     DOT ARROW
%left     LPAREN LBRACKET

%type <Csyntax.t> parse
%start parse

%type <Csyntax.assertion> assertion
%start assertion

%%
/* TODO: simplify code by generalizing!!! 
try to remove multiple occurence of same pattern: factor as much as possible
*/
// carefull not to have any empty rule: this deceives line number location

// TODO: simplify parser and link it to C standard sections!!!

parse:
  translation_unit EOF                      { ($1) }
;;

translation_unit:
  NPK translation_unit                      { (GlbUserSpec $1, get_loc ())::$2 }
| external_declaration translation_unit     { $1@$2 }
| SEMICOLON translation_unit                { 
    Npkcontext.report_accept_warning "Parser.translation_unit" 
      "unnecessary semicolon" Npkcontext.DirtySyntax;
    $2 
  }
|                                           { [] }
;;

function_prologue:
  declaration_specifiers
  function_declarator                       { ($1, $2) }
;;


function_declarator:
  pointer direct_declarator                { 
    let (ptr, decl) = $2 in
      (ptr+$1, decl)
  }
| direct_declarator                        { $1 }
| pointer direct_declarator 
      LPAREN identifier_list RPAREN
      old_parameter_declaration_list       { 
    Npkcontext.report_accept_warning "Parser.declarator"
      "deprecated style of function definition" Npkcontext.DirtySyntax;
	($1, Function ($2, build_funparams $4 $6))
  }
| direct_declarator 
      LPAREN identifier_list RPAREN
      old_parameter_declaration_list       { 
    Npkcontext.report_accept_warning "Parser.declarator"
      "deprecated style of function definition" Npkcontext.DirtySyntax;
    (0, Function ($1, build_funparams $3 $5))
  }
;;


function_definition:
  function_prologue compound_statement      { ($1, $2) }
;;

declaration:
  declaration_specifiers 
  init_declarator_list                      { ($1, $2) }
| typeof_declaration 			    { $1 }
;;

typeof_declaration:
type_qualifier_list TYPEOF LPAREN 
type_specifier pointer RPAREN 
type_qualifier_list ident_or_tname { ($4, [( ($5, Variable ($8, get_loc ())), []) , None])}
;;
init_declarator_list:
                                            { (((0, Abstract), []), None)::[] }
| non_empty_init_declarator_list            { $1 }
;;


non_empty_init_declarator_list:
  init_declarator COMMA 
  non_empty_init_declarator_list            { $1::$3 }
| init_declarator                           { $1::[] }
;;


init_declarator:
  attr_declarator                           { ($1, None) }
| attr_declarator EQ init                   { ($1, Some $3) }
;;


attr_declarator:
  declarator extended_attribute_list        { ($1, $2) }
;;

declarator:
  pointer direct_declarator                { 
    let (ptr, decl) = $2 in
      (ptr+$1, decl)
  }
| direct_declarator                        { $1 }
;;

direct_declarator:
  ident_or_tname                           { (0, Variable ($1, get_loc ())) }
| LPAREN declarator RPAREN                 { $2 }
| direct_declarator LBRACKET 
      expression_sequence RBRACKET         { (0, Array ($1, Some $3)) }
| direct_declarator LBRACKET 
      type_qualifier_list RBRACKET         { (0, Array ($1, None)) }
| direct_declarator 
  LPAREN parameter_list RPAREN             { (0, Function ($1, $3)) }
| direct_declarator LPAREN RPAREN          { (0, Function ($1, [])) }
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
| declarator COLON expression              { ($1, Some $3) }
| COLON expression                         { 
    Npkcontext.report_accept_warning "Parser.struct_declarator"
      "anonymous field declaration in structure" Npkcontext.DirtySyntax;
    ((0, Abstract), Some $2) 
  }
;;

old_parameter_declaration_list:
  old_parameter_declaration 
  old_parameter_declaration_list            { $1@$2 }
| old_parameter_declaration                 { $1 }
;;

old_parameter_declaration:
  declaration SEMICOLON                     { 
    let (b, m) = $1 in
    let normalize_param ((m, attr), init) =
      match init with
	  None when attr = [] -> (b, m)
	| _ -> 
	    Npkcontext.report_error "Parser.old_parameter_declaration"
	      "parameter can not be initialized"
    in
      List.map normalize_param m
  }
;;

// TODO: careful, this is a bit of a hack
parameter_declaration:
  declaration_specifiers declarator        { ($1, $2) }
| declaration_specifiers 
  abstract_declarator                      { ($1, $2) }
| declaration_specifiers                   { ($1, (0, Abstract)) }
;;

type_name:
  declaration_specifiers                   { ($1, (0, Abstract)) }
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

statement:
  IDENTIFIER COLON statement               { (Label $1, get_loc ())::$3 }
| declaration SEMICOLON                    { build_stmtdecl false false $1 }
| STATIC declaration SEMICOLON             { build_stmtdecl true false $2 }
| EXTERN declaration SEMICOLON             { build_stmtdecl false true $2 }
| TYPEDEF declaration SEMICOLON            { build_typedef $2 }
| IF LPAREN expression_sequence RPAREN
      statement           %prec below_ELSE {
    [If (normalize_bexp $3, $5, []), get_loc ()] 
  }
| IF LPAREN expression_sequence RPAREN statement
  ELSE statement                           { 
    [If (normalize_bexp $3, $5, $7), get_loc ()] 
  }
| switch_stmt                              { [CSwitch $1, get_loc ()] }
| iteration_statement                      { [$1, get_loc ()] }
| RETURN expression_sequence SEMICOLON              { 
    let loc = get_loc () in
      (Exp (Set (RetVar, None, $2)), loc)::(Return, loc)::[]
  }
| RETURN SEMICOLON                         { [Return, get_loc ()] }
| expression_sequence SEMICOLON            { [Exp $1, get_loc ()] }
| BREAK SEMICOLON                          { [Break, get_loc ()] }
| CONTINUE SEMICOLON                       { [Continue, get_loc ()] }
| GOTO IDENTIFIER SEMICOLON                { 
    Npkcontext.report_accept_warning "Parser.statement" "goto statement"
      Npkcontext.ForwardGoto;
    [Goto $2, get_loc ()] 
  }
| compound_statement                       { [Block $1, get_loc ()] }
| SEMICOLON                                { [] }
| asm SEMICOLON                            { [] }
| NPK                                      { (UserSpec $1, get_loc ())::[] }
;;

asm:
  ASM LPAREN asm_statement_list RPAREN     { report_asm $3 }
| ASM VOLATILE 
  LPAREN asm_statement_list RPAREN         { report_asm $4 }
;;

asm_statement_list:
  asm_statement                            { $1::[] }
| asm_statement COLON asm_statement_list   { $1::$3 }
| COLON asm_statement_list                 { $2 }
| asm_statement COMMA asm_statement_list   { $1::$3 }
;;

asm_statement:
  string_literal                           { $1 }
| string_literal LPAREN expression RPAREN  { $1 } 
| LBRACKET ident_or_tname RBRACKET string_literal LPAREN expression RPAREN { $2^" "^$4 }
;;

// TODO: this could be simplified a lot by following the official grammar
// but there should be another way to issue warnings
iteration_statement:
| FOR LPAREN assignment_expression_list SEMICOLON 
      expression_statement
      assignment_expression_list RPAREN
      statement                            { For($3, normalize_bexp $5, $8, $6) }
| FOR LPAREN SEMICOLON 
      expression_statement
      assignment_expression_list RPAREN
      statement                            { 
	Npkcontext.report_warning "Parser.iteration_statement" 
	  "init statement expected";
	For([], normalize_bexp $4, $7, $5) 
      }
| FOR LPAREN assignment_expression_list SEMICOLON 
      expression_statement RPAREN
      statement                            { 
	Npkcontext.report_warning "Parser.iteration_statement" 
	  "increment statement expected";
	For($3, normalize_bexp $5, $7, []) 
      }
| FOR LPAREN SEMICOLON expression_statement RPAREN
      statement                            { 
	Npkcontext.report_warning "Parser.iteration_statement" 
	  "init statement expected";
	For([], normalize_bexp $4, $6, []) 
      }
| WHILE LPAREN expression_sequence RPAREN 
  statement                                { For ([], normalize_bexp $3, $5, [])
					   }
| DO statement
  WHILE LPAREN expression_sequence 
  RPAREN SEMICOLON                         { DoWhile ($2, normalize_bexp $5) }
;;

expression_statement:
  SEMICOLON                                { 
    Npkcontext.report_warning "Parser.expression_statement" 
      "halting condition should be explicit";
    exp_of_int 1
  }
| expression SEMICOLON                     { $1 }
;;

switch_stmt:
  SWITCH LPAREN expression_sequence RPAREN LBRACE
    case_list
  RBRACE                                   { 
    let (cases, default) = $6 in
      (* TODO: all bexp normalizations should be done after typing and 
	 before firstpass!!!
	 should remove normalize_bexp from csyntax!!!
      *)
      ($3, cases, default) 
  }
;;

case_list:
  CASE expression_sequence COLON statement_list 
  case_list                                { 
    let (cases, default) = $5 in
      (($2, $4, get_loc ())::cases, default)
  }
| DEFAULT COLON statement_list case_list   { 
    let (cases, _) = $4 in
      if cases <> [] then begin
	Npkcontext.report_accept_warning "Parser.case_list" 
	  "switch with default case in intermediary position" 
	  Npkcontext.DirtySyntax
      end;
      (cases, $3)
  }
|                                          { ([], []) }
;;

assignment_expression_list:
  expression COMMA 
  assignment_expression_list               { (Exp $1, get_loc ())::$3 }
| expression                               { (Exp $1, get_loc ())::[] }
;;

constant:
  CHARACTER                                { Csyntax.char_cst_of_lexeme $1 }
| INTEGER                                  { Csyntax.int_cst_of_lexeme $1 }
| FLOATCST                                 { Csyntax.float_cst_of_lexeme $1 }
;;

string_literal:
  STRING                                   { $1 }
| STRING string_literal                    { $1^$2 }
;;

expression:
  IDENTIFIER                               { Var $1 }
| constant                                 { Cst $1 }
| string_literal                           { Str $1 }
| FUNNAME                                  { FunName }
| LPAREN expression_sequence RPAREN        { $2 }
| LPAREN compound_statement RPAREN         { 
    Npkcontext.report_accept_warning "Parser.relational_expression"
      "block within expression" Npkcontext.DirtySyntax;
    BlkExp $2
  }
| expression 
  LBRACKET expression_sequence RBRACKET    { Index ($1, $3) }
| expression 
  LPAREN argument_expression_list RPAREN   { Call ($1, $3) }
| expression DOT ident_or_tname            { Field ($1, $3) }
| expression ARROW ident_or_tname          { Field (Index ($1, exp_of_int 0),
						    $3) }
| expression PLUSPLUS                      { OpExp (Plus, $1, true) }
| expression MINUSMINUS                    { OpExp (Minus, $1, true) }
// GNU C
| BUILTIN_CONSTANT_P 
  LPAREN expression_sequence RPAREN        { 
     Npkcontext.report_warning "Parser.assignment_expression"
       "__builtin_constant_p ignored, assuming value 0";
    exp_of_int 0
  }
| OFFSETOF 
  LPAREN type_name COMMA IDENTIFIER RPAREN { Offsetof (build_type_decl $3, $5) }
| PLUSPLUS   expression    %prec prefix_OP { OpExp (Plus, $2, false) }
| MINUSMINUS expression    %prec prefix_OP { OpExp (Minus, $2, false) }
| AMPERSAND  expression    %prec prefix_OP { AddrOf $2 }
| STAR       expression    %prec prefix_OP { Index ($2, exp_of_int 0) }
// TODO: factor these with unop non-terminal
| BNOT       expression    %prec prefix_OP { Unop (BNot, $2) }
| NOT        expression    %prec prefix_OP { Unop (Not, $2) }
| MINUS      expression    %prec prefix_OP { Csyntax.neg $2 }
| PLUS       expression                    { $2 }
| SIZEOF     expression    %prec prefix_OP { SizeofE $2 }
| SIZEOF LPAREN type_name RPAREN 
                           %prec prefix_OP { Sizeof (build_type_decl $3) }
| EXTENSION expression     %prec prefix_OP { $2 }
| LPAREN type_name RPAREN expression
                           %prec prefix_OP { Cast ($4, build_type_decl $2) }
| LPAREN type_name RPAREN composite        { 
    let loc = get_loc () in
    let (blk, t) = build_type_blk loc $2 in
    let d = 
      { 
	t = t; is_static = false; is_extern = false; 
	initialization = Some (Sequence $4) 
      } 
    in
    let id = Temps.to_string (new_id ()) (Temps.Misc "parser") in
    let decl = (LocalDecl (id, VDecl d), loc) in
    let e = (Exp (Var id), loc) in
      Npkcontext.report_accept_warning "Parser.cast_expression" 
	"local composite creation" Npkcontext.DirtySyntax;
      BlkExp (blk@decl::e::[])
  }
// TODO: factor these as binop non-terminal??
| expression STAR      expression          { Binop (Mult, $1, $3) }
| expression DIV       expression          { Binop (Div, $1, $3) }
| expression MOD       expression          { Binop (Mod, $1, $3) }
| expression PLUS      expression          { Binop (Plus, $1, $3) }
| expression MINUS     expression          { Binop (Minus, $1, $3) }
| expression SHIFTL    expression          { Binop (Shiftl, $1, $3) }
| expression SHIFTR    expression          { Binop (Shiftr, $1, $3) }
| expression GT        expression          { Binop (Gt, $1, $3) }
| expression GTEQ      expression          { Unop (Not, Binop (Gt, $3, $1)) }
| expression LT        expression          { Binop (Gt, $3, $1) }
| expression LTEQ      expression          { Unop (Not, Binop (Gt, $1, $3)) }
| expression EQEQ      expression          { Binop (Eq, $1, $3) }
| expression NOTEQ     expression          { Unop (Not, Binop (Eq, $1, $3)) }
| expression AMPERSAND expression          { Binop (BAnd, $1, $3) }
| expression BXOR      expression          { Binop (BXor, $1, $3) }
| expression BOR       expression          { Binop (BOr, $1, $3) }
| expression AND       expression          { 
    IfExp (normalize_bexp $1, normalize_bexp $3, exp_of_int 0) 
  }
| expression OR expression                 { 
    IfExp (normalize_bexp $1, exp_of_int 1, normalize_bexp $3) 
  }
| expression QMARK expression_sequence
    COLON expression           %prec QMARK {
	Npkcontext.report_strict_warning "Parser.expression"
	  "conditional expression";
	IfExp (normalize_bexp $1, $3, $5)
  }

| expression QMARK COLON expression           %prec QMARK {
    let e = normalize_bexp $1 in
    let loc = get_loc () in
    let t = Csyntax.Typeof e in
    let d = 
      {
	t = t; is_static = false; is_extern = false;
	initialization = Some (Data e)
      }
    in
    let id = Temps.to_string (new_id ()) (Temps.Misc "parser") in
    let decl = (LocalDecl (id, VDecl d), loc) in
    let e' = Var id in
      BlkExp( [ decl; ( Exp (IfExp(e', e', $4)), loc ) ] )
  }

| expression assignment_operator
                   expression     %prec EQ { Set ($1, $2, $3) }
;;

expression_sequence:
  expression                               { $1 }
| expression_sequence COMMA expression     { 
    Npkcontext.report_accept_warning "Parser.expression"
      "comma in expression" Npkcontext.DirtySyntax;
    let loc = get_loc () in
      BlkExp ((Exp $1, loc)::(Exp $3, loc)::[])
  }
;;

assignment_operator:
| EQ                                       { None }
| assignment_op_operator                   { Some $1 }
;;

assignment_op_operator:
  PLUSEQ                                   { Plus }
| MINUSEQ                                  { Minus }
| STAREQ                                   { Mult }
| DIVEQ                                    { Div }
| MODEQ                                    { Mod }
| OREQ                                     { BOr }
| AMPERSANDEQ                              { BAnd }
| SHIFTLEQ                                 { Shiftl }
| SHIFTREQ                                 { Shiftr }
| BXOREQ                                   { BXor }
;;

argument_expression_list:
                                           { [] }
| nonempty_argument_expression_list        { $1 }
;;

nonempty_argument_expression_list:
  expression                               { $1::[] }
| expression 
  COMMA nonempty_argument_expression_list  { $1::$3 }
;;

init:
  expression                               { Data $1 }
| composite                                { Sequence $1 }
;;

composite:
  LBRACE init_list RBRACE                  { $2 }
| LBRACE named_init_list RBRACE            { $2 }
;;

named_init_list:
  named_init COMMA named_init_list         { $1::$3 }
| named_init                               { $1::[] }
| named_init COMMA                         { $1::[] }
;;

named_init:
  DOT IDENTIFIER EQ expression             { (Some $2, Data $4) }
| DOT IDENTIFIER EQ LBRACE init_list RBRACE { (Some $2, Sequence $5) }
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
  pointer                                  { ($1, Abstract) }
| direct_abstract_declarator               { $1 }
| pointer direct_abstract_declarator       { 
    let (ptr, decl) = $2 in
      ($1+ptr, decl) 
  }
;;

direct_abstract_declarator:
  LPAREN abstract_declarator RPAREN        { $2 }
| LBRACKET type_qualifier_list RBRACKET    { (0, Array ((0, Abstract), None)) }
| LBRACKET expression_sequence RBRACKET    { 
    (0, Array ((0, Abstract), Some $2)) 
  }
| direct_abstract_declarator 
  LBRACKET expression_sequence RBRACKET    { (0, Array ($1, Some $3)) }
| direct_abstract_declarator 
  LPAREN parameter_list RPAREN             { (0, Function ($1, $3)) }
| direct_abstract_declarator LPAREN RPAREN { (0, Function ($1, [])) }
;;

pointer:
  STAR type_qualifier_list                 { 1 }
| STAR type_qualifier_list pointer         { $3 + 1 }
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
      (Va_arg, (0, Variable ("__builtin_newspeak_va_arg", loc)))::[] 
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
    Npkcontext.report_warning "Parser.ident_or_tname" 
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
| IDENTIFIER EQ expression               { ($1, Some $3) }
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
| LONG LONG UNSIGNED INT                 {
    Npkcontext.report_strict_warning "Parser.type_specifier"
      ("'long long unsigned int' is not normalized : "
      ^"use 'unsigned long long int' instead");
    Integer (Newspeak.Unsigned, Config.size_of_longlong)
  }
| UNSIGNED ityp                          { Integer (Newspeak.Unsigned, $2) }
| UNSIGNED                               { 
    Npkcontext.report_strict_warning "Parser.type_specifier"
      "unspecified integer kind";
    Integer (Newspeak.Unsigned, Config.size_of_int) 
  }

| LONG SIGNED INT                        {
  Npkcontext.report_strict_warning "Parser.type_specifier" 
      ("'long signed int' is not normalized: "
       ^"use 'signed long int' instead");
    Integer (Newspeak.Signed, Config.size_of_long)
      }

| LONG SIGNED                            {
  Npkcontext.report_strict_warning "Parser.type_specifier" 
      ("'long signed' is not normalized: "
       ^"use 'signed long int' instead");
    Integer (Newspeak.Signed, Config.size_of_long)
      }

| LONG UNSIGNED INT                        {
  Npkcontext.report_strict_warning "Parser.type_specifier" 
      ("'long unsigned int' is not normalized: "
       ^"use 'unsigned long int' instead");
    Integer (Newspeak.Unsigned, Config.size_of_long)
  }

| LONG UNSIGNED                            {
  Npkcontext.report_strict_warning "Parser.type_specifier" 
      ("'long unsigned' is not normalized: "
       ^"use 'unsigned long int' instead");
    Integer (Newspeak.Unsigned, Config.size_of_long)
  }

| SHORT SIGNED INT                        {
  Npkcontext.report_strict_warning "Parser.type_specifier" 
      ("'short signed int' is not normalized: "
       ^"use 'signed short int' instead");
    Integer (Newspeak.Signed, Config.size_of_short)
      }

| SHORT SIGNED                            {
  Npkcontext.report_strict_warning "Parser.type_specifier" 
      ("'short signed' is not normalized: "
       ^"use 'signed short int' instead");
    Integer (Newspeak.Signed, Config.size_of_short)
      }

| SHORT UNSIGNED INT                        {
  Npkcontext.report_strict_warning "Parser.type_specifier" 
      ("'short unsigned int' is not normalized: "
       ^"use 'unsigned short int' instead");
    Integer (Newspeak.Unsigned, Config.size_of_short)
  }

| SHORT UNSIGNED                            {
  Npkcontext.report_strict_warning "Parser.type_specifier" 
      ("'short unsigned' is not normalized: "
       ^"use 'unsigned short int' instead");
    Integer (Newspeak.Unsigned, Config.size_of_short)
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
| TYPEOF LPAREN type_specifier RPAREN    { $3 }
| TYPEOF LPAREN IDENTIFIER RPAREN        { Typeof $3 }
| VA_LIST                                { Va_arg }
;;


//Section that is dependent on version of the compiler (standard ANSI or GNU)
//TODO: find a way to factor some of these, possible!!!
external_declaration:
  STATIC declaration SEMICOLON             { build_glbdecl (true, false) $2 }
| function_definition                      { build_fundef false $1 }
| STATIC function_definition               { build_fundef true $2 }
| INLINE STATIC function_definition        { build_fundef true $3 }
| ATTRIBUTE LPAREN LPAREN attribute_name_list 
  RPAREN RPAREN STATIC function_definition { build_fundef true $8 }
// GNU C extension
| optional_extension 
  EXTERN function_definition               {
    Npkcontext.report_ignore_warning "Parser.external_declaration" 
      "extern function definition" Npkcontext.ExternFunDef;
    let ((b, m), _) = $3 in
      build_glbdecl (false, false) (b, ((m, []), None)::[])
}
| optional_extension TYPEDEF 
  declaration SEMICOLON                    { build_glbtypedef $3 }
| EXTENSION declaration SEMICOLON          { build_glbdecl (false, false) $2 }
| declaration SEMICOLON                    { build_glbdecl (false, false) $1 }
| optional_extension
  EXTERN declaration SEMICOLON             { build_glbdecl (false, true) $3 }
| asm SEMICOLON                            { [] }
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
| AUTO                                     { }
| REGISTER                                 { }
;;

gnuc_field_declaration:
// GNU C extension
  optional_extension field_declaration     { $2 }
;;

field_declaration:
  declaration_specifiers
  struct_declarator_list attribute_list    { flatten_field_decl ($1, $2) }
| declaration_specifiers                   { 
    Npkcontext.report_accept_warning "Parser.field_declaration"
      "anonymous field declaration in structure" Npkcontext.DirtySyntax;
    flatten_field_decl ($1, ((0, Abstract), None)::[]) 
  }
;;

attribute:
  ATTRIBUTE LPAREN LPAREN attribute_name_list 
  RPAREN RPAREN                            { $4 }
| INLINE                                   { [] }
| CDECL                                    { [] }
| RESTRICT                                 { [] }
;;

attribute_name_list:
  attribute_name COMMA attribute_name_list { $1@$3 }
| attribute_name                           { $1 }
;;

attribute_name:
  IDENTIFIER                               { 
    begin match $1 with
	"aligned" | "__aligned__" | "__cdecl__" | "noreturn" | "__noreturn__"
      | "__always_inline__" | "always_inline"  | "__nothrow__" 
      | "__pure__" | "pure" | "__gnu_inline__"
      | "__deprecated__" | "deprecated" | "__malloc__" 
      | "__warn_unused_result__" | "warn_unused_result"
      | "__unused__" | "unused" 
      | "__artificial__" | "__cold__" | "cold" -> ()
      | "dllimport" -> 
	  Npkcontext.report_warning "Parser.attribute" 
	    "ignoring attribute dllimport"
      | "packed" | "__packed__" -> 
	  Npkcontext.report_ignore_warning "Parser.attribute_name" 
	    "packed attribute" Npkcontext.Pack
      | "__transparent_union__" -> 
	  Npkcontext.report_accept_warning "Parser.attribute_name" 
	    "transparent union" Npkcontext.TransparentUnion
      | "weak" | "__weak__" ->
	  Npkcontext.report_warning "Parser.attribute" 
	    "ignoring attribute weak"
      | _ -> raise Parsing.Parse_error
    end;
    [] 
  }
| IDENTIFIER LPAREN string_list RPAREN               {
    if ($1 = "alias") then begin
      Npkcontext.report_warning "Parser.attribute" 
      ("ignoring attribute alias")
    end 
    else if ($1 <> "__warning__") && ($1 <> "__error__") && ($1 <> "__section__") && ($1 <> "section")
    then raise Parsing.Parse_error;
    []
  }
| IDENTIFIER LPAREN integer_list RPAREN    { 
    match ($1, $3) with
	(("__format_arg__" | "aligned" | "__regparm__" | "regparm"), _::[]) -> []
      | (("packed" | "__packed__"), _::[]) -> 
	  Npkcontext.report_ignore_warning "Parser.attribute_name" 
	    "packed attribute" Npkcontext.Pack;
	  []
      | (("__nonnull__" | "__nonnull"), _) -> []
      | _ -> raise Parsing.Parse_error
  }
| IDENTIFIER LPAREN LPAREN 
      IDENTIFIER LPAREN type_name RPAREN 
      RPAREN RPAREN                         { 
	if $1 <> "aligned" then raise Parsing.Parse_error;
	if $4 <> "__alignof__" then raise Parsing.Parse_error;
	[]
      }
| IDENTIFIER LPAREN 
    IDENTIFIER COMMA INTEGER COMMA INTEGER 
  RPAREN                                   {
(* TODO: instead of comparing all the possibilities __format__, format...
   maybe have a treatment that trims the __ first and then compares
   and do that in an uniform way??
*)
    if $1 <> "__format__" && $1 <> "format" then raise Parsing.Parse_error;
    begin match $3 with
	"__printf__" | "printf" | "__scanf__" | "scanf"
      | "__strftime__" | "strftime" | "__strfmon__" | "strfmon"-> ()
      | _ -> raise Parsing.Parse_error
    end;
    [] 
  }
| IDENTIFIER LPAREN IDENTIFIER RPAREN           { 
    if $1 <> "__mode__" then raise Parsing.Parse_error;
    let imode =
      match $3 with
	  "__QI__" -> Config.size_of_byte
	| "__HI__" -> Config.size_of_byte*2
	| "__SI__" | "__word__" -> Config.size_of_byte*4
	| "__DI__" -> Config.size_of_byte*8
	| _ -> raise Parsing.Parse_error
    in
      imode::[]
  }
| CONST                                    { [] }
;;

string_list:
  STRING                                   { () }
| STRING string_list                       { () }
;;

integer_list:
  INTEGER                                  { $1::[] }
| INTEGER COMMA integer_list               { $1::$3 }
;;

// Newspeak assertion language
assertion:
  SYMBOL assertion                         { (SymbolToken $1)::$2 }
| IDENTIFIER assertion                     { (IdentToken $1)::$2 }
| constant assertion                       { (CstToken $1)::$2 }
| EOF                                      { [] }
;;
