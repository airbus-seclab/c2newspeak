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
(* TODO: should return the bare tree without doing any normalization, simplifications error reporting other than parsing errors *)
(* TODO: change this T to BareSyntax rather than Synthack *)
module T = Synthack

(* TODO: remove this function, should not be necessary *)
let gen_tmp_id =
  let tmp_cnt = ref 0 in
  let gen_tmp_id () = 
    incr tmp_cnt;
    Temps.to_string !tmp_cnt (Temps.Misc "parser")
  in
    gen_tmp_id

let gen_struct_id = 
  let struct_cnt = ref 0 in
  let gen_struct_id () =
    incr struct_cnt;
    "anon_struct"^(string_of_int !struct_cnt)
  in
    gen_struct_id
  
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
(* TODO: rename in declare type? *)
let build_typedef (b, m) =
  let b = Synthack.normalize_base_typ b in
  let build_vdecl ((v, attrs), _) =
    let b = apply_attrs attrs b in
    let x = Synthack.normalize_var_modifier b v in
      match x with
	| None -> ()
	| Some x -> Synthack.define_type x
  in
    List.iter build_vdecl m
     
(*
(* TODO: remove code?? *)
let normalize_fun_prologue b m =
  let (t, x, loc) = Synthack.normalize_decl (b, m) in
  let x =
    match x with
      | Some x -> x
      | None -> 
	  (* TODO: code cleanup remove these things !!! *)
	  Npkcontext.report_error "Firstpass.translate_global" 
	    "unknown function name"
  in
    (t, x, loc)
*)

(*
(* TODO: remove build_glbdecl and process_decl => now in bare2C *)
let build_type_decl d =
  let (t, _, _) = Synthack.normalize_decl d in
    t
    *)
(*
let build_type_blk _ d =
  let (t, _, _) = Synthack.normalize_decl d in
    t
    *)
let flatten_field_decl (b, x) = List.map (fun (v, i) -> (b, v, i)) x

(* TODO: simplify and put in synthack so as to optimize?? *)
let build_funparams params types =
  let has_name x d =
    match Synthack.normalize_decl d with
	Some y when x = y -> true
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

%type <BareSyntax.t> parse
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
  NPK translation_unit                      { (BareSyntax.GlbUserSpec $1, get_loc ())::$2 }
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
	($1, BareSyntax.Function ($2, build_funparams $4 $6))
  }
| direct_declarator 
      LPAREN identifier_list RPAREN
      old_parameter_declaration_list       { 
    Npkcontext.report_accept_warning "Parser.declarator"
      "deprecated style of function definition" Npkcontext.DirtySyntax;
    (0, BareSyntax.Function ($1, build_funparams $3 $5))
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
type_qualifier_list ident_or_tname          { ($4, [( ($5, BareSyntax.Variable ($8, get_loc ())), []) , None])}
;;
init_declarator_list:
                                            { (((0, BareSyntax.Abstract), []), None)::[] }
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
  ident_or_tname                           { (0, BareSyntax.Variable ($1, get_loc ())) }
| LPAREN declarator RPAREN                 { $2 }
| direct_declarator LBRACKET 
      expression_sequence RBRACKET         { (0, BareSyntax.Array ($1, Some $3)) }
| direct_declarator LBRACKET 
      type_qualifier_list RBRACKET         { (0, BareSyntax.Array ($1, None)) }
| direct_declarator 
  LPAREN parameter_list RPAREN             { (0, BareSyntax.Function ($1, $3)) }
| direct_declarator LPAREN RPAREN          { (0, BareSyntax.Function ($1, [])) }
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
    ((0, BareSyntax.Abstract), Some $2) 
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
| declaration_specifiers                   { ($1, (0, BareSyntax.Abstract)) }
;;

type_name:
  declaration_specifiers                   { ($1, (0, BareSyntax.Abstract)) }
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

// TODO: factor get_loc ()
statement:
  IDENTIFIER COLON statement               { (BareSyntax.Label $1, get_loc ())::$3 }
| IF LPAREN expression_sequence RPAREN statement
  else_branch_option                       { 
    [BareSyntax.If ((*normalize_bexp *)$3, $5, $6), get_loc ()] 
  }
| switch_stmt                              { [BareSyntax.CSwitch $1, get_loc ()] }
| iteration_statement                      { [$1, get_loc ()] }
| NPK                                      { (BareSyntax.UserSpec $1, get_loc ())::[] }
| compound_statement                       { [BareSyntax.Block $1, get_loc ()] }
| simple_statement SEMICOLON               { $1 }
;;

else_branch_option:
  ELSE statement                           { $2 }
|                         %prec below_ELSE { [] }
;;
  

simple_statement:
  declaration_modifier declaration         { [BareSyntax.LocalDecl ($1, $2), get_loc ()] (*build_stmtdecl $1 $2*) }
| TYPEDEF declaration                      { 
    (* TODO: cleanup/simplify this function *)
    let _ = build_typedef $2 in
      [BareSyntax.Typedef $2, get_loc ()]
  }
| RETURN expression_sequence               { 
    let loc = get_loc () in
      (BareSyntax.Exp (BareSyntax.Set (BareSyntax.RetVar, None, $2)), loc)::(BareSyntax.Return, loc)::[]
  }
| RETURN                                   { [BareSyntax.Return, get_loc ()] }
| expression_sequence                      { [BareSyntax.Exp $1, get_loc ()] (* [Exp $1, get_loc ()] *) }
| BREAK                                    { [BareSyntax.Break, get_loc ()] }
| CONTINUE                                 { [BareSyntax.Continue, get_loc ()] }
| GOTO IDENTIFIER                          { 
    Npkcontext.report_accept_warning "Parser.statement" "goto statement"
      Npkcontext.ForwardGoto;
    [BareSyntax.Goto $2, get_loc ()] 
  }
| asm                                      { [] }
|                                          { [] }
;;

declaration_modifier:
                                           { (false, false) }
| STATIC                                   { (true, false) }
| EXTERN                                   { (false, true) }
;;

asm:
  ASM volatile_option 
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
// TODO: factor cases
iteration_statement:
  FOR LPAREN assignment_expression_list SEMICOLON 
      expression_statement
      assignment_expression_list RPAREN
      statement                            { 
	BareSyntax.For ($3, (*normalize_bexp*) $5, $8, $6) 
      }
| FOR LPAREN SEMICOLON 
      expression_statement
      assignment_expression_list RPAREN
      statement                            { 
	Npkcontext.report_warning "Parser.iteration_statement" 
	  "init statement expected";
	BareSyntax.For ([], (*normalize_bexp*) $4, $7, $5) 
      }
| FOR LPAREN assignment_expression_list SEMICOLON 
      expression_statement RPAREN
      statement                            { 
	Npkcontext.report_warning "Parser.iteration_statement" 
	  "increment statement expected";
	BareSyntax.For ($3, (*normalize_bexp*) $5, $7, []) 
      }
| FOR LPAREN SEMICOLON expression_statement RPAREN
    statement                            { 
      Npkcontext.report_warning "Parser.iteration_statement" 
	"init statement expected";
      BareSyntax.For ([], (*normalize_bexp *)$4, $6, []) 
    }
| WHILE LPAREN expression_sequence RPAREN 
  statement                                { 
    BareSyntax.For ([], (*normalize_bexp*) $3, $5, [])
  }
| DO statement
  WHILE LPAREN expression_sequence 
  RPAREN SEMICOLON                         { 
    BareSyntax.DoWhile ($2, $5)
    (* TODO: BareSyntax.DoWhile ($2, normalize_bexp $5) *)
  }
;;

expression_statement:
  SEMICOLON                                { 
    Npkcontext.report_warning "Parser.expression_statement" 
      "halting condition should be explicit";
    BareSyntax.exp_of_int 1
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
  assignment_expression_list               { (BareSyntax.Exp $1, get_loc ())::$3 }
| expression                               { (BareSyntax.Exp $1, get_loc ())::[] }
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
  IDENTIFIER                               { BareSyntax.Var $1 }
| constant                                 { BareSyntax.Cst $1 }
| string_literal                           { BareSyntax.Str $1 }
| FUNNAME                                  { BareSyntax.FunName }
| LPAREN expression_sequence RPAREN        { $2 }
| LPAREN compound_statement RPAREN         { 
    Npkcontext.report_accept_warning "Parser.relational_expression"
      "block within expression" Npkcontext.DirtySyntax;
    BareSyntax.BlkExp $2
  }
// TODO: cleanup: remove all BareSyntax.
| expression 
  LBRACKET expression_sequence RBRACKET    { BareSyntax.Index ($1, $3) }
| expression 
  LPAREN argument_expression_list RPAREN   { BareSyntax.Call ($1, $3) }
| expression DOT ident_or_tname            { BareSyntax.Field ($1, $3) }
| expression ARROW ident_or_tname          { 
    BareSyntax.Field (BareSyntax.Index ($1, BareSyntax.exp_of_int 0), $3) 
  }
| expression PLUSPLUS                      { BareSyntax.OpExp (Plus, $1, true) }
| expression MINUSMINUS                    { BareSyntax.OpExp (Minus, $1, true) }
// GNU C
| BUILTIN_CONSTANT_P 
  LPAREN expression_sequence RPAREN        { 
     Npkcontext.report_warning "Parser.assignment_expression"
       "__builtin_constant_p ignored, assuming value 0";
    BareSyntax.exp_of_int 0
  }
| OFFSETOF 
  LPAREN type_name COMMA offsetof_member RPAREN { BareSyntax.Offsetof ($3, $5) }
// TODO: factor all these cases => prefix_operator
| PLUSPLUS   expression    %prec prefix_OP { BareSyntax.OpExp (Plus, $2, false) }
| MINUSMINUS expression    %prec prefix_OP { BareSyntax.OpExp (Minus, $2, false) }
| AMPERSAND  expression    %prec prefix_OP { BareSyntax.AddrOf $2 }
| STAR       expression    %prec prefix_OP { BareSyntax.Index ($2, BareSyntax.exp_of_int 0) }
// TODO: factor these with unop non-terminal
| BNOT       expression    %prec prefix_OP { BareSyntax.Unop (BNot, $2) }
| NOT        expression    %prec prefix_OP { BareSyntax.Unop (Not, $2) }
| MINUS      expression    %prec prefix_OP { BareSyntax.neg $2 }
| PLUS       expression                    { $2 }
| SIZEOF     expression    %prec prefix_OP { BareSyntax.SizeofE $2 }
| SIZEOF LPAREN type_name RPAREN 
                           %prec prefix_OP { BareSyntax.Sizeof $3 }
| EXTENSION expression     %prec prefix_OP { $2 }
| LPAREN type_name RPAREN expression
                           %prec prefix_OP { BareSyntax.Cast ($4, $2) }
| LPAREN type_name RPAREN composite        { 
(*
    let loc = get_loc () in
    let (blk, t) = build_type_blk loc $2 in
    let d = 
      { 
	t = t; is_static = false; is_extern = false; 
	initialization = Some (Sequence $4) 
      } 
    in
    let id = gen_tmp_id () in
    let decl = (BareSyntax.LocalDecl (id, BareSyntax.VDecl d), loc) in
    let e = (BareSyntax.Exp (BareSyntax.Var id), loc) in
      Npkcontext.report_accept_warning "Parser.cast_expression" 
	"local composite creation" Npkcontext.DirtySyntax;
      BareSyntax.BlkExp (blk@decl::e::[])
      *)
    BareSyntax.LocalComposite ($2, $4, get_loc ())
  }
// TODO: factor these as binop non-terminal??
| expression STAR      expression          { BareSyntax.Binop (Mult, $1, $3) }
| expression DIV       expression          { BareSyntax.Binop (Div, $1, $3) }
| expression MOD       expression          { BareSyntax.Binop (Mod, $1, $3) }
| expression PLUS      expression          { BareSyntax.Binop (Plus, $1, $3) }
| expression MINUS     expression          { BareSyntax.Binop (Minus, $1, $3) }
| expression SHIFTL    expression          { BareSyntax.Binop (Shiftl, $1, $3) }
| expression SHIFTR    expression          { BareSyntax.Binop (Shiftr, $1, $3) }
| expression GT        expression          { BareSyntax.Binop (Gt, $1, $3) }
| expression GTEQ      expression          { BareSyntax.Unop (Not, BareSyntax.Binop (Gt, $3, $1)) }
| expression LT        expression          { BareSyntax.Binop (Gt, $3, $1) }
| expression LTEQ      expression          { BareSyntax.Unop (Not, BareSyntax.Binop (Gt, $1, $3)) }
| expression EQEQ      expression          { BareSyntax.Binop (Eq, $1, $3) }
// TODO: remove all reference to BareSyntax. => use an open
| expression NOTEQ     expression          { 
    BareSyntax.Unop (Not, BareSyntax.Binop (Eq, $1, $3)) 
  }
| expression AMPERSAND expression          { BareSyntax.Binop (BAnd, $1, $3) }
| expression BXOR      expression          { BareSyntax.Binop (BXor, $1, $3) }
| expression BOR       expression          { BareSyntax.Binop (BOr, $1, $3) }
| expression AND       expression          { BareSyntax.And ($1, $3) }
| expression OR expression                 { 
    BareSyntax.Or ($1, $3) 
  }
| expression QMARK expression_sequence
    COLON expression           %prec QMARK {
	Npkcontext.report_strict_warning "Parser.expression"
	  "conditional expression";
	BareSyntax.IfExp ($1, $3, $5)
  }
/* TODO!
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
    let id = gen_tmp_id () in
    let decl = (LocalDecl (id, VDecl d), loc) in
    let e' = Var id in
      BlkExp( [ decl; ( Exp (IfExp(e', e', $4)), loc ) ] )
  }
*/
| expression assignment_operator
                   expression     %prec EQ { BareSyntax.Set ($1, $2, $3) }
;;

aux_offsetof_member:
  IDENTIFIER { BareSyntax.OffComp $1 }
| aux_offsetof_member DOT IDENTIFIER { BareSyntax.OffField ($1, $3) }

offsetof_member:
  IDENTIFIER { BareSyntax.OIdent $1 }
| aux_offsetof_member DOT IDENTIFIER { BareSyntax.OField ($1, $3) }
| aux_offsetof_member DOT IDENTIFIER LBRACKET expression RBRACKET { BareSyntax.OArray ($1, $3, $5) }
;;

expression_sequence:
  expression                               { $1 }
| expression_sequence COMMA expression     { 
    Npkcontext.report_accept_warning "Parser.expression"
      "comma in expression" Npkcontext.DirtySyntax;
    let loc = get_loc () in
      BareSyntax.BlkExp ((BareSyntax.Exp $1, loc)::(BareSyntax.Exp $3, loc)::[])
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
  expression                               { BareSyntax.Data $1 }
| composite                                { BareSyntax.Sequence $1 }
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
  DOT IDENTIFIER EQ expression             { (Some $2, BareSyntax.Data $4) }
| DOT IDENTIFIER EQ LBRACE init_list RBRACE { (Some $2, BareSyntax.Sequence $5) }
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
  pointer                                  { ($1, BareSyntax.Abstract) }
| direct_abstract_declarator               { $1 }
| pointer direct_abstract_declarator       { 
    let (ptr, decl) = $2 in
      ($1+ptr, decl) 
  }
;;

// TODO: try to factor cases more
direct_abstract_declarator:
  LPAREN abstract_declarator RPAREN        { $2 }
| LBRACKET type_qualifier_list RBRACKET    { 
    (0, BareSyntax.Array ((0, BareSyntax.Abstract), None)) 
  }
| LBRACKET expression_sequence RBRACKET    { 
    (0, BareSyntax.Array ((0, BareSyntax.Abstract), Some $2)) 
  }
| direct_abstract_declarator 
  LBRACKET expression_sequence RBRACKET    { (0, BareSyntax.Array ($1, Some $3)) }
| direct_abstract_declarator 
  LPAREN parameter_list RPAREN             { (0, BareSyntax.Function ($1, $3)) }
| direct_abstract_declarator LPAREN RPAREN { (0, BareSyntax.Function ($1, [])) }
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
      (BareSyntax.Va_arg, (0, BareSyntax.Variable ("__builtin_newspeak_va_arg", loc)))::[] 
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
  FLOAT                                   { Config.size_of_float }
| DOUBLE                                  { Config.size_of_double }
| LONG DOUBLE                             { Config.size_of_longdouble }
;;

type_specifier:
  VOID                                    { BareSyntax.Void }
| ityp                                    { BareSyntax.Integer (Newspeak.Signed, $1) }
| SIGNED ityp                             {
    Npkcontext.report_strict_warning "Parser.type_specifier" 
      "signed specifier not necessary";
    BareSyntax.Integer (Newspeak.Signed, $2)
  }
| LONG LONG UNSIGNED INT                  {
    Npkcontext.report_strict_warning "Parser.type_specifier"
      ("'long long unsigned int' is not normalized : "
      ^"use 'unsigned long long int' instead");
    BareSyntax.Integer (Newspeak.Unsigned, Config.size_of_longlong)
  }
| UNSIGNED ityp                           { BareSyntax.Integer (Newspeak.Unsigned, $2) }
| UNSIGNED                                { 
    Npkcontext.report_strict_warning "Parser.type_specifier"
      "unspecified integer kind";
    BareSyntax.Integer (Newspeak.Unsigned, Config.size_of_int) 
  }

| LONG SIGNED INT                         {
  Npkcontext.report_strict_warning "Parser.type_specifier" 
      ("'long signed int' is not normalized: "
       ^"use 'signed long int' instead");
    BareSyntax.Integer (Newspeak.Signed, Config.size_of_long)
  }

| LONG SIGNED                             {
  Npkcontext.report_strict_warning "Parser.type_specifier" 
      ("'long signed' is not normalized: "
       ^"use 'signed long int' instead");
    BareSyntax.Integer (Newspeak.Signed, Config.size_of_long)
  }

| LONG UNSIGNED INT                        {
  Npkcontext.report_strict_warning "Parser.type_specifier" 
      ("'long unsigned int' is not normalized: "
       ^"use 'unsigned long int' instead");
    BareSyntax.Integer (Newspeak.Unsigned, Config.size_of_long)
  }

| LONG UNSIGNED                            {
  Npkcontext.report_strict_warning "Parser.type_specifier" 
      ("'long unsigned' is not normalized: "
       ^"use 'unsigned long int' instead");
    BareSyntax.Integer (Newspeak.Unsigned, Config.size_of_long)
  }

| SHORT SIGNED INT                         {
  Npkcontext.report_strict_warning "Parser.type_specifier" 
      ("'short signed int' is not normalized: "
       ^"use 'signed short int' instead");
    BareSyntax.Integer (Newspeak.Signed, Config.size_of_short)
  }

| SHORT SIGNED                             {
  Npkcontext.report_strict_warning "Parser.type_specifier" 
      ("'short signed' is not normalized: "
       ^"use 'signed short int' instead");
    BareSyntax.Integer (Newspeak.Signed, Config.size_of_short)
  }

| SHORT UNSIGNED INT                       {
  Npkcontext.report_strict_warning "Parser.type_specifier" 
      ("'short unsigned int' is not normalized: "
       ^"use 'unsigned short int' instead");
    BareSyntax.Integer (Newspeak.Unsigned, Config.size_of_short)
  }

| SHORT UNSIGNED                           {
  Npkcontext.report_strict_warning "Parser.type_specifier" 
      ("'short unsigned' is not normalized: "
       ^"use 'unsigned short int' instead");
    BareSyntax.Integer (Newspeak.Unsigned, Config.size_of_short)
  }

| ftyp                                     { BareSyntax.Float $1 }
| struct_or_union composite_arguments      { BareSyntax.Composite ($1, $2) }
| TYPEDEF_NAME                             { BareSyntax.Name $1 }
| ENUM enum_arguments                      { BareSyntax.Enum $2 }
| VA_LIST                                  { BareSyntax.Va_arg }
| TYPEOF LPAREN type_specifier RPAREN      { $3 }
| TYPEOF LPAREN IDENTIFIER RPAREN          { BareSyntax.Typeof $3 }
;;

struct_or_union:
  STRUCT                                   { true }
| UNION                                    { false }
;;

composite_arguments:
  field_blk                                { (gen_struct_id (), Some $1) }
| ident_or_tname                           { ($1, None) }
| ident_or_tname field_blk                 { ($1, Some $2) }
;;

enum_arguments:
  enum_values                              { $1 }
| IDENTIFIER                               { None }
| IDENTIFIER enum_values                   { $2 }
;;

enum_values:
  LBRACE enum_list RBRACE                  { Some $2 }
;;

//Section that is dependent on version of the compiler (standard ANSI or GNU)
//TODO: find a way to factor some of these, possible!!!
external_declaration:
  function_definition                      { (BareSyntax.FunctionDef (false, $1), get_loc ())::[] }
| STATIC function_definition               { (BareSyntax.FunctionDef (true, $2), get_loc ())::[] }
| INLINE STATIC function_definition        { (BareSyntax.FunctionDef (true, $3), get_loc ())::[] }
| ATTRIBUTE LPAREN LPAREN attribute_name_list 
  RPAREN RPAREN STATIC function_definition { (BareSyntax.FunctionDef (true, $8), get_loc ())::[] }
| extension_option
  EXTERN function_definition               {
    Npkcontext.report_ignore_warning "Parser.external_declaration" 
      "extern function definition" Npkcontext.ExternFunDef;
    let ((b, m), _) = $3 in
      (BareSyntax.GlbDecl ((false, false), (b, ((m, []), None)::[])), get_loc ())::[]
  }
| global_declaration SEMICOLON             { $1 }
;;

global_declaration:
  STATIC declaration                       { (BareSyntax.GlbDecl ((true, false), $2), get_loc ())::[] }
| EXTENSION declaration                    { (BareSyntax.GlbDecl ((false, false), $2), get_loc ())::[] }
| declaration                              { (BareSyntax.GlbDecl ((false, false), $1), get_loc ())::[] }
| extension_option EXTERN declaration      { (BareSyntax.GlbDecl ((false, true), $3), get_loc ())::[] }
| extension_option TYPEDEF declaration     { 
(* TODO: cleanup/simplify *)
    let _ = build_typedef $3 in
      (BareSyntax.GlbTypedef $3, get_loc ())::[] }
| asm                                      { [] }
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
  extension_option field_declaration       { $2 }
;;

field_declaration:
  declaration_specifiers
  struct_declarator_list attribute_list    { flatten_field_decl ($1, $2) }
| declaration_specifiers                   { 
    Npkcontext.report_accept_warning "Parser.field_declaration"
      "anonymous field declaration in structure" Npkcontext.DirtySyntax;
    flatten_field_decl ($1, ((0, BareSyntax.Abstract), None)::[]) 
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
      | "no_instrument_function"
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
| IDENTIFIER LPAREN LPAREN INTEGER SHIFTL
  LPAREN INTEGER RPAREN RPAREN RPAREN    {
    match $1 with
	"__aligned__" -> []
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

extension_option:
  EXTENSION                                { }
|                                          { }
;;

volatile_option:
  VOLATILE                                 { }
|                                          { }
;;

