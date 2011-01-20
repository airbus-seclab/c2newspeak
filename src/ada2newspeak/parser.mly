/* (*
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

  Etienne Millon
  email: etienne.millon AT gmail . com

  Jasmine Duchon
  email : jasmine . duchon AT free . fr

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org
*) */

%{
open AdaSyntax
open Ada_utils

let check_ident i1 i2 =
  if (String.compare i1 i2) <> 0
  then
    Npkcontext.report_error "Parser.check_ident"
      ("syntax error : \"end "^i1^";\" expected")

let check_name l1 = function
  | None    -> ()
  | Some l2 -> List.iter2 check_ident l1 l2

let check_end decl end_name =
  let begin_name = match decl with
    | Subprogram(name,_,_) -> name
  in
    match end_name with
    | None    -> ()
    | Some en -> check_ident begin_name en

let check_loop_name a b =
  match (a, b) with
  | Some a, None   -> Npkcontext.report_error "Parser.loop_label"
                      ("Loop name \""^a^"\" expected at end of loop")
  | None  , Some b -> Npkcontext.report_error "Parser.loop_label"
                      ("Closing loop \""^b^"\", that has no beginning")
  | Some a, Some b
       when a <> b -> Npkcontext.report_error "Parser.loop_label"
                      "Loop statement closed with wrong identifier"
  | _ -> ()

(**
  * Prepare a list of exp*block for the Case constructor.
  * It takes a list of expression list * block and flattens it into
  * a list of expression*block.
  *)
let rec build_case_ch (choices:(expression list*block)list)
    :(expression*block) list =
    match choices with
      | []                     -> []
      | (exp_list,block)::tail -> (List.map (function exp -> exp,block)
                                            exp_list)
                                 @ (build_case_ch tail)

(**
 * Build a parameter specification list from a "factored" one.
 * That is to say, expand "X, Y : Integer" to "X : Integer ; Y : Integer".
 *
 * Rationale : RM95, 3.3.1.(7)
 *   "    Any declaration that includes a defining_identifier_list with more
 *    than one defining_identifier is equivalent to a series of declarations
 *    each containing one defining_identifier from the list, with the rest
 *    of the text of the declaration copied for each declaration in the
 *    series, in the same order as the list."
 *)
let make_param_spec (idents:string list)
                    (common_mode:param_mode)
                    (common_type:subtyp)
                    (common_default_value:expression option)
    :param list =
    List.map (function x -> { formal_name=x;
                                     mode=common_mode;
                               param_type=common_type;
                            default_value=common_default_value;
                            })
             idents

let make_enum list_val =
  let rec make_id list_val next_id =
    match list_val with
      | v::r -> (v,Newspeak.Nat.of_int next_id)::(make_id r (next_id +1))
      | [] -> []
  in
  let list_assoc = make_id list_val 0 in
  Enum list_assoc

let make_range exp_b_inf exp_b_sup =
  IntegerRange(exp_b_inf, exp_b_sup)

let report_perror s =
  Npkcontext.report_error "parser" s

let extract_name = function
  | (Some x,_,_) -> x
  | _ -> report_perror "No name"
%}
%token EOF

%token <Newspeak.location*Newspeak.Nat.t> CONST_INT
%token <Newspeak.location*int>            CONST_CHAR
%token <Newspeak.location*string>         CONST_FLOAT
%token <Newspeak.location*string>         CONST_STRING
%token <Newspeak.location*string>         IDENT

%token <Newspeak.location> ABS       ACCESS    ALL     AND        ARRAY
%token <Newspeak.location> ARROW     ASSIGN    AT      BEGIN      BODY
%token <Newspeak.location> CASE      COLON     COMMA   CONSTANT   DECLARE
%token <Newspeak.location> DIGITS    DIV       DOT     DOUBLE_DOT ELSE
%token <Newspeak.location> ELSIF     END       EQ      EXIT       FOR
%token <Newspeak.location> FUNCTION  GE        GT      IF         IN
%token <Newspeak.location> IS        LE        LOOP    LPAR       LT
%token <Newspeak.location> MINUS     MOD       MULT    NE         NEW
%token <Newspeak.location> NOT       NULL      OF      OR         OTHERS
%token <Newspeak.location> OUT       PACKAGE   PLUS    POW        PRAGMA
%token <Newspeak.location> PROCEDURE QUOTE     RANGE   RECORD     REM
%token <Newspeak.location> RENAMES   RETURN    REVERSE RPAR       SEMICOLON
%token <Newspeak.location> SUBTYPE   THEN      TYPE    USE        VBAR
%token <Newspeak.location> WHEN      WHILE     WITH    XOR

%left       AND OR XOR          /*            logical operators */
%left       EQ NE LT LE GT GE   /*         relational operators */
%left       PLUS MINUS          /*      binary adding operators */
%nonassoc   UPLUS UMINUS        /*       unary adding operators */
%left       MULT DIV MOD REM    /*        multiplying operators */
%right      POW                 /* highest precedence operators */
%nonassoc   ABS NOT

%start s
%type <AdaSyntax.compilation_unit> s

%type <string list*Newspeak.location> name
%%

s :
| context library_item EOF                 { $1, fst $2, snd $2 }
;;

context :
|                                          { [] }
| context_item context                     { $1@$2 }
;;

context_item :
| WITH ident_list SEMICOLON                { List.map (fun n ->
                                               With(n, None)) $2 }
| USE  ident_list SEMICOLON                { List.map (fun n ->
                                               UseContext n)  $2 }
;;

library_item :
| decl                                     { Spec(fst $1), snd $1 }
| body                                     { Body(fst $1), snd $1 }
;;

body :
| subprogram_spec IS declarative_part
  BEGIN instr_list END ident_opt SEMICOLON { let (spec, loc) = $1 in
                                             check_end spec $7;
                                             SubprogramBody(spec,$3,$5), loc }
| PACKAGE BODY ident IS declarative_part
  END name_opt SEMICOLON                   { check_name [$3] $7;
                                             PackageBody($3, None, $5), $1 }
;;

decl :
| subprogram_spec SEMICOLON                { (SubprogramSpec(fst $1)), snd $1 }
| PACKAGE ident IS basic_declarative_part
  END name_opt SEMICOLON                   { check_name [$2] $6;
                                             PackageSpec($2, $4), $1 }
| PACKAGE ident IS NEW name
  LPAR actual_parameter_part
  RPAR SEMICOLON                           { Npkcontext.set_loc $1;
                                             Npkcontext.report_warning "parser"
                                             ( "Ignoring instanciation "
                                             ^ "of generic package '"
                                             ^ name_to_string (fst $5)
                                             ^ "'");
                                             PackageSpec($2, []), $1
                                           }
;;

subprogram_spec :
| PROCEDURE ident full_formal_part         { Subprogram ($2,$3,None)    , $1 }
| FUNCTION  ident full_formal_part
            RETURN subtyp                  { Subprogram ($2,$3,Some $5) , $1 }
;;

full_formal_part :
|                                          { [] }
| LPAR formal_part RPAR                    { $2 }
;;

formal_part :
| parameter_specification                  { $1 }
| parameter_specification
    SEMICOLON formal_part                  { $1@$3 }
;;

mode :
|                                          { In    }
| IN                                       { In    }
|    OUT                                   {   Out }
| IN OUT                                   { InOut }
;;

parameter_specification :
| ident_list COLON mode subtyp
    assign_option                          { make_param_spec $1 $3 $4 $5 }
;;

assign_option :
|                                          { None    }
| ASSIGN expr                              { Some $2 }
;;

declarative_part :
|                                          { [] }
| declarative_item declarative_part        { $1::$2 }
| factored_decl    declarative_part        { (List.map (fun (x,y) ->
                                                (BasicDecl x),y) $1)
                                             @$2 }
| pragma           declarative_part        { Npkcontext.report_warning "parser"
                                             ("pragma '" ^ $1 ^ "' is ignored");
                                             $2 }
;;

factored_decl :
| use_decl                                 { $1 }
| number_decl                              { $1 }
;;

basic_declarative_part :
|                                          { [] }
| basic_declaration basic_declarative_part { $1::$2 }
| factored_decl     basic_declarative_part { $1@$2 }
;;

pragma :
| PRAGMA ident SEMICOLON                   { $2 }
| PRAGMA ident LPAR
  pragma_argument_association_list
  RPAR SEMICOLON                           { $2 }
;;

pragma_argument_association_list :
| pragma_argument_association              { }
| pragma_argument_association COMMA
    pragma_argument_association_list       { }
;;

pragma_argument_association :
| expr                                     { }
| ident ARROW expr                         { }
;;

declarative_item :
| basic_declaration                        { BasicDecl(fst $1), snd $1 }
| body                                     { BodyDecl (fst $1), snd $1 }
;;

use_decl :
| USE ident_list SEMICOLON                 { List.map (fun n ->
                                               (UseDecl n),$1) $2 }
;;

number_decl :
| ident_list COLON CONSTANT
  ASSIGN expr SEMICOLON                    { (* As the expression must be
                                              * static, we can safely copy it :
                                              * multiple evaluations will yield
                                              * the same result.
                                              *)
                                             List.map (fun x ->
                                               NumberDecl (x, $5), $2) $1
                                           }
;;

basic_declaration :
| ident_list COLON subtyp_indication
  assign_option SEMICOLON                  { ObjectDecl ($1, $3,
                                                $4, Variable), $2 }
| ident_list COLON CONSTANT
  subtyp_indication ASSIGN
  expr SEMICOLON                           { ObjectDecl ($1, $4,
                                                Some $6, Constant), $2 }
| TYPE ident IS type_declaration SEMICOLON { TypeDecl ($2, $4), $1 }
| SUBTYPE ident IS
  subtyp_indication SEMICOLON              { SubtypDecl($2,$4), $1 }
| decl                                     { SpecDecl(fst $1), snd $1 }
| representation_clause SEMICOLON          { RepresentClause(fst (fst $1),
                                             snd (fst $1)), snd $1 }
| subprogram_spec RENAMES name SEMICOLON   { 
                                      let nm = fst $3 in
                                        match (fst $1) with
                                          | Subprogram (n, ag, rt) ->
                                            RenamingDecl(n,Some ag, rt, nm),$2
  }
| ident_list COLON subtyp_indication
  RENAMES name SEMICOLON                   { match $1 with
                                             | [x] -> RenamingDecl(x, None, None, fst $5),$2
                                             | _ -> Npkcontext.report_error
                                                 "Parser" ("Only one identifier"
                                                 ^" is allowed before "
                                                 ^"\"renames\"");
                                           }
| FUNCTION ident IS NEW name
  LPAR actual_parameter_part
  RPAR SEMICOLON                           { GenericInstanciation ($2, fst $5,
                                                                   $7), $1 }
;;

type_declaration :
| ARRAY LPAR subtyp_indication_list RPAR
  OF subtyp_indication                     { Array ($3,$6) }
| LPAR ident_list RPAR                     { make_enum $2 }
| NEW subtyp_indication                    { DerivedType $2 }
| DIGITS CONST_INT                         { Digits (snd $2) }
| RECORD record_component_list END RECORD  { Record $2 }
| RANGE expr DOUBLE_DOT expr               { IntegerRange ($2, $4) }
| ACCESS name                              { Access (fst $2) }
;;

subtyp_indication_list :
| subtyp_indication                        { $1::[] }
| subtyp_indication COMMA
  subtyp_indication_list                   { $1::$3 }
;;

record_component_list :
| record_component record_component_list   { $1::$2 }
| record_component                         { $1::[] }
;;

record_component :
  IDENT COLON subtyp SEMICOLON             { snd $1,$3 }
;;

contrainte :
| expr DOUBLE_DOT expr                     { $1, $3 }
;;

array_component_association :
| ident ARROW expr                         { $1, $3 }
;;

named_array_aggregate :
| array_component_association              { $1::[] }
| array_component_association COMMA
  named_array_aggregate                    { $1::$3 }
;;

array_aggregate :
| LPAR named_array_aggregate RPAR          { $2 }
;;

representation_clause :
| FOR ident USE array_aggregate            { ($2,EnumRepClause $4) ,$1 }
| FOR ident QUOTE ident USE expr           { ($2,SizeRepClause $6), $1 }
| FOR ident USE RECORD
  record_clause_list END RECORD            { ($2,RecordRepClause $5), $1 }
;;

record_clause_list :
| record_clause                            { $1::[] }
| record_clause record_clause_list         { $1::$2 }
;;

record_clause :
| ident AT expr RANGE expr
  DOUBLE_DOT expr SEMICOLON                { $1, $3, $5, $7 }
;;

instr_list :
| instr SEMICOLON                          { $1::[] }
| instr SEMICOLON instr_list               { $1::$3 }
;;

instr :
| NULL                                     { NullInstr, $1 }
| RETURN expr                              { Return $2, $1 }
| RETURN                                   { ReturnSimple, $1 }
| lvalue                                   { LvalInstr(fst $1),snd $1 }
| lvalue ASSIGN expr                       { Assign (fst $1, $3), $2 }
| EXIT                                     { Exit, $1 }
| EXIT WHEN expr                           { If($3,[Exit,$1],[]), $1 }
| IF expr THEN instr_list
  instruction_else END IF                  { (If($2, $4, $5), $1) }
| loop_label iteration_scheme LOOP
  instr_list END LOOP ident_opt            { check_loop_name $1 $7;
                                             Loop (fst $2, $4),
                                             (if snd $2 = Newspeak.unknown_loc
                                              then $3
                                              else snd $2) }
| CASE expr IS
  case_stmt_alternative_list END CASE      { Case($2, build_case_ch (fst $4),
                                                  (snd $4)), $1 }
| DECLARE declarative_part
  BEGIN instr_list END                     { Block ($2,$4), $1 }
;;

loop_label :
|                                          { None    }
| ident COLON                              { Some $1 }
;;

name_opt :
|                                          { None          }
| name                                     { Some (fst $1) }
;;

ident_opt :
|                                          { None    }
| ident                                    { Some $1 }
;;

case_stmt_alternative_list :
| when_others                              { []           , Some $1 }
| case_stmt_alternative                    { $1::[]       , None   }
| case_stmt_alternative
  case_stmt_alternative_list               { $1::(fst $2) , snd $2 }
;;

when_others :
| WHEN OTHERS ARROW instr_list             { $4 }
;;

case_stmt_alternative :
| WHEN discrete_choice_list
  ARROW instr_list                         { $2, $4 }
;;

discrete_choice_list :
| expr                                     { $1::[] }
| expr            VBAR
  discrete_choice_list                     { $1::$3 }
;;

lvalue :
| IDENT                                    { Var   (snd $1)     , fst $1 }
| lvalue DOT ident                         { SName (fst $1, $3) , $2    }
| lvalue DOT ALL                           { PtrDeref (fst $1)  , $2    }
| lvalue LPAR
  actual_parameter_part RPAR               { ParExp (fst $1, $3), $2    }
;;

iteration_scheme :
|                                          { NoScheme, Newspeak.unknown_loc }
| WHILE expr                               { While($2), $1 }
| FOR ident IN for_loop_reverse
  for_loop_range                           { For($2, $5, $4), $1 }
;;

for_loop_reverse :
|                                          { false }
| REVERSE                                  { true  }
;;

for_loop_range :
| expr DOUBLE_DOT expr                     { DirectRange  ($1, $3) }
| lvalue QUOTE RANGE                       { ArrayRange (fst $1) }
| lvalue                                   { SubtypeRange (fst $1) }
;;

instruction_else :
|                                          { [] }
| ELSIF expr THEN instr_list
  instruction_else                         { [(If($2, $4, $5), $1)] }
| ELSE instr_list                          { $2 }
;;

expr :
| expr AND THEN expr %prec AND             { Binary(AndThen, $1, $4) }
| expr OR ELSE  expr %prec OR              { Binary(OrElse , $1, $4) }
| expr AND      expr                       { Binary(And    , $1, $3) }
| expr OR       expr                       { Binary(Or     , $1, $3) }
| expr XOR      expr                       { Binary(Xor    , $1, $3) }
| expr MULT     expr                       { Binary(Mult   , $1, $3) }
| expr DIV      expr                       { Binary(Div    , $1, $3) }
| expr MOD      expr                       { Binary(Mod    , $1, $3) }
| expr REM      expr                       { Binary(Rem    , $1, $3) }
| expr PLUS     expr                       { Binary(Plus   , $1, $3) }
| expr MINUS    expr                       { Binary(Minus  , $1, $3) }
| expr EQ       expr                       { Binary(Eq     , $1, $3) }
| expr NE       expr                       { Binary(Neq    , $1, $3) }
| expr LE       expr                       { Binary(Le     , $1, $3) }
| expr GE       expr                       { Binary(Ge     , $1, $3) }
| expr LT       expr                       { Binary(Lt     , $1, $3) }
| expr GT       expr                       { Binary(Gt     , $1, $3) }
| expr POW      expr                       { Binary(Power  , $1, $3) }
| PLUS  expr %prec UPLUS                   { Unary (UPlus  , $2    ) }
| MINUS expr %prec UMINUS                  { Unary (UMinus , $2    ) }
| NOT   expr                               { Unary (Not    , $2    ) }
| ABS   expr                               { Unary (Abs    , $2    ) }
| CONST_INT                                { CInt   (snd $1) }
| CONST_CHAR                               { CChar  (snd $1) }
| CONST_FLOAT                              { CFloat (float_of_string (snd $1)) }
| LPAR expr RPAR                           { $2 }
| lvalue QUOTE LPAR expr RPAR              { Qualified (fst $1 ,$4) }
| lvalue QUOTE DIGITS                      { Attribute (fst $1
                                                       , "digits"
                                                       ,None
                                                       ) }
| lvalue QUOTE ident                       { Attribute (fst $1
                                                       ,String.lowercase $3
                                                       ,None
                                                       ) }
| lvalue QUOTE ident LPAR expr RPAR        { Attribute (fst $1
                                                       ,String.lowercase $3
                                                       ,Some $5
                                                       ) }
| LPAR aggregate_association_list RPAR     { Aggregate (NamedAggr      $2) }
| LPAR expr_list RPAR                      { Aggregate (PositionalAggr $2) }
| lvalue                                   { Lval (fst $1) }
;;

expr_list :
| expr COMMA expr                          { $1::$3::[] }
| expr COMMA expr_list                     { $1::$3     }
;;

aggregate_association_list :
| aggr_lpart ARROW expr                    { ($1, $3)::[] }
| aggr_lpart ARROW expr COMMA
  aggregate_association_list               { ($1, $3)::$5 }
;;

aggr_lpart :
| expr                                     { match $1 with
                                             | Lval(Var x) -> AggrField x
                                             | _           -> AggrExp   $1 }
| expr DOUBLE_DOT expr                     { AggrRange ($1, $3) }
| OTHERS                                   { AggrOthers }
;;

subtyp_indication :
| subtyp RANGE contrainte                  { $1, Some $3 }
| subtyp                                   { $1, None    }
;;

subtyp :
| name                                     { fst $1 }
;;

actual_parameter_part :
| parameter_association                    { $1::[] }
| parameter_association COMMA
  actual_parameter_part                    { $1::$3 }
;;

parameter_association :
| expr                                     { None   , $1 }
| ident ARROW expr                         { Some $1, $3 }
;;

ident :
| IDENT                                    { snd $1 }
| CONST_STRING                             { operator_of_string (snd $1) }
;;

ident_list :
| ident                                    { $1::[] }
| ident COMMA ident_list                   { $1::$3 }
;;

name :
| ident_or_opname                          { (snd $1 ::[])     , fst $1 }
| ident_or_opname DOT name                 { ((snd $1)::fst $3), fst $1 }
;;

ident_or_opname :
| IDENT                                    { $1 }
| CONST_STRING                             { fst $1,
                                             (operator_of_string (snd $1)) }

%%

