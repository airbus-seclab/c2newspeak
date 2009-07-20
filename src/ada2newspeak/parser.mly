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
open Syntax_ada

let check_ident i1 i2 =
  if (String.compare i1 i2) <> 0
  then
    Npkcontext.report_error "Parser.check_ident"
      ("syntax error : \"end "^i1^";\" expected")
  else ()

let check_name (p1,i1) (p2,i2) =
  check_ident i1 i2;
  match (p1, p2) with
    | Some p1', Some p2' -> check_ident p1' p2'
    | Some _  , None     -> invalid_arg "check_name"
    | None    , Some _   -> invalid_arg "check_name"
    | None    , None     -> ()

let check_end decl end_name =
  let begin_name = match decl with
    | Function(name,_,_) -> name
    | Procedure(name,_) -> name
  in
    check_ident begin_name end_name

let build_access name lst   =
  let rec build_aux lst  =
    match lst with [] -> (Lval name)
      | hd::tl ->
          let built = build_aux tl in
            ArrayAccess (built, hd)
  in
    match lst with
        [] -> Lval name
      | _  ->
          let rev_list = List.rev lst in
            build_aux rev_list

let build_matrix l typ_ind loc =
  (* crafted buids the subtype_indication *)
  let rec crafted list_ind typ_elt =
    match list_ind with
        [] -> typ_elt
      | hd::tl ->
          let recu =  crafted tl typ_elt in
          let new_ind = Unconstrained
                          (Declared("no_name"
                                   ,Array({array_index     = hd
                                          ;array_component = recu
                                          ;array_size      = None
                                          }
                                         )
                                   ,Ada_types.new_array
                                               Ada_types.unknown (* hd   *)
                                               Ada_types.unknown (* recu *)
                                   ,loc
                                   )
                          )
          in
            ( new_ind, None, None, Ada_types.unknown)
  in
    match l with
        [] -> Npkcontext.report_error "Parser.build_matrix"
          ("in build matrix, no subtyp given ")
      | hd::[] -> {array_index     = hd;
                   array_component = typ_ind;
                   array_size      = None;
                  }
      | hd::tl -> {array_index     = hd;
                   array_component = crafted tl typ_ind;
                   array_size      = None;
                  }

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
let make_parameter_specification (idents:string list)
                                 (common_mode:param_mode)
                                 (common_type:subtyp)
                                 (common_default_value:expression option)
    :param list =
    List.map (function x -> {formal_name=x;
                                    mode=common_mode;
                              param_type=common_type;
                           default_value=common_default_value;})
             idents

let make_enum list_val =
  let rec make_id list_val next_id =
    match list_val with
      | v::r -> (v,Newspeak.Nat.of_int next_id)::(make_id r (next_id +1))
      | [] -> []
  in
  let list_assoc = make_id list_val 0 in
  let max = Newspeak.Nat.of_int ((List.length list_assoc) - 1)
  in Enum(list_assoc, Ada_config.size_of_enum Newspeak.Nat.zero max)

let make_range exp_b_inf exp_b_sup =
  IntegerRange(RangeConstraint(exp_b_inf, exp_b_sup), None)


%}
%token EOF

%token <Newspeak.location*Syntax_ada.nat> CONST_INT
%token <Newspeak.location*int>            CONST_CHAR
%token <Newspeak.location*string>         CONST_FLOAT
%token <Newspeak.location*string>         CONST_STRING
%token <Newspeak.location*string>         IDENT

%token <Newspeak.location> ABS        AND     ARRAY     ARROW     ASSIGN
%token <Newspeak.location> BEGIN      BODY    CASE      COLON     COMMA
%token <Newspeak.location> CONSTANT   DECLARE DIGITS    DIV       DOT
%token <Newspeak.location> DOUBLE_DOT ELSE    ELSIF     END       EQ
%token <Newspeak.location> EXIT       FALSE   FOR       FUNCTION  GE
%token <Newspeak.location> GT         IF      IN        IS        LE
%token <Newspeak.location> LOOP       LPAR    LT        MINUS     MOD
%token <Newspeak.location> MULT       NE      NEW       NOT       NULL
%token <Newspeak.location> OF         OR      OTHERS    OUT       PACKAGE
%token <Newspeak.location> PLUS       POW     PRAGMA    PROCEDURE QUOTE
%token <Newspeak.location> RANGE      RECORD  REM       RENAMES   RETURN
%token <Newspeak.location> REVERSE    RPAR    SEMICOLON SUBTYPE   THEN
%token <Newspeak.location> TRUE       TYPE    USE       VBAR      WHEN
%token <Newspeak.location> WHILE      WITH    XOR

%left       AND OR XOR          /*            logical operators */
%left       EQ NE LT LE GT GE   /*         relational operators */
%left       PLUS MINUS          /*      binary adding operators */
%nonassoc   UPLUS UMINUS        /*       unary adding operators */
%left       MULT DIV MOD REM    /*        multiplying operators */
%right      POW                 /* highest precedence operators */
%nonassoc   ABS NOT

%start s
%type <Syntax_ada.compilation_unit> s

%type <string> pragma
%type <param_mode> mode
%type <name*location> name
%type <string> ident
%type <string list> ident_list
%type <argument> parameter_association
%type <argument list> actual_parameter_part args
%type <subtyp> subtyp
%type <subtyp_indication> subtyp_indication
%type <expression> expression discrete_choice
%type <instruction*location> instr
%type <block> instr_list instruction_else when_others
%type <name*argument list> lvalue
%type <iteration_scheme*location> iteration_scheme
%type <expression list> discrete_choice_list
%type <expression list*block> case_stmt_alternative
%type <(expression list*block) list* block option> case_stmt_alternative_list
%type <representation_clause*location> representation_clause
%type <array_aggregate> array_aggregate
%type <(string * expression) list> named_array_aggregate
%type <string * expression> array_component_association
%type <subtyp_indication list> matrix_indication
%type <array_type_definition> constrained_array_definition
%type <string*subtyp> record_component
%type <(string*subtyp) list> record_type_definition record_component_list
%type <contrainte> contrainte
%type <basic_declaration*location> basic_declaration
%type <(basic_declaration*location) list> basic_declarative_part
%type <declarative_item*location> declarative_item
%type <(declarative_item*location) list> declarative_part
%type <unit> pragma_argument_association pragma_argument_association_list
%type <param list> parameter_specification formal_part
%type <sub_program_spec*location> subprogram_spec
%type <spec*location> decl
%type <body*location> body
%type <library_item*location> library_item
%type <context_clause list> context_item context
%type <(basic_declaration*location) list> factored_decl use_decl number_decl

%%
/*grammaire*/
s: context library_item EOF {($1, fst $2, snd $2)}
;

context :
| {[]}
| context_item context {$1@$2}
;

context_item :
| WITH ident_list SEMICOLON { List.map (fun n -> With(n, $1, None)) $2}
| USE  ident_list SEMICOLON { List.map (fun n -> UseContext n)      $2}
;

library_item :
| decl {let (spec, loc) = $1 in (Spec(spec), loc)}
| body {let (body, loc) = $1 in (Body(body), loc)}
;

body :
| subprogram_spec IS declarative_part BEGIN instr_list END ident SEMICOLON
    {let (spec, loc) = $1
     in
       (check_end spec $7);
       (SubProgramBody(spec,$3,$5), loc)}

| subprogram_spec IS declarative_part BEGIN instr_list END SEMICOLON
    {let (spec, loc) = $1
     in (SubProgramBody(spec,$3,$5), loc)}
| PACKAGE BODY ident IS declarative_part END SEMICOLON
    {(PackageBody($3, None, $5), $1)}
| PACKAGE BODY ident IS declarative_part END name SEMICOLON
    { (check_name (None,$3) (fst $7));
      (PackageBody($3, None, $5), $1)
    }
;

decl :
| subprogram_spec SEMICOLON
    {let (spec, loc) = $1 in ((SubProgramSpec(spec)),loc)}
| PACKAGE ident IS basic_declarative_part END SEMICOLON
    {(PackageSpec($2, $4), $1)}
| PACKAGE ident IS basic_declarative_part END name SEMICOLON
        { (check_name (None,$2) (fst $6));
          (PackageSpec($2, $4), $1)}
;

/* on renvoie aussi la position de la spec */
subprogram_spec :
| PROCEDURE ident  LPAR formal_part RPAR           {Procedure($2,$4), $1}
| PROCEDURE ident                                  {Procedure($2,[]), $1}
| FUNCTION  ident  LPAR formal_part RPAR RETURN subtyp
                                                 {Function ($2,$4,$7),$1}
| FUNCTION  ident                        RETURN subtyp
                                                  {Function ($2,[],$4),$1}
;

formal_part :
| parameter_specification                       {$1}
| parameter_specification SEMICOLON formal_part {$1@$3}
;

mode :
|        {In}
| IN     {In}
|    OUT {Out}
| IN OUT {InOut}
;

parameter_specification :
| ident_list COLON mode subtyp                   { make_parameter_specification
                                                            $1 $3 $4 None}
| ident_list COLON mode subtyp ASSIGN expression { make_parameter_specification
                                                            $1 $3 $4 (Some $6)}
;

declarative_part :
|                                   {[]}
| declarative_item declarative_part {$1::$2}
| factored_decl    declarative_part {(List.map (fun (x,y) -> (BasicDecl x),y)
                                               $1)
                                     @$2
                                    }
| pragma           declarative_part {Npkcontext.report_warning "parser"
                                     ("pragma '" ^ $1 ^ "' is ignored");
                                     $2
                                    }
;

factored_decl :
| use_decl {$1}
| number_decl {$1}
;

basic_declarative_part :
| {[]}
| basic_declaration basic_declarative_part {$1::$2}
| factored_decl     basic_declarative_part {$1@$2}
;

pragma :
| PRAGMA ident SEMICOLON {$2}
| PRAGMA ident LPAR pragma_argument_association_list RPAR SEMICOLON {$2}
;

pragma_argument_association_list :
| pragma_argument_association {}
| pragma_argument_association COMMA pragma_argument_association_list {}
;

pragma_argument_association :
| expression {}
| ident ARROW expression {}
;

declarative_item :
| basic_declaration
    {let (basic, loc) = $1
     in (BasicDecl(basic), loc)}
| body {let (body, loc) = $1
        in (BodyDecl(body),loc)}
;

use_decl :
| USE ident_list SEMICOLON {List.map (fun n -> (UseDecl n),$1) $2}
;

number_decl :
| ident_list COLON CONSTANT ASSIGN expression SEMICOLON
        { (*
           * As the expression must be static, we can safely copy it :
           * multiple evaluations will yield the same result.
           *)
          List.map (fun x ->NumberDecl(x, $5),$2) $1
        }
;

basic_declaration :
| ident_list COLON subtyp_indication SEMICOLON
        {ObjectDecl($1,$3,None, Variable), $2}
| ident_list COLON subtyp_indication ASSIGN expression SEMICOLON
        {ObjectDecl($1,$3,Some($5), Variable), $2}
| ident_list COLON CONSTANT subtyp_indication ASSIGN expression SEMICOLON
        {ObjectDecl($1,$4,Some($6), Constant), $2}
| TYPE ident IS ARRAY constrained_array_definition SEMICOLON
    { TypeDecl($2,Array $5, Ada_types.new_array Ada_types.unknown
                                                Ada_types.unknown),$1}
| TYPE ident IS LPAR ident_list RPAR SEMICOLON
  { TypeDecl($2, make_enum $5,Ada_types.new_enumerated $5),$1}
| TYPE ident IS NEW subtyp_indication SEMICOLON
        { let (_,_,_,t) = $5 in
            TypeDecl($2,DerivedType $5,Ada_types.new_derived t),$1
        }
| TYPE ident IS RANGE expression DOUBLE_DOT expression SEMICOLON
            { TypeDecl($2, IntegerRange(RangeConstraint($5, $7), None)
                      ,Ada_types.unknown),$1}
| TYPE ident IS DIGITS CONST_INT SEMICOLON
            {
              (* digits X -> new float *)
              TypeDecl ($2,DerivedType (Unconstrained Float
                                       ,None
                                       ,None
                                       ,Ada_types.std_float
                                       )
                           ,Ada_types.new_float(
                                    Newspeak.Nat.to_int (snd $5))
                       )
              ,$1
            }
| TYPE ident IS record_type_definition SEMICOLON {TypeDecl($2
                                                          ,Record $4
                                                          ,Ada_types.unknown)
                                                 ,$1}
| SUBTYPE ident IS subtyp_indication SEMICOLON
        {SubtypDecl($2,$4), $1}
| decl  {let (spec, loc) = $1 in (SpecDecl(spec), loc)}
| representation_clause SEMICOLON {(RepresentClause(fst $1), snd $1)}
| subprogram_spec RENAMES name SEMICOLON
        { match (fst $1) with
        | Function  (n,_,_) -> RenamingDecl (n, fst $3),$2
        | Procedure (n,_)   -> RenamingDecl (n, fst $3),$2
        }
              | ident_list COLON subtyp_indication RENAMES name SEMICOLON {
                if (List.length $1 <> 1) then
                  Npkcontext.report_error "Parser"
                    "Only one identifier is allowed before \"renames\"";
                RenamingDecl((List.hd $1),fst $5),$2 }
;

record_type_definition:
  RECORD record_component_list END RECORD {$2}
;

record_component_list:
  | record_component record_component_list {$1::$2}
  | record_component                       {$1::[]}
;

record_component:
  IDENT COLON IDENT SEMICOLON {(snd $1),SubtypName (None,snd $3)}

contrainte :
| expression DOUBLE_DOT expression {RangeConstraint($1, $3)}
;

constrained_array_definition :
| LPAR matrix_indication RPAR OF subtyp_indication
                                            {build_matrix $2 $5 $1}
;

matrix_indication :
| subtyp_indication {[$1]}
| matrix_indication COMMA subtyp_indication {$1@[$3]}

array_component_association :
| ident ARROW expression {($1, $3)}
;

named_array_aggregate :
| array_component_association                             {$1::[]}
| array_component_association COMMA named_array_aggregate {$1::$3}
;

array_aggregate :
| LPAR named_array_aggregate RPAR {NamedArrayAggregate($2)}
;

representation_clause :
| FOR ident USE array_aggregate         {EnumerationRepresentation($2,$4)   ,$1}
;

instr_list :
| instr SEMICOLON            {$1::[]}
| instr SEMICOLON instr_list {$1::$3}
;

instr :
| NULL {(NullInstr, $1)}
| RETURN expression {(Return($2), $1)}
| RETURN {ReturnSimple, $1}
| name {ProcedureCall(fst $1, []),snd $1}
| name LPAR actual_parameter_part RPAR {ProcedureCall(fst $1, $3),snd $1}
| lvalue ASSIGN expression
    { let (nm, ind) = $1 in
        (Assign ( (build_access nm (List.map snd ind)) , $3), $2)
    }
| EXIT {Exit, $1}
| EXIT WHEN expression { If($3,[Exit,$1],[]), $1
  (* EXIT WHEN x --> IF x THEN EXIT END IF *) }
| IF expression THEN instr_list instruction_else END IF
    {(If($2, $4, $5), $1)}
| loop_label iteration_scheme LOOP instr_list END LOOP ident_option
      { begin match ($1, $7) with
          | Some a, None   -> Npkcontext.report_error "Parser.loop_label"
                      ("Loop name \""^a^"\" expected at end of loop")
          | None  , Some b -> Npkcontext.report_error "Parser.loop_label"
                      ("Closing loop \""^b^"\", that has no beginning")
          | Some a, Some b -> if (a <> b) then Npkcontext.report_error
               "Parser.loop_label" "Loop statement closed with wrong identifier"
          | None  , None -> ()
        end;
        let (scheme,loc) = $2 in
          (Loop(scheme, $4), (if loc=Newspeak.unknown_loc then $3 else loc))
      }
| CASE expression IS case_stmt_alternative_list END CASE {Case($2,
                                                              build_case_ch
                                                                  (fst $4),
                                                              (snd $4)),
                                                          $1}
| DECLARE declarative_part BEGIN instr_list END
                                {Block ($2,$4),$1}
;

loop_label:
|             {None   }
| ident COLON {Some $1}

ident_option:
|             {None}
| ident       {Some $1}

case_stmt_alternative_list:
| when_others                                       {[]           , Some $1}
| case_stmt_alternative                             {$1::[]       , None}
| case_stmt_alternative case_stmt_alternative_list  {$1::(fst $2) , snd $2}
;

when_others:
| WHEN OTHERS ARROW instr_list {$4}
;

case_stmt_alternative:
| WHEN discrete_choice_list ARROW instr_list {$2,$4}
;

discrete_choice_list:
| discrete_choice                           {$1::[]}
| discrete_choice VBAR discrete_choice_list {$1::$3}
;

discrete_choice:
| expression {$1}
;

lvalue :
| name      {((fst $1), [])}
| name args {((fst $1), $2)}

args:
|      LPAR actual_parameter_part RPAR {    $2 }
| args LPAR actual_parameter_part RPAR { $1@$3 }

iteration_scheme :
| {NoScheme, Newspeak.unknown_loc}
| WHILE expression {(While($2), $1)}
| FOR ident IN         expression DOUBLE_DOT expression
                                                {For($2,$4,$6,false), $1}
| FOR ident IN REVERSE expression DOUBLE_DOT expression
                                                {For($2,$5,$7, true), $1}

instruction_else :
| {[]}
| ELSIF expression THEN instr_list instruction_else
      {[(If($2, $4, $5), $1)]}
| ELSE instr_list {$2}
;

expression :
| expression AND THEN expression %prec AND {Binary(AndThen, $1, $4)}
| expression OR ELSE  expression %prec OR  {Binary(OrElse , $1, $4)}
| expression AND      expression           {Binary(And    , $1, $3)}
| expression OR       expression           {Binary(Or     , $1, $3)}
| expression XOR      expression           {Binary(Xor    , $1, $3)}
| expression MULT     expression           {Binary(Mult   , $1, $3)}
| expression DIV      expression           {Binary(Div    , $1, $3)}
| expression MOD      expression           {Binary(Mod    , $1, $3)}
| expression REM      expression           {Binary(Rem    , $1, $3)}
| expression PLUS     expression           {Binary(Plus   , $1, $3)}
| expression MINUS    expression           {Binary(Minus  , $1, $3)}
| expression EQ       expression           {Binary(Eq     , $1, $3)}
| expression NE       expression           {Binary(Neq    , $1, $3)}
| expression LE       expression           {Binary(Le     , $1, $3)}
| expression GE       expression           {Binary(Ge     , $1, $3)}
| expression LT       expression           {Binary(Lt     , $1, $3)}
| expression GT       expression           {Binary(Gt     , $1, $3)}
| expression POW      expression           {Binary(Power  , $1, $3)}
| PLUS  expression %prec UPLUS  {Unary (UPlus  , $2    )}
| MINUS expression %prec UMINUS {Unary (UMinus , $2    )}
| NOT   expression              {Unary (Not    , $2    )}
| ABS   expression              {Unary (Abs    , $2    )}
| CONST_INT   {CInt(snd $1)}
| CONST_FLOAT {CFloat(float_of_string (snd $1))}
| CONST_CHAR  {CChar(snd $1)}
| TRUE        {CBool(true)}
| FALSE       {CBool(false)}
| LPAR expression RPAR {$2}
| name QUOTE LPAR expression RPAR {Qualified(SubtypName (fst $1),$4)}
| name QUOTE ident  {Attribute ( fst $1
                                 ,String.lowercase $3
                                 ,None
                                 )
                      }
| name QUOTE ident LPAR expression RPAR {Attribute ( fst $1
                                                   ,String.lowercase $3
                                                   ,Some $5
                                                   )
                                        }
| name {Var(fst $1)}
| name LPAR actual_parameter_part RPAR {FunctionCall((fst $1), $3)}
;

subtyp_indication :
| subtyp RANGE contrainte {$1, Some($3), None, Ada_types.unknown}
| subtyp {$1, None, None, Ada_types.unknown}

subtyp :
| name {SubtypName(fst $1)}
;

actual_parameter_part :
| parameter_association                             {$1::[]}
| parameter_association COMMA actual_parameter_part {$1::$3}
;

parameter_association:
| expression             {None   , $1}
| ident ARROW expression {Some $1, $3}
;

ident :
| IDENT {snd $1}
| CONST_STRING { Ada_utils.make_operator_name (
                    Ada_utils.operator_of_string (snd $1)
                )}
;

ident_list :
| ident                  {$1::[]}
| ident COMMA ident_list {$1::$3}
;

name :
|           ident_or_opname { (None         , snd $1), fst $1 }
| IDENT DOT ident_or_opname { (Some (snd $1), snd $3), fst $1 }
;

ident_or_opname :
| IDENT        {$1}
| CONST_STRING { fst $1,(Ada_utils.make_operator_name (
                    Ada_utils.operator_of_string (snd $1)
                ))
    }


%%

