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

  Jasmine Duchon
  email : jasmine . duchon AT free . fr

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org
*) */

%{ open Syntax_ada

   let compare_ident i1 i2 =
     String.compare i1 i2

   let check_ident i1 i2 =
     if (compare_ident i1 i2) <> 0
     then
       Npkcontext.report_error "Parser.parse_error"
         ("syntax error : \"end "^i1^";\" expected")
     else ()

   let check_name (p1,i1) (p2,i2) =
     List.iter2 check_ident (p1@[i1]) (p2@[i2])


   let check_end decl end_name =
     let begin_name = match decl with
       | Function(name,_,_) -> name
       | Procedure(name,_) -> name
     in
       check_name begin_name end_name

   let loc = Npkcontext.get_loc

   let build_access name list   =
     let rec build_aux  list  =
       match list with [] -> (Lval name)
         | hd::tl ->
             let built = build_aux tl in
               ArrayAccess (built, hd)
     in
       match list with
           [] -> Lval name
         | _  ->
             let rev_list = List.rev list in
               build_aux rev_list

   let build_matrix list typ_ind =
     (*crafted buids the subtype_indication*)
     let rec crafted list_ind typ_elt =
       match list_ind with
           [] -> typ_elt
         | hd::tl ->
             let recu =  crafted tl typ_elt in
             let new_ind = Unconstrained (Declared (Array (
                "no_name", ConstrainedArray(hd, recu, None)), loc()))
             in
               ( new_ind, None, None (*Some (new_ind)*) )
     in
       match list with
           [] -> Npkcontext.report_error "Parser.parse_error"
             ("in build matrix, no subtyp given ")
         | hd::[] -> ConstrainedArray(hd, typ_ind, None)
         | hd::tl ->
             ConstrainedArray(hd,
                              (crafted tl typ_ind),
                              None)

    (** Create a function name for an overloaded operator *)
    let make_operator_name opname =
        let ada2npk_operator_prefix = "__ada2npk_operator_" in
         match opname with
        | "and" -> ada2npk_operator_prefix ^ "logical_and"
        | "or"  -> ada2npk_operator_prefix ^ "logical_and"
        | "xor" -> ada2npk_operator_prefix ^ "xor"
        | "="   -> ada2npk_operator_prefix ^ "equals"
        | "/="  -> ada2npk_operator_prefix ^ "not_equals"
        | "<"   -> ada2npk_operator_prefix ^ "lt"
        | "<="  -> ada2npk_operator_prefix ^ "le"
        | ">"   -> ada2npk_operator_prefix ^ "gt"
        | ">="  -> ada2npk_operator_prefix ^ "ge"
        | "+"   -> ada2npk_operator_prefix ^ "plus"
        | "-"   -> ada2npk_operator_prefix ^ "minus"
        | "&"   -> ada2npk_operator_prefix ^ "binary_and"
        | "*"   -> ada2npk_operator_prefix ^ "times"
        | "/"   -> ada2npk_operator_prefix ^ "div"
        | "mod" -> ada2npk_operator_prefix ^ "mod"
        | "rem" -> ada2npk_operator_prefix ^ "rem"
        | "**"  -> ada2npk_operator_prefix ^ "pow"
        | "abs" -> ada2npk_operator_prefix ^ "abs"
        | "not" -> ada2npk_operator_prefix ^ "not"
        | _     -> Npkcontext.report_error "Parser.make_operator_name"
                ("Cannot overload '"^opname^"' : it is not an operator")

    (** Prepares a list of exp*block for the Case constructor. It takes
      * a list of expression list * block and flattens it into a list of
      * expression*block.
      *)
    let rec build_case_ch (choices:(expression list*block)list)
        :(expression*block) list =
        match choices with
          | []                     -> []
          | (exp_list,block)::tail -> (List.map (function exp -> exp,block)
                                                exp_list)
                                     @ (build_case_ch tail)

%}
/*declaration ocamlyacc*/
%token EOF
%token PLUS MINUS MULT DIV POW CONCAT
%token <Syntax_ada.nat> CONST_INT
%token <string> CONST_FLOAT
%token <string> IDENT
%token <string> STRING
%token <int> CONST_CHAR
%token LT GT LE GE EQ NE
%token AND OR XOR NOT ANDTHEN ORELSE
%token MOD REM ABS
%token ASSIGN
%token WITH USE
%token BEGIN END
%token PACKAGE IS
%token BODY
%token PROCEDURE FUNCTION PACKAGE BODY
%token NEW TYPE RANGE CONSTANT SUBTYPE ARRAY RECORD OF
%token IN OUT RETURN
%token IF THEN ELSE ELSIF LOOP WHILE FOR EXIT WHEN
%token INTEGER FLOAT BOOLEAN CHARACTER
%token NULL TRUE FALSE
%token LPAR RPAR ARROW DOUBLE_DOT
%token COMMA SEMICOLON DOT COLON QUOTE
%token REVERSE PRAGMA CASE VBAR OTHERS

%start s
%type <Syntax_ada.compilation_unit> s

/*priorite*/

%%
/*grammaire*/
s: context library_item EOF
  {let (body,loc) = $2 in ($1, body, loc)}
;

context :
  {[]}
| context_item context {$1@$2}
;

context_item :
| WITH name_list SEMICOLON
    { let loc = loc () in
        List.map
          (fun name -> With(name, loc, None))
          $2}
| use_clause {[UseContext($1)]}
;

use_clause :
| USE name_list SEMICOLON {$2}
;

library_item :
| decl {let (spec, loc) = $1 in (Spec(spec), loc)}
| body {let (body, loc) = $1 in (Body(body), loc)}
;

body :
| subprogram_body {$1}
| package_body {$1}
;

decl :
| subprogram_decl {$1}
| package_decl {$1}
;

subprogram_decl :
| subprogram_spec SEMICOLON
    {let (spec, loc) = $1 in ((SubProgramSpec(spec)),loc)}
;

subprogram_body:
| subprogram_spec IS declarative_part BEGIN instr_list END name SEMICOLON
    {let (spec, loc) = $1
     in
       (check_end spec $7);
       (SubProgramBody(spec,$3,$5), loc)}

| subprogram_spec IS declarative_part BEGIN instr_list END SEMICOLON
    {let (spec, loc) = $1
     in (SubProgramBody(spec,$3,$5), loc)}
;

package_loc :
| PACKAGE {loc ()}
;

package_instr :
| {[]}
| BEGIN instr_list {$2}


package_decl :
| package_loc name IS basic_declarative_part END SEMICOLON
    {(PackageSpec($2, $4), $1)}
| package_loc name IS basic_declarative_part END name SEMICOLON
        { (check_name $2 $6);
          (PackageSpec($2, $4), $1)}

package_body:
| package_loc BODY name IS declarative_part package_instr END SEMICOLON
    {(PackageBody($3, None, $5, $6), $1)}

| package_loc BODY name IS declarative_part package_instr END name SEMICOLON
    { (check_name $3 $8);
      (PackageBody($3, None, $5, $6), $1)
    }

/* on renvoie aussi la position de la spec */
subprogram_spec :
| PROCEDURE  name  LPAR formal_part RPAR           {(Procedure($2,$4),  loc())}
| PROCEDURE  name                                  {(Procedure($2,[]),  loc())}
| FUNCTION   name  LPAR formal_part RPAR RETURN subtyp
                                                   {(Function ($2,$4,$7),loc())}
| FUNCTION   name                        RETURN subtyp
                                                   {(Function ($2,[],$4),loc())}
| FUNCTION  STRING LPAR formal_part RPAR RETURN subtyp
                          {(Function (([],make_operator_name $2), $4,$7),loc())}
| PROCEDURE STRING LPAR formal_part RPAR
                          {(Procedure(([],make_operator_name $2), $4),   loc())}
;

formal_part :
| parameter_specification {[$1]}
| parameter_specification SEMICOLON formal_part {$1::$3}
;

mode :
  {In}
| IN {In}
| OUT {Out}
| IN OUT {InOut}
;

parameter_specification :
| ident_list COLON mode subtyp                   {{formal_name=$1;
                                                          mode=$3;
                                                    param_type=$4;
                                                 default_value=None}}
| ident_list COLON mode subtyp ASSIGN expression {{formal_name=$1;
                                                          mode=$3;
                                                    param_type=$4;
                                                 default_value=Some $6}}
;

declarative_part :
  {[]}
| declarative_item declarative_part {$1::$2}
| pragma declarative_part {Npkcontext.report_warning "parser"
                           ("pragma '" ^ $1 ^ "' is ignored");
                           $2
                          }
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

basic_declarative_part :
  {[]}
| basic_declaration basic_declarative_part {$1::$2}
;

basic_declaration :
| ident_list COLON subtyp_indication SEMICOLON
        {(ObjectDecl($1,$3,None, Variable), loc ())}
| ident_list COLON subtyp_indication ASSIGN expression SEMICOLON
        {(ObjectDecl($1,$3,Some($5), Variable), loc ())}
| ident_list COLON CONSTANT subtyp_indication ASSIGN expression SEMICOLON
        {(ObjectDecl($1,$4,Some($6), Constant), loc ())}
| ident_list COLON CONSTANT ASSIGN expression SEMICOLON
        {(NumberDecl($1, $5, None), loc())}
| type_definition
        {($1, loc ())}
| SUBTYPE ident IS subtyp_indication SEMICOLON
        {(SubtypDecl($2,$4), loc ())}
| use_clause {(UseDecl($1), loc ())}
| decl  {let (spec, loc) = $1 in
           (SpecDecl(spec), loc)}
| representation_clause SEMICOLON {(RepresentClause($1), loc ())}
;

contrainte :
| simple_expr DOUBLE_DOT simple_expr
    {RangeConstraint($1, $3)}
;

type_definition :
| TYPE ident IS ARRAY constrained_array_definition SEMICOLON
                { TypeDecl(Array($2,$5))}
| TYPE ident IS LPAR ident_list RPAR SEMICOLON
    { TypeDecl(Ada_utils.make_enum ($2) $5)}
| TYPE ident IS NEW subtyp_indication SEMICOLON
        {TypeDecl(DerivedType($2, $5))}
| TYPE ident IS RANGE simple_expr DOUBLE_DOT simple_expr SEMICOLON
            { TypeDecl(Ada_utils.make_range $2 $5 $7)}
| TYPE ident IS RECORD record_definition END RECORD SEMICOLON
                { TypeDecl(Record($2, $5)) }
;


record_definition :
  {[]}
| ident_list COLON subtyp_indication SEMICOLON record_definition
                                                            { ($1,$3,None)::$5 }
| ident_list COLON subtyp_indication ASSIGN expression
                        SEMICOLON record_definition {($1,$3,Some($5))::$7}
;



/* TO DO : if record initialization exists (?)
| ident_list COLON subtyp_indication ASSIGN expression SEMICOLON
        {(ObjectDecl($1,$3,Some($5), Variable), loc ())}
| ident_list COLON CONSTANT subtyp_indication ASSIGN expression SEMICOLON
        {(ObjectDecl($1,$4,Some($6), Constant), loc ())}
| ident_list COLON CONSTANT ASSIGN expression SEMICOLON
        {(NumberDecl($1, $5, None), loc())}
*/


constrained_array_definition :
  LPAR matrix_indication RPAR OF subtyp_indication
/*  LPAR subtyp_indication RPAR OF subtyp_indication
  {ConstrainedArray($2,$5,None)}
*/
  {
    let list_rg = $2 in
    let rev_list = List.rev list_rg in
      build_matrix rev_list $5
  }
;

matrix_indication :
| subtyp_indication {[$1]}
| matrix_indication COMMA subtyp_indication {$1@[$3]}

array_component_association :
| ident ARROW expression {($1, $3)}
;

named_array_aggregate :
| array_component_association {[$1]}
| array_component_association COMMA named_array_aggregate {$1::$3}
;

array_aggregate :
| LPAR named_array_aggregate RPAR {NamedArrayAggregate($2)}
;

representation_clause :
| FOR ident USE array_aggregate {EnumerationRepresentation($2, $4)}
;

instr_list :
| instr SEMICOLON instr_list {$1::$3}
| instr SEMICOLON {[$1]}
;

instr :
| NULL {(NullInstr, loc ())}
| RETURN expression {(Return($2), loc ())}
| RETURN {(ReturnSimple, loc ())}
| procedure_array { let (n,p)= $1 in
                      (ProcedureCall(n,p), loc())
                  }
| procedure_array ASSIGN expression
    { let (nm, ind) = $1 in
      let arr_or_lv = build_access nm ind in
        (Assign ( arr_or_lv  , $3), loc())
    }
| EXIT {(Exit(None), loc() )}
| EXIT WHEN expression {(Exit(Some($3)), loc ())}
| instruction_if {$1}
| iteration_scheme LOOP instr_list END LOOP
      { let (scheme,loc) = $1
        in (Loop(scheme, $3), loc)}
| CASE expression IS case_stmt_alternative_list END CASE {Case($2,
                                                              build_case_ch
                                                                  (fst $4),
                                                              (snd $4)),
                                                          loc()}
;

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
/*| discrete_range TODO */
;

procedure_array :
| name      {($1, [])}
| name args {($1, $2)}

args:
| LPAR param_assoc RPAR { $2 }
| args LPAR param_assoc RPAR { $1@$3 } /*TO DO checkin
                                          this case length 3 = 1*/
param_assoc:
| expression {[$1]}
| expression COMMA param_assoc {$1::$3}


iteration_scheme :
| {(NoScheme, loc ())}
| WHILE expression {(While($2), loc ())}
| FOR ident IN         simple_expr DOUBLE_DOT simple_expr
                                                {(For($2,$4,$6,false), loc ())}
| FOR ident IN REVERSE simple_expr DOUBLE_DOT simple_expr
                                                {(For($2,$5,$7, true), loc ())}

debut_if :
| IF {loc()}
;

debut_elsif :
| ELSIF {loc()}
;

instruction_if :
| debut_if expression THEN instr_list instruction_else END IF
    {(If($2, $4, $5), $1)}
;

instruction_else :
  {[]}
| debut_elsif expression THEN instr_list instruction_else
      {[(If($2, $4, $5), $1)]}
| ELSE instr_list {$2}
;

expression :
| relation {$1}
| expr_andthen ANDTHEN relation {Binary(AndThen, $1, $3)}
| expr_orelse  ORELSE  relation {Binary(OrElse,  $1, $3)}
| expr_and     AND     relation {Binary(And,     $1, $3)}
| expr_or      OR      relation {Binary(Or,      $1, $3)}
| expr_xor     XOR     relation {Binary(Xor,     $1, $3)}
;

expr_and :
| relation {$1}
| expr_and AND relation {Binary(And, $1, $3)}
;

expr_andthen :
| relation {$1}
| expr_andthen ANDTHEN relation {Binary(AndThen, $1, $3)}
;

expr_or :
| relation {$1}
| expr_or OR relation {Binary(Or, $1, $3)}
;

expr_orelse :
| relation {$1}
| expr_orelse ORELSE relation {Binary(OrElse, $1, $3)}
;

expr_xor :
| relation {$1}
| expr_xor XOR relation {Binary(Xor, $1, $3)}
;

rel_op :
| EQ {Eq}
| NE {Neq}
| LE {Le}
| GE {Ge}
| LT {Lt}
| GT {Gt}
;

relation :
| simple_expr {$1}
| simple_expr rel_op simple_expr {Binary($2,$1,$3)}
;

add_op :
| PLUS {Plus}
| MINUS {Minus}
| CONCAT {Concat}
;

simple_expr :
| term {$1}
| PLUS term {Unary(UPlus,$2)}
| MINUS term {Unary(UMinus,$2)}
| simple_expr add_op term {Binary($2,$1,$3)}
/*WG added for type'LAST,FIRST
| subtyp QUOTE LAST {Last($1)}
| subtyp QUOTE FIRST {First($1)}
*/
;

mult_op :
| MULT {Mult}
| DIV {Div}
| MOD {Mod}
| REM {Rem}
;

term :
| factor {$1}
| term mult_op factor {Binary($2,$1,$3)}

factor :
| primary {$1}
| primary POW primary {Binary(Power,$1,$3)}
| NOT primary {Unary(Not,$2)}
| ABS primary {Unary(Abs,$2)}
;

primary :
| NULL {NullExpr}
| integer_literal {CInt($1)}
| CONST_FLOAT {CFloat(float_of_string $1,$1)}
| CONST_CHAR {CChar($1)}
| TRUE {CBool(true)}
| FALSE {CBool(false)}
| STRING {CString($1)}
| name {Var($1)}
| LPAR expression RPAR {$2}
| subtyp QUOTE LPAR expression RPAR {Qualified($1,$4)}
| name args {FunctionCall($1, $2)}
| subtyp QUOTE ident  {Attribute ($1, AttributeDesignator(String.lowercase $3,
                                                          None))}
| name LPAR actual_parameter_part RPAR {FunctionCall($1, $3)}
;

integer_literal :
| CONST_INT {$1}
;

typ :
| INTEGER {Constrained(Integer, Ada_config.integer_constraint, true)}
| FLOAT {Unconstrained(Float)}
| BOOLEAN {Unconstrained(Boolean)}
| CHARACTER {Unconstrained(Character)}
;

subtyp_indication :
| subtyp RANGE contrainte {($1, Some($3), None)}
| subtyp {($1, None, None)}
/*| simple_expr DOUBLE_DOT simple_expr */
| CONST_INT DOUBLE_DOT CONST_INT
    {(Constrained(Integer, Ada_config.integer_constraint, true),
      Some (RangeConstraint(CInt($1), CInt($3))),
      None)}


subtyp :
| typ {$1}
| name {SubtypName($1)}
;

procedure_call :
| name {ProcedureCall($1, [])}
| name LPAR actual_parameter_part RPAR {ProcedureCall($1, $3)}
;

actual_parameter_part :
| parameter_association {[$1]}
| parameter_association COMMA actual_parameter_part {$1::$3}
;

parameter_association:
| expression {$1}
;


ident :
| IDENT {$1}
;

ident_list :
| ident                  {$1::[]}
| ident COMMA ident_list {$1::$3}
;

name_list :
| name {[$1]}
| name COMMA name_list {$1::$3}
;

name :
| ident {([],$1)}
| name DOT ident /* TODO : trouver une meilleur solution */
      {
    let (par, ident) = $1
    in (par@[ident], $3)}

    /* pour les tableaux */
/*| name LPAR parameter_association RPAR {FunctionCall($1, $3)} */
;


%%


