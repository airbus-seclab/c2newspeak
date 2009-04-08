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

%{ open Syntax_ada

   let check_ident i1 i2 =
     if (String.compare i1 i2) <> 0
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
    let make_parameter_specification (idents:identifier list)
                                     (common_mode:param_mode)
                                     (common_type:subtyp)
                                     (common_default_value:expression option)
        :param list =
        List.map (function x -> {formal_name=x;
                                        mode=common_mode;
                                  param_type=common_type;
                               default_value=common_default_value;})
                 idents

    (**
     * Prepend a symbol table to a declarative part.
     * Input : a list of declarative items
     *)
    let make_declarative_part (l:(declarative_item*location) list)
            :declarative_part =
      (Ada_types.create_table (List.length l)),l

%}
/*declaration ocamlyacc*/
%token EOF

%token                     ABS
%token                     AND
%token                     ANDTHEN
%token                     ARRAY
%token                     ARROW
%token <Newspeak.location> ASSIGN
%token                     BEGIN
%token                     BODY
%token <Newspeak.location> CASE
%token                     COLON
%token                     COMMA
%token                     CONCAT
%token                     CONSTANT
%token <Syntax_ada.nat>    CONST_INT
%token <int>               CONST_CHAR
%token <string>            CONST_FLOAT
%token <Newspeak.location> DECLARE
%token                     DIV
%token                     DOT
%token                     DOUBLE_DOT
%token                     ELSE
%token <Newspeak.location> ELSIF
%token                     END
%token                     EQ
%token <Newspeak.location> EXIT
%token                     FALSE
%token                     FOR
%token                     FUNCTION
%token                     GE
%token                     GT
%token <string>            IDENT
%token                     IN
%token <Newspeak.location> IF
%token                     IS
%token                     LE
%token                     LOOP
%token                     LPAR
%token                     LT
%token                     MINUS
%token                     MOD
%token                     MULT
%token                     NE
%token                     NEW
%token                     NOT
%token <Newspeak.location> NULL
%token                     OF
%token                     OR
%token                     ORELSE
%token                     OTHERS
%token                     OUT
%token <Newspeak.location> PACKAGE
%token                     PLUS
%token                     POW
%token                     PRAGMA
%token                     PROCEDURE
%token                     QUOTE
%token                     RANGE
%token                     RECORD
%token                     REM
%token <Newspeak.location> RETURN
%token                     REVERSE
%token                     RPAR
%token                     SEMICOLON
%token                     SUBTYPE
%token <string>            STRING
%token                     THEN
%token                     TRUE
%token                     TYPE
%token                     USE
%token                     VBAR
%token                     WHEN
%token                     WHILE
%token <Newspeak.location> WITH 
%token                     XOR

%left AND ANDTHEN OR ORELSE XOR
%left EQ NE LT LE GT GE
%left PLUS MINUS CONCAT
%nonassoc UPLUS UMINUS
%left MULT DIV MOD REM
%left POW ABS NOT

%start s
%type <Syntax_ada.compilation_unit> s

%type <identifier> pragma
%type <param_mode> mode
%type <name> name
%type <name list> name_list use_clause
%type <identifier> ident
%type <identifier list> ident_list
%type <argument> parameter_association
%type <argument list> actual_parameter_part args
%type <subtyp> subtyp
%type <subtyp_indication> subtyp_indication
%type <expression> expression discrete_choice
%type <instruction*location> instr
%type <instruction> procedure_call
%type <block> instr_list instruction_else when_others
%type <name*argument list> lvalue
%type <iteration_scheme*location> iteration_scheme
%type <expression list> discrete_choice_list
%type <expression list*block> case_stmt_alternative
%type <(expression list*block) list* block option> case_stmt_alternative_list
%type <representation_clause> representation_clause
%type <array_aggregate> array_aggregate
%type <(identifier * expression) list> named_array_aggregate
%type <identifier * expression> array_component_association
%type <subtyp_indication list> matrix_indication
%type <array_type_definition> constrained_array_definition
%type <field list> record_definition
%type <contrainte> contrainte
%type <basic_declaration> type_definition
%type <basic_declaration*location> basic_declaration
%type <(basic_declaration*location) list> basic_declarative_part
%type <declarative_item*location> declarative_item
%type <(declarative_item*location) list> declarative_part
%type <unit> pragma_argument_association pragma_argument_association_list
%type <param list> parameter_specification formal_part
%type <sub_program_spec*location> subprogram_spec
%type <spec*location> decl
%type <body*location> body
%type <block> package_instr
%type <library_item*location> library_item
%type <context_clause list> context_item context

/*priorite*/

%%
/*grammaire*/
s: context library_item EOF {($1, fst $2, snd $2)}
;

context :
| {[]}
| context_item context {$1@$2}
;

context_item :
| WITH name_list SEMICOLON { List.map (fun name -> With(name, $1, None)) $2}
| use_clause               {[UseContext $1]}
;

use_clause :
| USE name_list SEMICOLON {$2}
;

library_item :
| decl {let (spec, loc) = $1 in (Spec(spec), loc)}
| body {let (body, loc) = $1 in (Body(body), loc)}
;

body :
| subprogram_spec IS declarative_part BEGIN instr_list END name SEMICOLON
    {let (spec, loc) = $1
     in
       (check_end spec $7);
       (SubProgramBody(spec,make_declarative_part $3,$5), loc)}

| subprogram_spec IS declarative_part BEGIN instr_list END SEMICOLON
    {let (spec, loc) = $1
     in (SubProgramBody(spec,make_declarative_part $3,$5), loc)}
| PACKAGE BODY name IS declarative_part package_instr END SEMICOLON
    {(PackageBody($3, None, make_declarative_part $5, $6), $1)}

| PACKAGE BODY name IS declarative_part package_instr END name SEMICOLON
    { (check_name $3 $8);
      (PackageBody($3, None, make_declarative_part $5, $6), $1)
    }
;

decl :
| subprogram_spec SEMICOLON
    {let (spec, loc) = $1 in ((SubProgramSpec(spec)),loc)}
| PACKAGE name IS basic_declarative_part END SEMICOLON
    {(PackageSpec($2, $4), $1)}
| PACKAGE name IS basic_declarative_part END name SEMICOLON
        { (check_name $2 $6);
          (PackageSpec($2, $4), $1)}
;

package_instr :
| {[]}
| BEGIN instr_list {$2}

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
| pragma           declarative_part {Npkcontext.report_warning "parser"
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
| {[]}
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
| expression DOUBLE_DOT expression {RangeConstraint($1, $3)}
;

type_definition :
| TYPE ident IS ARRAY constrained_array_definition SEMICOLON
                { TypeDecl(Array($2,$5))}
| TYPE ident IS LPAR ident_list RPAR SEMICOLON
    { TypeDecl(Ada_utils.make_enum ($2) $5)}
| TYPE ident IS NEW subtyp_indication SEMICOLON
        {TypeDecl(DerivedType($2, $5))}
| TYPE ident IS RANGE expression DOUBLE_DOT expression SEMICOLON
            { TypeDecl(Ada_utils.make_range $2 $5 $7)}
| TYPE ident IS RECORD record_definition END RECORD SEMICOLON
                { TypeDecl(Record($2, $5)) }
;


record_definition :
| {[]}
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
| LPAR matrix_indication RPAR OF subtyp_indication
                                                {build_matrix (List.rev $2) $5}
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
| FOR ident USE array_aggregate         {EnumerationRepresentation($2,$4)}
| FOR subtyp QUOTE ident USE expression {AttributeDefinitionClause($2,$4,$6)}
;

instr_list :
| instr SEMICOLON            {$1::[]}
| instr SEMICOLON instr_list {$1::$3}
;

instr :
| NULL {(NullInstr, $1)}
| RETURN expression {(Return($2), $1)}
| RETURN {ReturnSimple, $1}
| procedure_call {$1, loc()}
| lvalue ASSIGN expression
    { let (nm, ind) = $1 in
        (Assign ( (build_access nm (List.map snd ind)) , $3), $2)
    }
| EXIT {Exit, $1}
| EXIT WHEN expression { If($3,[Exit,$1],[]), $1
  (* EXIT WHEN x --> IF x THEN EXIT END IF *) }
| IF expression THEN instr_list instruction_else END IF
    {(If($2, $4, $5), $1)}
| iteration_scheme LOOP instr_list END LOOP
      { let (scheme,loc) = $1
        in (Loop(scheme, $3), loc)}
| CASE expression IS case_stmt_alternative_list END CASE {Case($2,
                                                              build_case_ch
                                                                  (fst $4),
                                                              (snd $4)),
                                                          $1}
| DECLARE declarative_part BEGIN instr_list END
                                {Block (make_declarative_part $2,$4),$1}
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
;

lvalue :
| name      {($1, [])}
| name args {($1, $2)}

args:
|      LPAR actual_parameter_part RPAR {    $2 }
| args LPAR actual_parameter_part RPAR { $1@$3 }

iteration_scheme :
| {(NoScheme, loc ())}
| WHILE expression {(While($2), loc ())}
| FOR ident IN         expression DOUBLE_DOT expression
                                                {(For($2,$4,$6,false), loc ())}
| FOR ident IN REVERSE expression DOUBLE_DOT expression
                                                {(For($2,$5,$7, true), loc ())}

instruction_else :
| {[]}
| ELSIF expression THEN instr_list instruction_else
      {[(If($2, $4, $5), $1)]}
| ELSE instr_list {$2}
;

expression :
| expression ANDTHEN expression {Binary(AndThen, $1, $3)}
| expression ORELSE  expression {Binary(OrElse , $1, $3)}
| expression AND     expression {Binary(And    , $1, $3)}
| expression OR      expression {Binary(Or     , $1, $3)}
| expression XOR     expression {Binary(Xor    , $1, $3)}
| expression MULT    expression {Binary(Mult   , $1, $3)}
| expression DIV     expression {Binary(Div    , $1, $3)}
| expression MOD     expression {Binary(Mod    , $1, $3)}
| expression REM     expression {Binary(Rem    , $1, $3)}
| expression PLUS    expression {Binary(Plus   , $1, $3)}
| expression MINUS   expression {Binary(Minus  , $1, $3)}
| expression CONCAT  expression {Binary(Concat , $1, $3)}
| expression EQ      expression {Binary(Eq     , $1, $3)}
| expression NE      expression {Binary(Neq    , $1, $3)}
| expression LE      expression {Binary(Le     , $1, $3)}
| expression GE      expression {Binary(Ge     , $1, $3)}
| expression LT      expression {Binary(Lt     , $1, $3)}
| expression GT      expression {Binary(Gt     , $1, $3)}
| expression POW     expression {Binary(Power  , $1, $3)}
| PLUS  expression %prec UPLUS  {Unary (UPlus  , $2    )}
| MINUS expression %prec UMINUS {Unary (UMinus , $2    )}
| NOT   expression              {Unary (Not    , $2    )}
| ABS   expression              {Unary (Abs    , $2    )}
| NULL {NullExpr}
| CONST_INT   {CInt($1)}
| CONST_FLOAT {CFloat(float_of_string $1,$1)}
| CONST_CHAR  {CChar($1)}
| TRUE        {CBool(true)}
| FALSE       {CBool(false)}
| STRING      {CString($1)}
| LPAR expression RPAR {$2}
| subtyp QUOTE LPAR expression RPAR {Qualified($1,$4)}
| subtyp QUOTE ident  {Attribute ($1
                                 ,AttributeDesignator(String.lowercase $3
                                                     ,None
                                                     )
                                 )
                      }
| name {Var($1)}
| name LPAR actual_parameter_part RPAR {FunctionCall($1, $3)}
;

subtyp_indication :
| subtyp RANGE contrainte {($1, Some($3), None)}
| subtyp {($1, None, None)}
/*| simple_expr DOUBLE_DOT simple_expr */
| CONST_INT DOUBLE_DOT CONST_INT
    {(Constrained(Integer
                 ,Ada_config.integer_constraint
                 ,true
                 )
     ,Some(RangeConstraint(CInt($1)
                          ,CInt($3)
                          )
          )
     ,None
     )
    }

subtyp :
| name {SubtypName($1)}
;

procedure_call :
| name {ProcedureCall($1, [])}
| name LPAR actual_parameter_part RPAR {ProcedureCall($1, $3)}
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
| IDENT {$1}
;

ident_list :
| ident                  {$1::[]}
| ident COMMA ident_list {$1::$3}
;

name_list :
| name                 {$1::[]}
| name COMMA name_list {$1::$3}
;

name :
| ident {([],$1)}
| name DOT ident /* TODO : trouver une meilleur solution */
      {
    let (par, ident) = $1
    in (par@[ident], $3)}
;

%%

