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

  Jasmine Duchon
  email : jasmine . duchon AT free . fr
  
*/

%{ open Syntax_ada

   let compare_ident i1 i2 = 
     String.compare i1 i2

   let check_ident i1 i2 = 
     if (compare_ident i1 i2) <> 0
     then 
       Npkcontext.error "Parser.parse_error" 
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
           [] -> Npkcontext.error "Parser.parse_error" 
	     ("in build matrix, no subtyp given ")
	 | hd::[] -> ConstrainedArray(hd, typ_ind, None)
	 | hd::tl ->
	     ConstrainedArray(hd, 
			      (crafted tl typ_ind), 
			      None)
	       
	       
	       
	  

%}
/*declaration ocamlyacc*/
%token EOF
%token PLUS MOINS FOIS DIV PUISS CONCAT
%token <string> CONST_INT
%token <string> CONST_FLOAT
%token <string> IDENT
%token <string> STRING
%token <int> CONST_CHAR
%token LT GT LE GE EQ NE
%token AND OR XOR NOT ANDTHEN ORELSE
%token MOD REM ABS
%token AFFECT
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
%token PAR_G PAR_D FLECHE H_DEUX_POINTS
%token VIR POINT_VIR POINT DEUX_POINTS QUOTE
%token LAST FIRST LENGTH
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
| WITH name_list POINT_VIR 
    { let loc = loc () in
	List.map
	  (fun name -> With(name, loc, None))
	  $2}
| use_clause {[UseContext($1)]}
;

use_clause :
| USE name_list POINT_VIR {$2}
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
| subprogram_spec POINT_VIR
    {let (spec, loc) = $1 in ((SubProgramSpec(spec)),loc)}
;

subprogram_body:
| subprogram_spec IS declarative_part BEGIN instr_list END name POINT_VIR
    {let (spec, loc) = $1
     in 
       (check_end spec $7);
       (SubProgramBody(spec,$3,$5), loc)}

| subprogram_spec IS declarative_part BEGIN instr_list END POINT_VIR
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
| package_loc name IS basic_declarative_part END POINT_VIR
    {(PackageSpec($2, $4), $1)}
| package_loc name IS basic_declarative_part END name POINT_VIR
	{ (check_name $2 $6);
	  (PackageSpec($2, $4), $1)}

package_body:
| package_loc BODY name IS declarative_part package_instr END POINT_VIR
    {(PackageBody($3, None, $5, $6), $1)}

| package_loc BODY name IS declarative_part package_instr END name POINT_VIR 
    { (check_name $3 $8);
      (PackageBody($3, None, $5, $6), $1) 
    }

/* on renvoie aussi la position de la spec */
subprogram_spec :
| PROCEDURE name PAR_G formal_part PAR_D 
    {(Procedure($2,$4), loc ())}
| FUNCTION name PAR_G formal_part PAR_D RETURN subtyp
	{(Function($2,$4,$7), loc ())}
| PROCEDURE name {(Procedure($2,[]), loc ())}
| FUNCTION name RETURN subtyp {(Function($2,[],$4), loc ())}
;

formal_part : 
| parameter_specification {[$1]}
| parameter_specification POINT_VIR formal_part {$1::$3}
;

mode :
  {In}
| IN {In}
| OUT {Out}
| IN OUT {InOut}
;

parameter_specification : 
| ident_list DEUX_POINTS mode subtyp 
    {{pnom=$1;mode=$3;ptype=$4;pdef=None}}
| ident_list DEUX_POINTS mode subtyp AFFECT expression 
      {{pnom=$1;mode=$3;ptype=$4;pdef=Some($6)}}
;

declarative_part :
  {[]} 
| declarative_item declarative_part {$1::$2}
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
| ident_list DEUX_POINTS subtyp_indication POINT_VIR
        {(ObjectDecl($1,$3,None, Variable), loc ())}
| ident_list DEUX_POINTS subtyp_indication AFFECT expression POINT_VIR
	{(ObjectDecl($1,$3,Some($5), Variable), loc ())}
| ident_list DEUX_POINTS CONSTANT subtyp_indication AFFECT expression POINT_VIR
	{(ObjectDecl($1,$4,Some($6), Constant), loc ())}
| ident_list DEUX_POINTS CONSTANT AFFECT expression POINT_VIR 
	{(NumberDecl($1, $5, None), loc())}
| type_definition 
	{($1, loc ())}
| SUBTYPE ident IS subtyp_indication POINT_VIR
	{(SubtypDecl($2,$4), loc ())}
| use_clause {(UseDecl($1), loc ())}
| decl  {let (spec, loc) = $1 in 
	   (SpecDecl(spec), loc)}
| representation_clause POINT_VIR {(RepresentClause($1), loc ())}
;

contrainte :
| simpl_expr H_DEUX_POINTS simpl_expr 
    {RangeConstraint($1, $3)}
;

type_definition :
| TYPE ident IS ARRAY constrained_array_definition POINT_VIR 
		{ TypeDecl(Array($2,$5))}
| TYPE ident IS PAR_G ident_list PAR_D POINT_VIR
    { TypeDecl(Ada_utils.make_enum ($2) $5)}
| TYPE ident IS NEW subtyp_indication POINT_VIR
	{TypeDecl(DerivedType($2, $5))}
| TYPE ident IS RANGE simpl_expr H_DEUX_POINTS simpl_expr POINT_VIR
	    { TypeDecl(Ada_utils.make_range $2 $5 $7)}
/*| TYPE ident is RECORD record_definition END RECORD POINT_VIR
		{ TypeDecl() } 	

;


record_definition :
  {[]} 
| ident_list DEUX_POINTS subtyp_indication POINT_VIR record_definition {$1::$2}
      {(ObjectDecl($1,$3,None, Variable), loc ())}
;
*/


/* 
TO DO : check if initialization is allowed in records
| ident_list DEUX_POINTS subtyp_indication AFFECT expression POINT_VIR
	{(ObjectDecl($1,$3,Some($5), Variable), loc ())}
| ident_list DEUX_POINTS CONSTANT subtyp_indication AFFECT expression POINT_VIR
	{(ObjectDecl($1,$4,Some($6), Constant), loc ())}
| ident_list DEUX_POINTS CONSTANT AFFECT expression POINT_VIR 
	{(NumberDecl($1, $5, None), loc())}
*/

constrained_array_definition : 
  PAR_G matrix_indication PAR_D OF subtyp_indication 
/*  PAR_G subtyp_indication PAR_D OF subtyp_indication  
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
| matrix_indication VIR subtyp_indication {$1@[$3]}

array_component_association :
| ident FLECHE expression {($1, $3)} 
;

named_array_aggregate :
| array_component_association {[$1]}
| array_component_association VIR named_array_aggregate {$1::$3}
;

array_aggregate :
| PAR_G named_array_aggregate PAR_D {NamedArrayAggregate($2)}
;

representation_clause :
| FOR ident USE array_aggregate {EnumerationRepresentation($2, $4)}
;

instr_list :
| instr POINT_VIR instr_list {$1::$3}
| instr POINT_VIR {[$1]}
;

instr :
| NULL {(NullInstr, loc ())}
| RETURN expression {(Return($2), loc ())}
| RETURN {(ReturnSimple, loc ())}

| procedure_array { let (n,p)= $1 in 
		      (ProcedureCall(n,p), loc())
		  }

| procedure_array AFFECT expression 
    { let (nm, ind) = $1 in
      let arr_or_lv = build_access nm ind in 
	(Affect ( arr_or_lv  , $3), loc())
    }

| EXIT {(Exit(None), loc() )}
| EXIT WHEN expression {(Exit(Some($3)), loc ())}
| instruction_if {$1}
| iteration_scheme LOOP instr_list END LOOP 
      { let (scheme,loc) = $1
	in (Loop(scheme, $3), loc)}
;

procedure_array :
| name     {($1, [])} 
| name args {($1, $2)} 


args: 
| PAR_G param_assoc PAR_D { $2 }     
| args PAR_G param_assoc PAR_D { $1@$3 } /*TO DO checkin
					  this case length 3 = 1*/  
param_assoc:
| expression {[$1]}
| expression VIR param_assoc {$1::$3}
      

iteration_scheme :
| {(NoScheme, loc ())}
| WHILE expression {(While($2), loc ())}
;

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
| expr_orelse ORELSE relation {Binary(OrElse, $1, $3)}
| expr_and AND relation {Binary(And, $1, $3)}
| expr_or OR relation {Binary(Or, $1, $3)}
| expr_xor XOR relation {Binary(Xor, $1, $3)}
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
| simpl_expr {$1}
| simpl_expr rel_op simpl_expr {Binary($2,$1,$3)}
;

add_op :
| PLUS {Plus}
| MOINS {Moins}
| CONCAT {Concat}
;

simpl_expr :
| term {$1}
| PLUS term {Unary(UPlus,$2)}
| MOINS term {Unary(UMoins,$2)}
| simpl_expr add_op term {Binary($2,$1,$3)}
/*WG added for type'LAST,FIRST 
| subtyp QUOTE LAST {Last($1)}
| subtyp QUOTE FIRST {First($1)} 
*/
;

mult_op :
| FOIS {Fois}
| DIV {Div}
| MOD {Mod}
| REM {Rem}
;

term :
| factor {$1}
| term mult_op factor {Binary($2,$1,$3)}
      
factor :
| primary {$1}
| primary PUISS primary {Binary(Puissance,$1,$3)}
| NOT primary {Unary(Not,$2)}
| ABS primary {Unary(Abs,$2)}
;

primary :
| NULL {NullExpr}
| CONST_INT {CInt(Newspeak.Nat.of_string $1)}
| CONST_FLOAT {CFloat(float_of_string $1,$1)}
| CONST_CHAR {CChar($1)}
| TRUE {CBool(true)}
| FALSE {CBool(false)}
| STRING {CString($1)}
| name {Var($1)}
| PAR_G expression PAR_D {$2}
| subtyp QUOTE PAR_G expression PAR_D {Qualified($1,$4)}
| name args {FunctionCall($1, $2)}
/*from simpl_exp to */

| subtyp QUOTE LAST {Last($1)}
| subtyp QUOTE FIRST {First($1)} 
| subtyp QUOTE LENGTH {Length($1)}

/*| name PAR_G param_assoc PAR_D {FunctionCall($1, $3)}*/
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
/*| simpl_expr H_DEUX_POINTS simpl_expr */
| CONST_INT H_DEUX_POINTS CONST_INT 
    {(Constrained(Integer, Ada_config.integer_constraint, true),
      Some (RangeConstraint(CInt(Newspeak.Nat.of_string $1), 
			    CInt(Newspeak.Nat.of_string $3))
			),
      None)}


subtyp :
| typ {$1}
| name {SubtypName($1)}
;

/*
procedure_call : 
| name {ProcedureCall($1, [])}
| name PAR_G param_assoc PAR_D {ProcedureCall($1, $3)}
;
param_assoc :
| expression {[$1]}
| expression VIR param_assoc {$1::$3}
;
*/


ident :
| IDENT {$1}
;

ident_list :
| ident {[$1]}
| ident VIR ident_list {$1::$3}
;

name_list :
| name {[$1]}
| name VIR name_list {$1::$3}
;

name : 
| ident {([],$1)}
| name POINT ident /* TODO : trouver une meilleur solution */
      {
    let (par, ident) = $1
    in (par@[ident], $3)}
    
    /* pour les tableaux */
/*| name PAR_G param_assoc PAR_D {FunctionCall($1, $3)} */
;      


%%
  
  
