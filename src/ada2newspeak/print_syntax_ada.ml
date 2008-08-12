(*
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
  
*)

open Syntax_ada

(* Fonction transformant une liste [e1 e2 ... en ]
   en string de la forme 
   [ (to_string e1) sep (to_string e2) sep ... sep (to_string en)]
   si crochet = true, sans [ ] sinon
 *)

let list_to_string list to_string sep crochet = 
  match list with
    | a::r -> 
	(if crochet then "[" else "")
	^(to_string a)
	^(List.fold_left (fun debut x -> debut^sep^(to_string x)) 
	  "" r) 
	^(if crochet then "]" else "")
    | [] -> 
	if crochet then "[]" else ""
  
let option_to_string a to_string = match a with
  | None -> "None"
  | Some(a') -> "Some("^(to_string a')^")"

let rec ident_list_to_string l = 
  list_to_string l (fun x -> x) "." false

let rec name_to_string (packages, ident) = 
  ident_list_to_string (packages@[ident])

   
let line_of_loc (_,line,_) = "line "^(string_of_int line)

let mode_to_string mode = match mode with
  | In -> "in"
  | Out -> "out"
  | InOut -> "in out"

let ikind_to_string (s,taille) = 
  let string_signe = match s with
    | Newspeak.Signed -> "Signed"
    | Newspeak.Unsigned -> "Unsigned"
  in
    "("^string_signe^", "^(string_of_int taille)^")"



let uop_to_string op = match op with
  | UPlus -> "UPlus"
  | UMoins -> "UMoins"
  | Abs -> "Abs"
  | Not -> "Not"

let bop_to_string op = match op with
  | Plus -> "Plus"
  | Moins -> "Moins" 
  | Fois -> "Fois" 
  | Div -> "Div"
  | Puissance -> "Puissance"
  | Concat -> "Concat"
  | Mod -> "Mod"
  | Rem -> "Rem"
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Le -> "Le"
  | Lt -> "Lt"
  | Ge -> "Ge"
  | Gt -> "Gt"
  | And -> "And"
  | Or -> "Or"
  | Xor -> "Xor"
  | AndThen -> "AndThen"
  | OrElse -> "OrElse"

let rec typ_to_string typ = match typ with
  | Integer -> "Integer"
  | IntegerConst -> "IntegerConst"
  | Float -> "Float"
  | Boolean -> "Boolean"
  | Character -> "Character"
  | Declared(typ_decl,loc) -> "Declared("
      ^(typ_declaration_to_string typ_decl)^","
      ^(line_of_loc loc)^")"
  | String -> "String"

and typ_declaration_to_string typ_decl = match typ_decl with
  | Enum(ident, val_list, taille) ->
      "Enum("^ident^", "
      ^(list_to_string val_list 
	  (fun (nom,id) -> "("^nom^","^(string_of_int id)^")")
	  "; " true)
      ^", "^(string_of_int taille)^")"

  | DerivedType(ident, subtyp) ->
      "DerivedType("^ident^", "
      ^(subtyp_indication_to_string subtyp)^")"

  | IntegerRange(ident, contrainte, taille) ->
      "IntegerRange("^ident
      ^", "^(contrainte_to_string contrainte)
      ^", "^(option_to_string taille ikind_to_string)^")"
	  
and exp_to_string exp = match exp with
  | NullExpr -> "NullExpr"
  | CInt(i) -> "CInt("^(string_of_int i)^")"
  | CFloat(f,s) -> "CFloat("^(string_of_float f)^s^")"
  | CBool(b) -> "CBool("^(string_of_bool b)^")"
  | CChar(c) -> "CChar("^(string_of_int c)^")"
  | CString(s) -> "CString("^s^")"
  | Var(s) -> "Var("^(name_to_string s)^")"
  | Unary(op,exp) -> "Unary("^(uop_to_string op)^", "
      ^(exp_to_string exp)^")"
  | Binary(op,e1,e2) -> "Binary("^(bop_to_string op)^", "
      ^(exp_to_string e1)
      ^", "^(exp_to_string e2)^")"
  | Qualified(subtyp, exp) -> "Qualified("
      ^(subtyp_to_string subtyp)
      ^", "^(exp_to_string exp)^")"
  
  | FunctionCall(nom, params) -> "FunctionCall("
      ^(name_to_string nom)^", "
      ^(list_to_string params exp_to_string ", " true)^")"

and subtyp_to_string subtyp = match subtyp with
  | Unconstrained(typ) -> "Unconstrained("^(typ_to_string typ)^")"
  | SubtypName(name) -> "SubtypName("^(name_to_string name)^")"
  | Constrained(typ, contrainte, static) ->
      "Constrained("^(typ_to_string typ)^", "
      ^(contrainte_to_string contrainte)^", "
      ^(string_of_bool static)^")"

and subtyp_indication_to_string (subtyp_ref, contrainte, subtyp) =
  "("^(subtyp_to_string subtyp_ref)^", "
  ^(option_to_string contrainte contrainte_to_string)^", "
  ^(option_to_string subtyp subtyp_to_string)^")"

and contrainte_to_string contrainte = match contrainte with
  | RangeConstraint(e1, e2) -> 
      "RangeConstraint("^(exp_to_string e1)
      ^", "^(exp_to_string e2)^")"
  |  IntegerRangeConstraint(v1, v2, bounds) ->
       "IntegerRangeConstraint("^(string_of_int v1)
      ^", "^(string_of_int v2)
       ^", "^(Newspeak.string_of_bounds bounds)^")"
  |  FloatRangeConstraint(v1, v2) ->
       "FloatRangeConstraint("^(string_of_float v1)
      ^", "^(string_of_float v2)^")"
  | NullRange -> "NullRange"

and value_to_string v = match v with
  | IntVal(i) -> "IntVal("^(string_of_int i)^")"
  | FloatVal(f) -> "FloatVal("^(string_of_float f)^")"
  | EnumVal(i) -> "IntVal("^(string_of_int i)^")"
  | BoolVal(b) -> "BoolVal("^(string_of_bool b)^")"

let iteration_scheme_to_string scheme = match scheme with
  | NoScheme -> "NoScheme"
  | While(exp) -> "While("^(exp_to_string exp)^")"

let rec instr_list_to_string instr_list = 
  list_to_string instr_list
    (fun (instr,loc) -> "("^
       (instr_to_string instr)^", "
       ^(line_of_loc loc)^")")
    ";\n" true
and instr_to_string instr = match instr with
  | NullInstr -> "NullInstr"
  | ReturnSimple -> "ReturnSimple"
  | Return(exp) -> "Return("^(exp_to_string exp)^")"
  | Affect(var,exp) -> "Affect("^(name_to_string var)
      ^", "^(exp_to_string exp)^")"
  | If(exp, instr_then, instr_else) -> 
      "If("^(exp_to_string exp)^",\n"
      ^(instr_list_to_string instr_then)^",\n"
      ^(instr_list_to_string instr_else)^")"
	
  | Exit(exp) -> "Exit("^(option_to_string exp exp_to_string)^")"
  | Loop(scheme, instr_list) ->
      "Loop("^(iteration_scheme_to_string scheme)^",\n"
      ^(instr_list_to_string instr_list)^")"
  | ProcedureCall(nom, params) -> "ProcedureCall("
      ^(name_to_string nom)^", "
      ^(list_to_string params exp_to_string ", " true)^")"


let param_to_string param = 
  "{pnom="^(list_to_string param.pnom (fun x -> x) "," true)
  ^"; mode="^(mode_to_string param.mode)
  ^"; ptype="^(subtyp_to_string param.ptype)
  ^"; pdef="^(option_to_string param.pdef exp_to_string)^"}"

let param_list_to_string list = 
  list_to_string list param_to_string ";\n" true

let object_state_to_string status = match status with
  | Variable -> "Variable"
  | Constant -> "Constant"
  | StaticVal(value) -> "StaticVal("^(value_to_string value)^")"


let rec context_clause_to_string context_clause = 
  match context_clause with
    | With(name, loc, spec) -> "With("
	^(name_to_string name)
	^", "^(line_of_loc loc)^",\n"
	^(option_to_string 
	    spec
	    (fun (spec, loc) -> "("^(spec_to_string spec)
	       ^", "^(line_of_loc loc)^")"))
	^")"
    | UseContext(names) -> "UseContext("
	^(list_to_string names name_to_string "," false)^")"

and context_to_string context = list_to_string context context_clause_to_string ";\n" true

and basic_declaration_to_string basic_decl = match basic_decl with
  | ObjectDecl(idents,subtyp_ind,def,status) -> "ObjectDecl("
      ^(list_to_string idents (fun x-> x) "," true)
      ^", "^(subtyp_indication_to_string subtyp_ind)
      ^", "^(option_to_string def exp_to_string)
      ^", "^(object_state_to_string status)^")"
  | TypeDecl(typdecl) -> 
      "TypeDecl("^(typ_declaration_to_string typdecl)^")"
  | UseDecl(use_clause) -> "UseDecl("
      ^(list_to_string use_clause name_to_string "," false)^")"
  | SpecDecl(spec) -> "SpecDecl("
      ^(spec_to_string spec)^")"
  | NumberDecl(idents, exp, v) ->
      "NumberDecl("^(list_to_string idents (fun x-> x) "," true)
      ^", "^(exp_to_string exp)
      ^", "^(option_to_string v value_to_string)^")"
  | SubtypDecl(ident, subtyp_ind) ->
      "SubtypDecl("^ident^", "
      ^(subtyp_indication_to_string subtyp_ind)^")"
     

and declarative_item_to_string decl_item = match decl_item with
  | BasicDecl(basic_declaration) -> "BasicDecl("
      ^(basic_declaration_to_string basic_declaration)^")"

  | BodyDecl(body) -> 
      "BodyDecl("^(body_to_string body)^")"
 
and subprog_spec_to_string spec = match spec with
  | Function(name,param_list,return_type) ->
      "Function("^(name_to_string name)^", "
	^(param_list_to_string param_list)^", "
	^(subtyp_to_string return_type)^")"
  | Procedure(name,param_list) ->
      "Procedure("^(name_to_string name)^", "
      ^(param_list_to_string param_list)^")"

and sub_program_body_to_string 
    (subprog_decl, declarative_part, instr_list) = 
  "("^(subprog_spec_to_string subprog_decl)
  ^",\n\n"
  ^(list_to_string declarative_part
      (fun (item,loc) -> "("^
	 (declarative_item_to_string item)^", "
	 ^(line_of_loc loc)^")")
	  ";\n" true)
  ^",\n\n"
  ^instr_list_to_string instr_list
  ^")"

and package_spec_to_string (name, decls) =
  "("^(name_to_string name)
  ^(list_to_string
      decls
      (fun (decl, loc) -> 
	 "("^(basic_declaration_to_string decl)^", "
	 ^(line_of_loc loc)^")")
      ";\n" 
      true)^")"

and package_body_to_string 
    (name, package_spec, declarative_part, instr_list) =
  "("^(name_to_string name)^",\n\n"
  ^(option_to_string package_spec package_spec_to_string)^",\n"
  ^(list_to_string declarative_part
      (fun (item,loc) -> "("^
	 (declarative_item_to_string item)^", "
	 ^(line_of_loc loc)^")")
      ";\n" true)
  ^",\n\n"
  ^instr_list_to_string instr_list
  ^")"

and spec_to_string spec = match spec with
  | SubProgramSpec(subprogspec) -> "SubProgramSpec("
      ^(subprog_spec_to_string subprogspec)^")"
  | PackageSpec(packagespec) -> "PackageSpec("
      ^(package_spec_to_string packagespec)^")"

and body_to_string body = match body with
  | SubProgramBody(subprog_body) ->
      "SubProgramBody("^(sub_program_body_to_string subprog_body)
      ^")"
  | PackageBody(package_body) ->
      "PackageBody("^(package_body_to_string package_body)
      ^")"

let library_item_to_string lib_item = match lib_item with
  | Spec(spec) -> "Spec("^(spec_to_string spec)^")"
  | Body(body) -> "Body("^(body_to_string body)^")"

let compil_unit_to_string (context,lib_item,loc) = 
  "("^(context_to_string context)
  ^",\n\n"^(library_item_to_string lib_item)
  ^",\n\n"^(line_of_loc loc)^")\n"

      


(* changer avec print_list *)
let rec ast_to_string programme = match programme with
  | compil_unit::r -> (compil_unit_to_string compil_unit)^
      (ast_to_string r)
  | [] -> ""


let print_ast programme = print_string (ast_to_string programme)
