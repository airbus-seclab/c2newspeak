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

let nat_to_string = Newspeak.Nat.to_string

let list_to_string = Ada_utils.list_to_string
let name_to_string = Ada_utils.name_to_string

let option_to_string a to_string = match a with
  | None -> "None"
  | Some(a') -> "Some("^(to_string a')^")"

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
  | UPlus  -> "U+"
  | UMinus -> "U-"
  | Abs    -> "abs"
  | Not    -> "not"

let bop_to_string op = match op with
  | Plus    -> "+"
  | Minus   -> "-"
  | Mult    -> "*"
  | Div     -> "/"
  | Power   -> "**"
  | Mod     -> "mod"
  | Rem     -> "rem"
  | Eq      -> "="
  | Neq     -> "/="
  | Le      -> "<="
  | Lt      -> "<"
  | Ge      -> ">="
  | Gt      -> ">"
  | And     -> "and"
  | Or      -> "or"
  | Xor     -> "xor"
  | AndThen -> "and then"
  | OrElse  -> "or else"

let rec typ_to_string typ = match typ with
  | Integer      -> "Integer"
  | IntegerConst -> "IntegerConst"
  | Float        -> "Float"
  | Boolean      -> "Boolean"
  | Character    -> "Character"
  | Declared(id,typ_decl,loc) -> "Declared("
      ^id^","
      ^(typ_declaration_to_string typ_decl)^","
      ^(line_of_loc loc)^")"

and typ_declaration_to_string typ_decl = match typ_decl with
  | Enum(val_list, taille) ->
      "Enum("
      ^(list_to_string val_list
          (fun (nom,id) -> "("^nom^","^id^")")
          "; " true)
      ^", "^(ikind_to_string taille)^")"
  | DerivedType(subtyp) ->
      "DerivedType("^(subtyp_indication_to_string subtyp)^")"
  | IntegerRange(contrainte, taille) ->
      "IntegerRange("
      ^(contrainte_to_string contrainte)
      ^", "^(option_to_string taille ikind_to_string)^")"
  | Array(array_def) ->
      "Array("^(array_definition_to_string array_def)^")"

and array_definition_to_string ar = 
         "index = " ^(subtyp_indication_to_string ar.array_index)
      ^", component = "^(subtyp_indication_to_string ar.array_component)
      ^", size = "^(option_to_string ar.array_size string_of_int)
      ^")"


and exp_to_string exp = match exp with
  | CInt(i)          -> "CInt("^(nat_to_string i)^")"
  | CFloat(_,s)      -> "CFloat("^s^")"
  | CBool(b)         -> "CBool("^(string_of_bool b)^")"
  | CChar(c)         -> "CChar("^(string_of_int c)^")"
  | Var(s)           -> "Var("^(name_to_string s)^")"
  | Unary(op,exp)    -> "("^(uop_to_string op)^" " ^(exp_to_string exp)^")"
  | Binary(op,e1,e2) -> "("^(exp_to_string e1)^" "
                           ^(bop_to_string op)^" "
                           ^(exp_to_string e2)^")"
  | Qualified(subtyp, exp) -> "Qualified("
      ^(subtyp_to_string subtyp)
      ^", "^(exp_to_string exp)^")"
  | FunctionCall(nom, params) -> "FunctionCall-orArray("
      ^(name_to_string nom)^", "
      ^(String.concat "," (List.map arg_to_string params))^")"
  | Attribute (styp,des) ->
              (subtyp_to_string styp)
            ^ "'"
            ^ (designator_to_string des)

and designator_to_string des = match des with
| AttributeDesignator (id, None)       -> id
| AttributeDesignator (id, Some param) -> id ^ "(" ^ (exp_to_string param) ^ ")"


and subtyp_to_string subtyp = match subtyp with
  | Unconstrained(typ) -> "Unconstrained("^(typ_to_string typ)^")"
  | SubtypName(name) -> "SubtypName("^(name_to_string name)^")"
  | Constrained(typ, contrainte, static) ->
      "Constrained("^(typ_to_string typ)^", "
      ^(contrainte_to_string contrainte)^", "
      ^(string_of_bool static)^")"

and subtyp_indication_to_string (subtyp_ref, contrainte, subtyp, _adatype) =
  "("^(subtyp_to_string subtyp_ref)^", "
  ^(option_to_string contrainte contrainte_to_string)^", "
  ^(option_to_string subtyp subtyp_to_string)^")"

and contrainte_to_string contrainte = match contrainte with
  | RangeConstraint(e1, e2) ->
      "RangeConstraint("^(exp_to_string e1)
      ^", "^(exp_to_string e2)^")"
  |  IntegerRangeConstraint(v1,v2) ->
       "IntegerRangeConstraint("^(Newspeak.string_of_bounds (v1,v2))^")"
  |  FloatRangeConstraint((_,s1),(_,s2)) ->
       "FloatRangeConstraint("^s1^", "^s2^")"

and value_to_string v = match v with
  | IntVal(i) -> "IntVal("^(nat_to_string i)^")"
  | FloatVal(_,s) -> "FloatVal("^s^")"
(*  | EnumVal(i) -> "EnumVal("^(string_of_int i)^")"*)
  | BoolVal(b) -> "BoolVal("^(string_of_bool b)^")"

and iteration_scheme_to_string scheme = match scheme with
  | NoScheme -> "NoScheme"
  | While(exp) -> "While("^(exp_to_string exp)^")"
  | For(iter, exp1, exp2, isrev) -> "For "^iter^" in "
                    ^(if isrev then "reverse " else "")
                    ^(exp_to_string exp1) ^ ".." ^(exp_to_string exp2)

and lval_to_string lv =
  match lv with
    | Lval name -> name_to_string name
    | ArrayAccess (lval, e) ->
        (lval_to_string lval )^"["^(exp_to_string e)^"]"

and block_to_string block =
  list_to_string block
    (fun (instr,loc) -> "("^
       (instr_to_string instr)^", "
       ^(line_of_loc loc)^")")
    ";\n" true

and instr_to_string instr = match instr with
  | NullInstr    -> "(null)"
  | ReturnSimple -> "Return"
  | Return(exp)  -> "Return("^(exp_to_string exp)^")"
  | Assign(lval,exp) -> "("^(lval_to_string lval)^" <- "^(exp_to_string exp)^")"
  | If(exp, instr_then, instr_else) ->
      "If("^(exp_to_string exp)^",\n"
      ^(block_to_string instr_then)^",\n"
      ^(block_to_string instr_else)^")"

  | Exit -> "Exit"
  | Loop(scheme, block) ->
      "Loop("^(iteration_scheme_to_string scheme)^",\n"
      ^(block_to_string block)^")"
  | ProcedureCall(nom, params) -> "ProcedureCall("
      ^(name_to_string nom)^", "
      ^(String.concat "," (List.map arg_to_string params))^")"
  | Case(e,choices,default) -> "Case(("^(exp_to_string e)^"), ["
    ^ (String.concat ", "
     (List.map (function e,block ->
                "when "^(exp_to_string e)^" => "^(block_to_string block))
                choices))
    ^"]"^(match default with
            | None -> ""
            | Some block -> "when others => "^(block_to_string block)
         )
  | Block (decl_part,blk) -> "Declare ("^declarative_part_to_string decl_part
                          ^") {"^(block_to_string blk)^"}"

and arg_to_string (arg:argument) :string =
    match arg with
      | None   , e -> exp_to_string e
      | Some id, e -> id^" => "^(exp_to_string e)

and param_to_string param =
   "{formal_name = "    ^(param.formal_name)
  ^"; mode = "         ^(mode_to_string param.mode)
  ^"; param_type = "   ^(subtyp_to_string param.param_type)
  ^"; default_value = "^(option_to_string param.default_value exp_to_string)^"}"

and param_list_to_string list =
  list_to_string list param_to_string ";\n" true

and object_state_to_string status = match status with
  | Variable -> "Variable"
  | Constant -> "Constant"
  | StaticVal(value) -> "StaticVal("^(value_to_string value)^")"

and array_aggregate_to_string agregat = match agregat with
  | NamedArrayAggregate(assoc_list) ->
      let assoc_element_to_string (ident, exp) =
        "("^ident^", "^(exp_to_string exp)^")"
      in
        list_to_string assoc_list
          assoc_element_to_string
          ";\n" true

and representation_clause_to_string clause = match clause with
  | EnumerationRepresentation(ident, agregat) ->
      "EnumerationRepresentation("^ident^", "
      ^(array_aggregate_to_string agregat)^")"
  | AttributeDefinitionClause(st, id, exp) ->
      "For "^(subtyp_to_string st)^"'"^id^" use "^(exp_to_string exp)

and context_clause_to_string context_clause =
  match context_clause with
    | With(name, loc, spec) -> "With("
        ^(name_to_string name)
        ^", "^(line_of_loc loc)^",\n"
        ^(option_to_string
            spec
            (fun (spec, loc) -> "("^(spec_to_string spec)
               ^", "^(line_of_loc loc)^")"))
        ^")"
    | UseContext(names) -> "UseContext("^(name_to_string names)^")"

and context_to_string context = list_to_string context
                                    context_clause_to_string ";\n" true

and basic_declaration_to_string basic_decl = match basic_decl with
  | ObjectDecl(idents,subtyp_ind,def,status) -> "ObjectDecl("
      ^(list_to_string idents (fun x-> x) "," true)
      ^", "^(subtyp_indication_to_string subtyp_ind)
      ^", "^(option_to_string def exp_to_string)
      ^", "^(object_state_to_string status)^")"
  | TypeDecl(id,typdecl) ->
      "TypeDecl("^id^","^(typ_declaration_to_string typdecl)^")"
  | UseDecl(use_clause) -> "UseDecl("^(name_to_string use_clause)^")"
  | SpecDecl(spec) -> "SpecDecl("
      ^(spec_to_string spec)^")"
  | NumberDecl(idents, exp, v) ->
      "NumberDecl("^idents
      ^", "^(exp_to_string exp)
      ^", "^(option_to_string v value_to_string)^")"
  | SubtypDecl(ident, subtyp_ind) ->
      "SubtypDecl("^ident^", "
      ^(subtyp_indication_to_string subtyp_ind)^")"
  | RepresentClause(clause) ->
      "RepresentClause("
      ^(representation_clause_to_string clause)^")"
  | RenamingDecl(n,n') -> "Renaming : "^name_to_string n^" renames "^name_to_string n'


and declarative_item_to_string decl_item = match decl_item with
  | BasicDecl(basic_declaration) -> "BasicDecl("
      ^(basic_declaration_to_string basic_declaration)^")"

  | BodyDecl(body) ->
      "BodyDecl("^(body_to_string body)^")"

and sub_program_spec_to_string spec = match spec with
  | Function(name,param_list,return_type) ->
      "Function("^(name_to_string name)^", "
        ^(param_list_to_string param_list)^", "
        ^(subtyp_to_string return_type)^")"
  | Procedure(name,param_list) ->
      "Procedure("^(name_to_string name)^", "
      ^(param_list_to_string param_list)^")"

and package_spec_to_string (name, decls) =
  "("^(name_to_string name)
  ^(list_to_string
      decls
      (fun (decl, loc) ->
         "("^(basic_declaration_to_string decl)^", "
         ^(line_of_loc loc)^")")
      ";\n"
      true)^")"

and declarative_part_to_string dp :string =
    (list_to_string dp
          (fun (item,loc) -> "("^
             (declarative_item_to_string item)^", "
             ^(line_of_loc loc)^")")
          ";\n" true)

and spec_to_string spec = match spec with
  | SubProgramSpec(subprogspec) -> "SubProgramSpec("
      ^(sub_program_spec_to_string subprogspec)^")"
  | PackageSpec(packagespec) -> "PackageSpec("
      ^(package_spec_to_string packagespec)^")"

and body_to_string body = match body with
  | SubProgramBody (subprog_decl, declarative_part, block) ->
      "SubProgramBody("^(sub_program_spec_to_string subprog_decl)       ^",\n\n"
                       ^declarative_part_to_string declarative_part ^",\n\n"
                       ^block_to_string block
                  ^")"
  | PackageBody (name, package_spec, declarative_part, block) ->
      "PackageBody("^(name_to_string name)^",\n\n"
                   ^(option_to_string package_spec package_spec_to_string)^",\n"
                   ^declarative_part_to_string declarative_part ^",\n\n"
                   ^block_to_string block
      ^")"

let library_item_to_string lib_item = match lib_item with
  | Spec(spec) -> "Spec("^(spec_to_string spec)^")"
  | Body(body) -> "Body("^(body_to_string body)^")"

let compil_unit_to_string (context,lib_item,loc) =
  "("^(context_to_string context)
  ^",\n\n"^(library_item_to_string lib_item)
  ^",\n\n"^(line_of_loc loc)^")\n"

let rec ast_to_string programme =
  list_to_string programme compil_unit_to_string "" false

let print_ast programme = print_string (ast_to_string programme)
