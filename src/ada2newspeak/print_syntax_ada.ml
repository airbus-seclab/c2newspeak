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

open AdaSyntax

(* Fonction transformant une liste [e1 e2 ... en ]
   en string de la forme
   [ (to_string e1) sep (to_string e2) sep ... sep (to_string en)]
   si crochet = true, sans [ ] sinon
 *)

let nat_to_string = Newspeak.Nat.to_string

let name_to_string = Ada_utils.name_to_string

let list_to_string l to_string sep crochet =
  let r = String.concat sep (List.map to_string l) in
  if crochet then "[" ^ r ^ "]" else r

let ident_list_to_string l =
  list_to_string l (fun x -> x) "." false

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

let rec typ_declaration_to_string typ_decl = match typ_decl with
  | Enum val_list ->
      "Enum("
      ^(list_to_string val_list
          (fun (nom,id) -> "("^nom^","^id^")")
          "; " true)
      ^")"
  | DerivedType(subtyp) ->
      "DerivedType("^(subtyp_indication_to_string subtyp)^")"
  | IntegerRange(min,max) ->
      "IntegerRange("
      ^exp_to_string min
      ^", "
      ^exp_to_string max
      ^")"
  | Record r -> "Record("^(list_to_string r record_component_to_string
                                          ", " false)
  | Array _ -> "Array(...)"
  | Access _ -> "Access(...)"
  | Digits _ -> "Digits(...)"

and record_component_to_string (c,st) =
  c ^ " => " ^ name_to_string st

and exp_to_string exp = match exp with
  | CInt   i          -> "CInt("^(nat_to_string i)^")"
  | CFloat s          -> "CFloat("^string_of_float s^")"
  | CChar  c          -> "CChar("^(string_of_int c)^")"
  | Lval   l          -> "LV "^lval_to_string l
  | Unary  (op,exp)   -> "("^(uop_to_string op)^" " ^(exp_to_string exp)^")"
  | Binary (op,e1,e2) -> "("^(exp_to_string e1)^" "
                           ^(bop_to_string op)^" "
                           ^(exp_to_string e2)^")"
  | Qualified(subtyp, exp) -> "Qualified("
      ^(lval_to_string subtyp)
      ^", "^(exp_to_string exp)^")"
  | Attribute (lv,des,arg) -> (lval_to_string lv) ^ "'" ^ des
                              ^ (match arg with None -> ""
                                             |  Some e ->
                                                 "("^exp_to_string e^")"
                                )
  | Aggregate _ -> "... aggregate ..."

and subtyp_indication_to_string (subtyp_ref, contrainte) =
  "("^(name_to_string subtyp_ref)^", "
  ^(option_to_string contrainte exp_pair_to_string)
  ^")"

and exp_pair_to_string (e1,e2) =
    "("
  ^ exp_to_string e1
  ^ ","
  ^ exp_to_string e2
  ^ ")"

and contrainte_to_string contrainte = match contrainte with
  |  IntegerRangeConstraint(v1,v2) ->
       "IntegerRangeConstraint("^(Newspeak.string_of_bounds (v1,v2))^")"
  |  FloatRangeConstraint(s1,s2) ->
       "FloatRangeConstraint("^string_of_float s1^", "^string_of_float s2^")"

and iteration_scheme_to_string scheme = match scheme with
  | NoScheme -> "NoScheme"
  | While(exp) -> "While("^(exp_to_string exp)^")"
  | For(iter, range, isrev) -> "For "^iter^" in "
                    ^(if isrev then "reverse " else "")
                    ^for_loop_range_to_string range

and for_loop_range_to_string = function
  | DirectRange (exp1, exp2) ->          (exp_to_string exp1)
                                ^ ".." ^ (exp_to_string exp2)
  | ArrayRange   n -> lval_to_string n ^ "'range"
  | SubtypeRange n -> lval_to_string n

and lval_to_string lv =
  match lv with
    | Var s -> s
    | SName (lv,s) -> (lval_to_string lv^"."^s)
    | ParExp (lval, e) ->
         (lval_to_string lval )
        ^"["
        ^(list_to_string e arg_to_string "," false)
        ^"]"
    | PtrDeref v -> lval_to_string v^".all"

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
  | LvalInstr(lv) -> "LvalInstr " ^(lval_to_string lv)
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
  ^"; param_type = "   ^(name_to_string param.param_type)
  ^"; default_value = "^(option_to_string param.default_value exp_to_string)^"}"

and param_list_to_string list =
  list_to_string list param_to_string ";\n" true

and object_state_to_string status = match status with
  | Variable -> "Variable"
  | Constant -> "Constant"

and array_aggregate_to_string assoc_list =
      let assoc_element_to_string (ident, exp) =
        "("^ident^", "^(exp_to_string exp)^")"
      in
        list_to_string assoc_list
          assoc_element_to_string
          ";\n" true

and representation_clause_to_string = function
  | EnumRepClause (agregat) -> array_aggregate_to_string agregat
  | SizeRepClause (sz)      -> "size = "^exp_to_string sz
  | RecordRepClause _       -> "record"

and context_clause_to_string context_clause =
  match context_clause with
    | With(name, spec) -> "With("
        ^name
        ^", "
        ^(option_to_string
            spec
            (fun (spec, loc) -> "("^(spec_to_string spec)
               ^", "^(line_of_loc loc)^")"))
        ^")"
    | UseContext(name) -> "UseContext("^name^")"

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
  | UseDecl(use_clause) -> "UseDecl("^(use_clause)^")"
  | SpecDecl(spec) -> "SpecDecl("
      ^(spec_to_string spec)^")"
  | NumberDecl(idents, exp) ->
      "NumberDecl("^idents
      ^", "^(exp_to_string exp)
      ^")"
  | SubtypDecl(ident, subtyp_ind) ->
      "SubtypDecl("^ident^", "
      ^(subtyp_indication_to_string subtyp_ind)^")"
  | RepresentClause(id,rc) ->
      "RepresentClause("^id^","
      ^(representation_clause_to_string rc)^")"
  | RenamingDecl(n, _, _, n') ->  "Renaming : "
                          ^n
                          ^" renames "
                          ^name_to_string n'
  | GenericInstanciation _ -> "generic instanciation"

and declarative_item_to_string decl_item = match decl_item with
  | BasicDecl(basic_declaration) -> "BasicDecl("
      ^(basic_declaration_to_string basic_declaration)^")"

  | BodyDecl(body) ->
      "BodyDecl("^(body_to_string body)^")"

and sub_program_spec_to_string spec = match spec with
  | Subprogram(name,param_list,Some return_type) ->
      "Function("^name^", "
        ^(param_list_to_string param_list)^", "
        ^(name_to_string return_type)^")"
  | Subprogram(name,param_list,None) ->
      "Procedure("^name^", "
      ^(param_list_to_string param_list)^")"

and package_spec_to_string (name, decls) =
  "("^name
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
  | SubprogramSpec(subprogspec) -> "SubProgramSpec("
      ^(sub_program_spec_to_string subprogspec)^")"
  | PackageSpec(packagespec) -> "PackageSpec("
      ^(package_spec_to_string packagespec)^")"

and body_to_string body = match body with
  | SubprogramBody (subprog_decl, declarative_part, block) ->
      "SubProgramBody("^(sub_program_spec_to_string subprog_decl)       ^",\n\n"
                       ^declarative_part_to_string declarative_part ^",\n\n"
                       ^block_to_string block
                  ^")"
  | PackageBody (name, package_spec, declarative_part) ->
      "PackageBody("^name^",\n\n"
                   ^(option_to_string package_spec package_spec_to_string)^",\n"
                   ^declarative_part_to_string declarative_part ^",\n\n"
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
