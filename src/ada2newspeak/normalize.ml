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

  Etienne Millon
  email: etienne.millon AT gmail . com

  Jasmine Duchon
  email : jasmine . duchon AT free . fr

*)

open Syntax_ada
open Ada_utils

module Nat = Newspeak.Nat

exception AmbiguousTypeException

(* FIXME Temporary tweak. *)
let get_legacy_definition id = match String.lowercase id with
| "integer"   -> Syntax_ada.Constrained(Syntax_ada.Integer
                                       ,Ada_config.integer_constraint
                                       ,true
                                       )
| "float"     -> Syntax_ada.Unconstrained Syntax_ada.Float
| "boolean"   -> Syntax_ada.Unconstrained Syntax_ada.Boolean
| "character" -> Syntax_ada.Unconstrained Syntax_ada.Character
| _ ->  failwith "legacy definition available only for int float bool or char"

let make_package (n:name) :package =
  (fst n)@[snd n]

let mk_float(f:float):float_number = (f, string_of_float f)

let temp =
    object
        val mutable count = 0

        (**
         * Create a new temporary variable identifier.
         * Multiple calls will yield "tmp_range0", "tmp_range1", and so on.
         *
         * /!\ Side-effects : calls will alter the state of [temp].
         *)
        method create :string =
            let res = "tmp_range"^(string_of_int count) in
            count <- count + 1;
            res
    end

(* variable booleenne :
   cas fonction : extern
   autres : global *)
(** Constant symbols *)
type constant_symb =
  | Number       of value*bool      (** bool = global? *)
  | StaticConst  of value*typ*bool  (** bool = global? *)
  | EnumLitteral of typ*nat*bool    (** bool = global? *)
  | VarSymb      of bool            (** bool = global? *)
  | FunSymb      of typ option*bool (** bool = extern? *)

let string_of_name = Ada_utils.name_to_string

(**
 * Try to find a body for a given specification.
 *
 * @param ~specification is the [basic_declaration] associated to the
 *                       specification, as found in a [package_spec].
 * @param ~bodylist is a list of possible bodies, as found
 *                  in a [declarative_part].
 * @return a boolean describing whether a body could be found.
 *)
let find_body_for_spec ~(specification:basic_declaration)
                       ~(bodylist:declarative_item list) :bool =
  (* Try to match a spec and a body *)
  let match_ok (s:basic_declaration) (b:declarative_item) = match (s,b) with
    | SpecDecl(SubProgramSpec sps),BodyDecl(SubProgramBody (spsb,_,_))->sps=spsb
    | ObjectDecl _ as x,BasicDecl (ObjectDecl _ as y) -> x = y
    | _ -> false
  in
  List.exists (function bd -> match_ok specification bd) bodylist

(** Return the name for a specification. *)
let name_of_spec (spec:basic_declaration) :string = match spec with
  | ObjectDecl (idl,_,_,_) -> ident_list_to_string idl
  | TypeDecl (Enum (i,_,_))
  | TypeDecl (DerivedType (i,_))
  | TypeDecl (IntegerRange (i,_,_))
  | TypeDecl (Array  (i,_))
  | TypeDecl (Record (i,_))
  | NumberDecl (i,_,_)
  | SubtypDecl (i,_) -> i
  | SpecDecl (SubProgramSpec (Function  (n,_,_)))
  | SpecDecl (SubProgramSpec (Procedure (n,_)))
  | SpecDecl (PackageSpec (n,_)) -> name_to_string n
  | UseDecl _ | RepresentClause _-> "<no name>"

let check_package_body_against_spec ~(body:declarative_part)
                                    ~(spec:package_spec) =
  let (pkgname,spec_and_loc) = spec in
  let       (_,body_and_loc) = body in
  let speclist = List.map fst spec_and_loc in
  let bodylist = List.map fst body_and_loc in
  (* Filter on specifications : only sp such as
   * filterspec sp = true will be checked.      *)
  let filterspec = function
    | NumberDecl _ | SpecDecl   _ -> true
    | ObjectDecl _ | TypeDecl _ | SubtypDecl _
    | RepresentClause _ | UseDecl _ -> false
  in
  List.iter (function sp ->
    if (filterspec sp) then
      begin
        if not (find_body_for_spec ~specification:sp ~bodylist)
        then Npkcontext.report_error "Ada_utils.check_package_body_against_spec"
          ("Body for package " ^(name_to_string pkgname)
          ^" does not match its specification : cannot find a body for \""
          ^(name_of_spec sp)^"\"")
      end
  ) speclist

let normalize_ident ident package extern =
  if extern then (package, ident)
            else (   []  , ident)

let normalize_name (name:name) (package:package_manager) extern =
  if (not extern) then name
    else let (parents,ident) = name in
         let pack = package#current in
           match parents with
             | []                         -> (pack, ident)
             | a when a=pack              -> (pack, ident)
             | a when (package#is_with a) -> (  a , ident)
             | _ -> Npkcontext.report_error "ada_normalize.normalize_name"
                   ("unknown package "^(Ada_utils.ident_list_to_string parents))

(** Evaluate (at compile-time) an expression. *)
let eval_static (exp:expression) (expected_typ:typ option)
                (csttbl:(name,constant_symb) Hashtbl.t)
                (context:identifier list list)
                (package:package_manager)
                (extern:bool)
   :(value*typ) =

  let find_all_cst nom = Hashtbl.find_all csttbl nom in

  let find_all_use ident =
    List.flatten
      (List.map
         (fun pack -> find_all_cst (pack, ident))
         context) in

  let rec eval_static_exp (exp:expression) (expected_typ:typ option)
          :(Syntax_ada.value*Syntax_ada.typ) =
    match exp with
      | CInt   (i)   -> IntVal(i),           check_typ expected_typ IntegerConst
      | CFloat (f,s) -> FloatVal(f,s),       check_typ expected_typ Float
      | CChar  (c)   -> IntVal(Nat.of_int c),check_typ expected_typ Character
      | CBool  (b)   -> BoolVal(b),          check_typ expected_typ Boolean
      | Var(v) -> eval_static_const (normalize_name v
                                                    package
                                                    extern) expected_typ
      | FunctionCall _  -> raise NonStaticExpression

      | NullExpr | CString _ -> Npkcontext.report_error
                                 "Ada_normalize.eval_static_exp"
                                       "not implemented"

      | Unary (op,exp)   -> eval_static_unop  op  exp  expected_typ
      | Binary(op,e1,e2) -> eval_static_binop op e1 e2 expected_typ

      | Qualified(subtyp, exp) ->
          let typ = check_typ expected_typ
            (base_typ subtyp) in
          let (value,typ) = eval_static_exp exp (Some(typ)) in
            check_static_subtyp subtyp value;
            (value, typ)

      | Attribute  (_(*name*), AttributeDesignator (id, _(*param*))) ->
            match id with
            | "first" | "last" | "length" ->
                        Npkcontext.report_error "Ada_normalize:attributes"
                                        "First, last, length not implemented"
            | _ ->      Npkcontext.report_error "Ada_normalize:attributes"
                                            ("unknown attribute " ^ id)

  (**
   * Evaluate statically a binary expression.
   * expected_typ is the expected type of the result, not the operands.
   *)
  and eval_static_binop (op:binary_op) (e1:expression) (e2:expression)
                                (expected_typ:typ option)
        :value*typ =
    let expected_typ1 = typ_operand op expected_typ in
    let (val1, val2, typ) =
      try
        let (val1, typ1) = eval_static_exp e1 expected_typ1 in
          match op with
            | Power ->
                let (val2, _) = eval_static_exp e2 (Some(Integer))
                in (val1, val2, typ1)
            | _ ->
                let (val2, typ2) = eval_static_exp e2 (Some(typ1))
                in (val1, val2, typ2)
      with
          AmbiguousTypeException ->
            try
              let (val2, typ2) =
                eval_static_exp e2 expected_typ1
              in let (val1, typ1) =
                  eval_static_exp e1 (Some(typ2))
              in (val1, val2, typ1)
            with
                AmbiguousTypeException ->
                  Npkcontext.report_error "Ada_normalize.eval_static_exp"
                    "ambiguous operands"
    in
      check_operand_typ op typ;
      match (op,val1,val2) with
          (* operations sur entiers ou flottants *)
        | Plus , IntVal   v1  , IntVal   v2   -> IntVal  (Nat.add v1 v2),    typ
        | Minus, IntVal   v1  , IntVal   v2   -> IntVal  (Nat.sub v1 v2),    typ
        | Mult , IntVal   v1  , IntVal   v2   -> IntVal  (Nat.mul v1 v2),    typ
        | Div  , IntVal   v1  , IntVal   v2   -> IntVal  (Nat.div v1 v2),    typ
        | Power, IntVal   v1  , IntVal   v2   -> IntVal  (puiss   v1 v2),    typ
        | Plus , FloatVal(a,_), FloatVal(b,_) -> FloatVal(mk_float(a +. b)), typ
        | Minus, FloatVal(a,_), FloatVal(b,_) -> FloatVal(mk_float(a -. b)), typ
        | Mult , FloatVal(a,_), FloatVal(b,_) -> FloatVal(mk_float(a *. b)), typ
        | Div  , FloatVal(a,_), FloatVal(b,_) -> FloatVal(mk_float(a /. b)), typ
        | Power, FloatVal(v1,_), IntVal(v2) ->
                  FloatVal(mk_float (v1 ** (float_of_int (Nat.to_int v2)))), typ

        (*operations sur les entiers*)
        | (Rem, IntVal v1, IntVal v2) -> (IntVal(rem_ada v1 v2), typ)
        | (Mod, IntVal v1, IntVal v2) -> (IntVal(mod_ada v1 v2), typ)

        (* comparaisons *)
        | Eq,  v1, v2 -> (BoolVal(      eq_val v1 v2 ), Boolean)
        | Neq, v1, v2 -> (BoolVal(not  (eq_val v1 v2)), Boolean)
        | Lt,  v1, v2 -> (BoolVal(     inf_val v1 v2 ), Boolean)
        | Le,  v1, v2 -> (BoolVal(not (inf_val v2 v1)), Boolean)
        | Ge,  v1, v2 -> (BoolVal(not (inf_val v1 v2)), Boolean)
        | Gt,  v1, v2 -> (BoolVal(     inf_val v2 v1 ), Boolean)

        (* operations sur les booleens *)
        | ((AndThen|And),BoolVal(b1),BoolVal(b2))->(BoolVal(b1 && b2), Boolean)
        | ((OrElse|Or),  BoolVal(b1),BoolVal(b2))->(BoolVal(b1 || b2), Boolean)
        | (Xor,          BoolVal(b1),BoolVal(b2))->(BoolVal(b1 <> b2), Boolean)

        (* operations sur les string *)
        | Concat,_,_ ->Npkcontext.report_error "Ada_normalize.eval_static_binop"
                                              "string error : not implemented"
        | _ -> Npkcontext.report_error "Ada_normalize.eval_static_binop"
                                    "invalid operator and argument"


  (**
   * Evaluate statically the "- E" expression.
   *)
  and eval_static_uminus (exp:expression) :value*typ =
      match (eval_static_exp exp expected_typ) with
        | IntVal i, t when (integer_class t) -> (IntVal(Nat.neg i), t)
        | (FloatVal(f,_), Float) -> (FloatVal(mk_float (-.f)), Float)
        | _ -> Npkcontext.report_error "Ada_normalize.eval_static_exp"
                                   "invalid operator and argument"

  (**
   * Evaluate statically the "abs E" expression.
   *)
  and eval_static_abs (exp:expression) :value*typ =
      match (eval_static_exp exp expected_typ) with
        | IntVal i, t when (integer_class t) ->
            let abs = if (Nat.compare i Nat.zero)<0 then Nat.neg i else i
            in (IntVal(abs), t)
        | (FloatVal(f,_), Float) -> (FloatVal(mk_float (abs_float f)), Float)
        | _ -> Npkcontext.report_error "Ada_normalize.eval_static_exp"
                                   "invalid operator and argument"

  (**
   * Evaluate statically the "op E" expressions.
   *)
  and eval_static_unop (op:unary_op) (exp:expression) (expected_typ:typ option)
      :value*typ =
      match (op, expected_typ) with
        | UPlus, Some t when integer_class t -> eval_static_exp exp expected_typ
        | UPlus, Some Float                  -> eval_static_exp exp expected_typ
        | UPlus, None ->
                        let (tr_exp, typ) = eval_static_exp exp expected_typ in
                        (match typ with
                           | Float                  -> tr_exp, typ
                           | t when integer_class t -> tr_exp, typ
                           | _ -> Npkcontext.report_error
                                 "Ada_normalize.eval_static_unop"
                                 "Unexpected unary operator and argument"
                        )

        | UMinus, None
        | UMinus, Some Float                  -> eval_static_uminus exp
        | UMinus, Some t when integer_class t -> eval_static_uminus exp
        | Abs,    None
        | Abs,    Some Float                  -> eval_static_abs exp
        | Abs,    Some t when integer_class t -> eval_static_abs exp
        | Not,    None
        | Not,    Some Boolean ->
                (match (eval_static_exp exp expected_typ) with
                   | BoolVal(b), Boolean -> BoolVal(not b), Boolean
                   | _ -> Npkcontext.report_error "eval_static_unop"
                                        "Unexpected unary operator and argument"
                )
        | _ ->  Npkcontext.report_error
            "Ada_normalize.eval_static_unop"
              "Unexpected unary operator and argument"



  and eval_static_const (name:name) (expected_typ:typ option) :value*typ =

    (********** mem_other_cst **********)
    let mem_other_cst (list_cst:constant_symb list) ?(filter=(fun _ -> true))
                           (use:identifier option) (var_masque:bool)
            :bool =
         let is_it_ok cst = begin match cst with
           | (Number _|StaticConst _|VarSymb _) -> if (var_masque) then false
             else Npkcontext.report_error "normalize.mem_other_cst"
                      ((string_of_name name)^" is not visible : "
                       ^"multiple use clauses cause hiding")
           (* un autre symbole existe ayant le bon type *)
           | (EnumLitteral(typ,_,_)|FunSymb(Some(typ),_))
               when (filter typ) -> true
           (* symbole d'enumeration ou de fonctions n'ayant
               pas le bon type *)
           | (EnumLitteral(_)|FunSymb(_)) -> false
         end in

         (List.exists is_it_ok list_cst)
           || (match use with
                 | None -> false
                 | Some id -> List.exists is_it_ok (find_all_use id)
           )
    in

    (********** sans_selecteur **********)

    let sans_selecteur (ident:identifier) (name:name) :value*typ =
      (* les variables masquees le sont par un symbole de fonction
         ou enum interne.
         var_possible indique si on peut avoir une variable :
         si on a rencontre un symbole d'enumeration ou de fonction,
         (qui ne convenait pas, sinon, on a appele mem_other symb)
         on declenche une erreur si on rencontre une variable *)

                (******* --> find_use *******)
      let rec find_use (list_cst:constant_symb list) (var_masque:bool)
                        (var_possible:bool) :value*typ =
            match list_cst with
              | (Number _|StaticConst _|VarSymb _)::r when var_masque ->
                                            find_use r var_masque var_possible
              | Number(IntVal(i),_)::[] when var_possible ->
                      IntVal i, check_typ expected_typ IntegerConst
              | Number(FloatVal(f),_)::[] when var_possible ->
                      FloatVal f, check_typ expected_typ Float
              | StaticConst(v, typ,_)::[] when var_possible ->
                  v, check_typ expected_typ typ
              | VarSymb _::[] when var_possible -> raise NonStaticExpression
              | (Number(_)|StaticConst(_)|VarSymb(_))::_ ->
                  Npkcontext.report_error "Ada_normalize.eval_static_cst"
                                            (ident^" is not visible : " ^
                                        "multiple use clauses cause hiding")
              | EnumLitteral(typ,v,_)::r
                        when known_compatible_typ expected_typ typ ->
                  if (mem_other_cst r
                                    ~filter:(known_compatible_typ expected_typ)
                                    None
                                    var_masque
                                    )
                  then (Npkcontext.report_error "Ada_normalize.eval_static_cst"
                      (ident^" is not visible : "
                       ^"multiple use clauses cause hiding"))
                  else (IntVal v, typ)

              | FunSymb(Some typ,_)::r
                  when known_compatible_typ expected_typ typ ->
                  if (mem_other_cst r
                                    ~filter:(known_compatible_typ expected_typ)
                                    None
                                    var_masque)
                  then (Npkcontext.report_error
                      "Ada_normalize.eval_static_cst"
                      (ident^" is not visible : "
                       ^"multiple use clauses cause hiding"))
                  else raise NonStaticExpression

              | EnumLitteral(typ,v,_)::r when expected_typ = None ->
                  if (mem_other_cst r None var_masque)
                  then raise AmbiguousTypeException
                  else (IntVal v, typ)

              | FunSymb(Some _,_)::r when expected_typ = None ->
                  if (mem_other_cst r None var_masque)
                  then raise AmbiguousTypeException
                  else raise NonStaticExpression

              | (EnumLitteral(_)|FunSymb(_))::r ->
                  find_use r var_masque false

              | []  -> if var_masque then (* variable masque : au moins
                             un symbol mais mauvais type *)
                     Npkcontext.report_error "Ada_normalize.eval_static_cst"
                                          "uncompatible types"
                    else Npkcontext.report_error "Ada_normalize.eval_static_cst"
                                              ("cannot find symbol "^ident)

                    (****** --> find_interne *****)
      and find_interne (list_cst:constant_symb list) (var_masque:bool)
            :value*typ =
          match list_cst with
            | Number(IntVal i,_)::_ when not var_masque->
                IntVal(i), check_typ expected_typ IntegerConst
            | Number(FloatVal(f),_)::_ when not var_masque ->
                FloatVal(f), check_typ expected_typ Float
            | StaticConst(v, typ, _)::_ when not var_masque ->
                v, check_typ expected_typ typ
            | VarSymb(_)::_ when not var_masque ->
                raise NonStaticExpression
            | EnumLitteral(typ, v, _)::_  when
                known_compatible_typ expected_typ typ -> IntVal v, typ
            | FunSymb(Some typ,_)::_  when
                known_compatible_typ expected_typ typ ->
                                    raise NonStaticExpression
            | EnumLitteral(typ, v, _)::r when expected_typ=None ->
                if (mem_other_cst r (Some ident) true)
                then raise AmbiguousTypeException
                else (IntVal(v), typ)
            | FunSymb(Some _,_)::r when expected_typ=None ->
                if (mem_other_cst r (Some ident) true)
                then raise AmbiguousTypeException
                else raise NonStaticExpression
            | (EnumLitteral _|FunSymb _)::r -> find_interne r true
            | (Number _|StaticConst _|VarSymb _)::r ->
                find_interne r var_masque
            | [] -> find_use (find_all_use ident) var_masque true

      in find_interne (find_all_cst name) false


        (********* avec_selecteur *********)
  and avec_selecteur (name:name):value*typ =

        (********* --> find_enum *********)
      (* les variables sont masquees *)
      let rec find_enum (list_cst:constant_symb list) :value*typ =
          match list_cst with
            | (Number _|StaticConst _|VarSymb _)::r -> find_enum r
            | EnumLitteral(typ,v,_)::_ when
                known_compatible_typ expected_typ typ ->
                IntVal v, typ
            | FunSymb(Some(typ), _)::_
                when known_compatible_typ expected_typ typ ->
                raise NonStaticExpression
            | EnumLitteral(typ,v,_)::r when expected_typ=None ->
                if mem_other_cst r None true
                then raise AmbiguousTypeException
                else IntVal v, typ
            | FunSymb(Some(_), _)::r when expected_typ=None ->
                if mem_other_cst r None true
                then raise AmbiguousTypeException
                else raise NonStaticExpression
            | (EnumLitteral _|FunSymb _)::r -> find_enum r
            | [] ->
                Npkcontext.report_error
                  "Ada_normalize.eval_static_cst"
                  "uncompatible types" in
      let list_symb = find_all_cst name in
      match list_symb with
            | Number(IntVal    i,_)::_ ->
                        IntVal i, check_typ expected_typ IntegerConst
            | Number(FloatVal(f),_)::_ ->
                        FloatVal f, check_typ expected_typ Float
            | StaticConst(v, typ, _)::_ -> v, check_typ expected_typ typ
            | VarSymb _::_ -> raise NonStaticExpression
            | [] -> Npkcontext.report_error "Ada_normalize.eval_static_cst"
                                  ("cannot find symbol "^(string_of_name name))
            | _ -> find_enum list_symb

        (********* avec_selecteur_courant) *********)
  and avec_selecteur_courant (ident:name) (name:name) :value*typ =

        (*********  --> find_global ) *********)
      let rec find_global (list_symb:constant_symb list) :value*typ =
          match list_symb with
            | [] -> Npkcontext.report_error "Ada_normalize.eval_static_cst"
                                   ("cannot find symbol "^(string_of_name name))
            | Number(IntVal i,true)::_ ->
                  IntVal i, check_typ expected_typ IntegerConst
            | Number(FloatVal(f),true)::_ ->
                  FloatVal f, check_typ expected_typ Float
            | Number(BoolVal _, _)::_ -> Npkcontext.report_error
                                      "Ada_normalize.eval_static_cst"
                                   "internal error : number cannot have EnumVal"
            | StaticConst(v, typ, true)::_ -> v, check_typ expected_typ typ
            | EnumLitteral(typ,v,true)::_
                when known_compatible_typ expected_typ typ ->
                IntVal v, typ
            | FunSymb(Some typ, false)::_
                when known_compatible_typ expected_typ typ ->
                raise NonStaticExpression
            | EnumLitteral(typ, v, true)::r when expected_typ=None ->
                if mem_other_cst r None false
                then raise AmbiguousTypeException
                else (IntVal(v), typ)
            | FunSymb(Some(_), false)::r when expected_typ=None ->
                if mem_other_cst r None false
                then raise AmbiguousTypeException
                else raise NonStaticExpression
            | VarSymb(true)::_ -> raise NonStaticExpression
            | (Number(_, false)
                | StaticConst(_,_,false)
                | EnumLitteral _
                | VarSymb false
                | FunSymb _)::r -> find_global r
      in find_global (find_all_cst ident)

  in
      match name with
        | [], ident -> sans_selecteur ident name
        | pack, ident when extern || package#is_with pack ->
                                                    avec_selecteur (pack,ident)
        | (pack, ident) when pack = package#current->
                                        avec_selecteur_courant ([],ident) name
        | (pack, _) -> Npkcontext.report_error "Ada_normalize.eval_static_cst"
              ("unknown package " ^(Ada_utils.ident_list_to_string pack))
  in
      eval_static_exp exp expected_typ

(**
 * Evaluate statically an integer expression. FIXME
 *)
let eval_static_integer_exp (exp:expression)
                            (csttbl:(name, constant_symb) Hashtbl.t)
                            (context:identifier list list)
                            (package:package_manager)
                            (extern:bool)
    :nat =
    try
        let (v,_) =
          eval_static
              exp (Some(IntegerConst))
              csttbl context
              package
              extern in
            match v with
              | FloatVal _
              | BoolVal  _ -> Npkcontext.report_error
                          "Ada_normalize.eval_static_integer_exp"
                          "expected static integer constant"
              | IntVal i -> i
    with
      | NonStaticExpression -> Npkcontext.report_error
                          "Ada_normalize.eval_static_integer_exp"
                          "expected static expression"
      | AmbiguousTypeException -> Npkcontext.report_error
                          "Ada_normalize.eval_static_integer_exp"
                          "uncaught ambiguous type exception"

(**
 * Evaluate statically an constant number.
 *)
let eval_static_number (exp:expression)
                       (csttbl:(name, constant_symb) Hashtbl.t)
                       (context:identifier list list)
                       (package:package_manager)
                       (extern:bool)
    :value =
     try
         let (v,_) = eval_static exp None
                                     csttbl context
                                     package
                                     extern in
             match v with
               | BoolVal _ ->
                   Npkcontext.report_error
                     "Ada_normalize.eval_static_integer_exp"
                     "expected static float or integer constant"
               | FloatVal _ | IntVal _ -> v
     with
       | NonStaticExpression ->
        Npkcontext.report_error
          "Ada_normalize.eval_static_integer_exp"
          "expected static expression"
       | AmbiguousTypeException ->
       Npkcontext.report_error
         "Ada_normalize.eval_static_integer_exp"
         "uncaught ambiguous type exception"

let extract_subprog_spec (ast:compilation_unit):compilation_unit =
    match ast with
      | (context, Body(SubProgramBody(spec,_,_)), loc) ->
        (context, Spec(SubProgramSpec(spec)),     loc)
      | (_, Spec _, _) -> Npkcontext.report_error
          "Ada_normalize.extract_subprog_spec"
        "body expected, specification found"
      | (_, Body(PackageBody _), _) -> Npkcontext.report_error
          "Ada_normalize.parse_specification"
        "subprogram body expected, package body found"

(* renvoie la specification correspondant a name,
   extrait eventuellement cette specification d'un corps
   de sous-programme, dans le cas ou aucun fichier de specification
   n'est fourni.*)
let rec parse_specification (name:name) :compilation_unit =
  (* tricherie : probleme avec sous-package *) (* FIXME *)
  let spec_name = (string_of_name name)^".ads" in
  let spec_ast =
    if Sys.file_exists spec_name
    then
      let res = File_parse.parse spec_name in
      if (!Npkcontext.verb_ast) then begin
        print_endline "Abstract Syntax Tree (extern)";
        print_endline "-----------------------------";
        Print_syntax_ada.print_ast [res];
        print_newline ();
      end;
      res
    else
      let body_name = (string_of_name name)^".adb" in
        extract_subprog_spec (File_parse.parse body_name)
  in
    match spec_ast with
      | (_, Spec(_), _) -> spec_ast
      | (_, Body(_), _) -> Npkcontext.report_error
          "normalize.parse_specification"
            "specification expected, body found"

(* renvoie la specification normalisee du package correspondant
   a name, les noms etant traites comme extern a la normalisation*)
and parse_extern_specification (name:name):spec*location =
  let spec_ast = parse_specification name
  in
    match (normalization spec_ast true) with
      | (_, Spec(spec), loc) -> (spec, loc)
      | (_, Body(_), _) -> Npkcontext.report_error
          "normalize.parse_extern_specification"
            "internal error : specification expected, body found"

(* renvoie la specification du package correspondant a name.
   cette specification est normalisee *)
and parse_package_specification (name:name):package_spec =
  match (parse_specification name) with
    | (_, Spec(PackageSpec(name, decls)),_) -> (name, decls)
    | (_, Spec(SubProgramSpec _),_) ->
                Npkcontext.report_error
                   "Ada_normalize.parse_package_specification"
                  ("package specification expected, "
                  ^"subprogram specification found")
    | (_, Body _, _) -> Npkcontext.report_error
           "normalize.parse_package_specification"
          "internal error : specification expected, body found"

(**
 * Iterates through the abstract syntax tree, performing miscellaneous tasks.
 *   - match type identifiers to their declaration (or raise en error)
 *   - look for specs (.ads files)
 *   - transforms functions and type names to "package.ident" in their
 *     declarations.
 *
 * TODO document extern
 *)
and normalization (compil_unit:compilation_unit) (extern:bool)
    :compilation_unit =
  let csttbl = Hashtbl.create 100

  and package=new package_manager
    in

  (** Wrapper between [types] (legacy, see below) and Ada_types.  *)
  let atypes =
    object(s)
      val tbl :Ada_types.table = Ada_types.create_table 100

      method private subtyp_to_atype (s:subtyp) :Ada_types.t =
        match s with
        | Constrained   _ -> failwith "Constr is not impl"
        | Unconstrained _ -> failwith "Unconstr not impl"
        | SubtypName    _ -> failwith "SubtypeName not impl"

      method add (n:name) (st:subtyp) :unit =
        Ada_types.add_type tbl (snd n) (s#subtyp_to_atype st);
        Ada_types.print_table tbl

      method remove (n:name) :unit =
        Ada_types.remove_type tbl (snd n)

      method mem (n:name) :bool =
        try
          ignore (Ada_types.find_type tbl (snd n));
          true
        with Not_found ->
          begin try
            ignore (Ada_types.builtin_type (snd n));
            true;
          with Not_found -> false
          end

      method find (n:name) :(subtyp*location*bool) =
        get_legacy_definition (snd n),Newspeak.unknown_loc,true

    end
  in

  (**
   * This object encapsulates the table of types. It is basically a Hashtbl.t
   * mapping a [name] to a triplet of :
   *   - a [subtyp]
   *   - a [location]
   *   - a [bool] which indicates whether the type is global or not
   *
   * /!\ Side-effects : all methods call the underlying ones in Hashtbl.t,
   *                    and thus can modify the state of the object.
   *)
  let types =
    object (s)
      val tbl = Hashtbl.create 100

      (** Add a new subtype, or raise an error in case of conflict. *)
      method add (n:Syntax_ada.name) (subtyp:subtyp)
      (location:location)   (global:bool)
      :unit =
        if s#mem n then begin
          match s#find n with
          | (_, _, glob) when global = glob ->
              Npkcontext.report_error
              "normalize.typ_normalization.types#add"
              ("conflict : "^(string_of_name n)
              ^" already declared")
          | _ -> ()
    end;
    Hashtbl.add tbl n (subtyp,location,global);

      (** Is this type known ? *)
      method mem (n:name) :bool =
        Hashtbl.mem tbl n
          || atypes#mem n

      (** Find the type definition. *)
      method find (n:name) :(subtyp*location*bool) =
        try Hashtbl.find tbl n
        with Not_found -> atypes#find n

      (** Find all the types matching. *)
      method find_all (n:name)
      :(subtyp*location*bool) list =
        Hashtbl.find_all tbl n

      (** Remove a subtype definition. *)
      method remove (x:name) :unit =
        atypes#remove x;
        Hashtbl.remove tbl x
    end
  in

  (* gestion de la table des constantes *)

  (* ajout d'un nombre ou d'une constante *)
  (* FIXME : variables also get there... *)
  (* FIXME erroneous pattern matching ? *)
  let add_cst (nom:name) (cst:constant_symb) (global:bool) :unit =
    (if Hashtbl.mem csttbl nom
     then
       match Hashtbl.find csttbl nom with
         | Number      (_,    glob)
         | StaticConst (_, _, glob)
         | VarSymb     (      glob)
         | EnumLitteral(_, _, glob) when global = glob ->
             Npkcontext.report_error
               "Ada_normalize.add_cst"
               ("conflict : "^(string_of_name nom)
                ^" already declared")
         | FunSymb(_,ext) when global && ext=extern ->
              Npkcontext.report_error
               "Ada_normalize.add_cst"
               ("conflict : "^(string_of_name nom)
                ^" already declared")
         | _ -> ());
    Hashtbl.add csttbl nom cst

  (* ajout d'un litteral d'enumeration *)
  and add_enum (nom:name) typ global value =
    if Hashtbl.mem csttbl nom then
         List.iter (function
              | Number(_, glob) | VarSymb(glob)
              | StaticConst(_, _, glob) when global = glob ->
                  Npkcontext.report_error
                    "Ada_normalize.add_enum"
                    ("conflict : "^(string_of_name nom)
                     ^" already declared")
              | EnumLitteral(t, _, glob)
                  when typ=t && global = glob ->
                  Npkcontext.report_error
                    "Ada_normalize.add_enum"
                    ("conflict : "^(string_of_name nom)
                     ^" already declared")
              | FunSymb(Some t,ext) when typ=t
                                      &&  global
                                      &&  ext=extern ->
                  Npkcontext.report_error
                    "Ada_normalize.add_enum"
                    ("conflict : "^(string_of_name nom)
                     ^" already declared")
              | _ -> ())
           (Hashtbl.find_all csttbl nom)
       ;
    Hashtbl.add csttbl nom (EnumLitteral(typ, value, global))

  (* ajout d'un symbole de fonction *)
  and add_function (nom:name) (typ:typ option) (ext:bool) =
    (if Hashtbl.mem csttbl nom then begin
         List.iter
           (function
              | Number(_, true) | VarSymb(true)
              | StaticConst(_, _, true) ->
                  Npkcontext.report_error
                    "Ada_normalize.add_function"
                    ("conflict : "^(string_of_name nom)
                     ^" already declared")
              | EnumLitteral(t, _, true)
                  when typ=Some t ->
                  Npkcontext.report_error
                    "Ada_normalize.add_function"
                    ("conflict : "^(string_of_name nom)
                     ^" already declared")

              (* on ignore le cas ou deux fonctions ont
                 le meme type, pour accepter cas spec+body *)
              | _ -> ())
           (Hashtbl.find_all csttbl nom)
       end);
    Hashtbl.add csttbl nom (FunSymb(typ,ext))

  and remove_cst (ident:name) :unit = Hashtbl.remove csttbl ident in

  let normalize_ident_cur ident =
    normalize_ident ident package#current extern

  in

  let find_all_use (ident:identifier)
        :(subtyp*location*bool) list =
    List.flatten
      (List.map
         (fun pack -> types#find_all (pack, ident))
         package#get_use)
  in

  let add_enum_litt symbs typ global extern =
    List.iter (fun (ident,v) -> add_enum (normalize_ident ident
                                                               package#current
                                                               extern)
                                         typ
                                         global
                                         v)
              symbs
  in
  let add_typ nom typdecl location ~global ~extern =
    let subtyp = match typdecl with
      | Array _
      | Record _ -> Unconstrained(Declared(typdecl, location))

      | Enum(_,symbs,_) ->
          let min = snd (List.hd symbs)
          and max = snd (List.nth symbs ((List.length symbs) -1)) in
          let contrainte = IntegerRangeConstraint(min, max)
          and typ = Declared(typdecl, location)
          in
            add_enum_litt symbs typ global extern;
            Constrained(typ, contrainte, true)

      | IntegerRange(_, contrainte, _) ->
          Constrained(Declared(typdecl, location), contrainte, true)
      | DerivedType(_, subtyp_ind) ->
          let subtyp = extract_subtyp subtyp_ind in
          let typ = base_typ subtyp in
            begin
              match typ with
                | Declared(Enum(_,symbs,_),_) ->
                    add_enum_litt symbs typ global extern
                | _ -> ()
            end;
            subtyp
    in
      types#add nom subtyp location global;

  and find_subtyp x =

    let sans_selecteur ident =
      if types#mem x then
        let (decl, loc, _) = types#find x in
          (decl,loc)
      else begin
        match find_all_use ident with
          | [(typ_decl, loc, _)] -> (typ_decl, loc)
          | [] -> Npkcontext.report_error
              "Ada_normalize.typ_normalization.find_subtyp"
                ("unknown identifier "^ident)
          | _::_ -> Npkcontext.report_error
              "Normalize.find_subtyp"
                (ident^" is not visible : "
                 ^"multiple use clauses cause hiding")
      end

    and avec_selecteur _ =
      try
        let (decl, loc, _) = types#find x in
          (decl,loc)
      with Not_found ->
        Npkcontext.report_error
          "Ada_normalize.normalization.find_typ.avec_selecteur"
          ("unknown identifier "^(string_of_name x))

    and selecteur_courant ident =
      begin try let (tdecl,loc,_) = List.find (fun (_,_,x) -> x)
                                        (types#find_all ident)
          in (tdecl,loc)
      with Not_found -> Npkcontext.report_error
        "Ada_normalize.normalization.find_typ.selecteur_courant"
          ("unknown identifier "^(string_of_name x))
      end

    in
        match x with
          | ([], ident) -> sans_selecteur ident
          | (pack, _) when extern||package#is_with pack -> avec_selecteur x
          | (pack, ident) when pack = package#current ->
              selecteur_courant ([],ident)
          | (pack, _) -> Npkcontext.report_error "Ada_normalize.find_typ"
                ("unknown package " ^(Ada_utils.ident_list_to_string pack))
  in

  let normalize_name name =
    let add_package (parents, ident) pack = match parents with
      | [] -> (pack, ident)
      | a when pack = a -> (pack, ident)
      | a when (package#is_with a) -> (a, ident)
      | _ -> Npkcontext.report_error
          "ada_normalize.normalize_name.add_package"
            ("unknown package "
             ^(Ada_utils.ident_list_to_string parents))

    in
      if extern then
        add_package name (package#current)
        (* pas de gestion de with dans package inclus *)
      else name
  in

let rec normalize_subtyp_indication (subtyp_ref, contrainte, subtyp) =
  (* on etablit le sous-type tel qu'il sera utilise dans
     le reste du code, a partir du type de base du sous-type
     de reference, de la contrainte normalisee, et d'un
     booleen qui indique si le sous-type de reference est
     statique *)
  let subtyp_of_constraint contrainte typ static_ref =
    let static_constraint = constraint_is_static contrainte
    in

    (* Dans le cas de contrainte statique, la contrainte
       du sous-type resultat reste la meme.
       Dans le cas d'un RangeCosntraint contenant des expressions
       on genere deux temporaires, qui permettront de se
       referer aux valeurs des bornes a l'instant de la
       declaration du sous-type.
       Ces temporaires sont declares et initialises dans
       firstpass.
    *)
    let contrainte_subtyp_result = match contrainte with
      | RangeConstraint(_, _) ->
          let min = normalize_ident_cur (temp#create)
          and max = normalize_ident_cur (temp#create) in
            RangeConstraint(Var min, Var max)
      | _ -> contrainte
    in
      (Constrained(typ, contrainte_subtyp_result,
                   static_ref && static_constraint))
  in (match subtyp with
        | None -> ()
        | Some(_) ->
            Npkcontext.report_error
              "Ada_normalize.normalize_subtyp_indication"
              "internal error : subtyp already provided");

    let norm_subtyp_ref = normalize_subtyp subtyp_ref in

    let (norm_subtyp, norm_contrainte) =
      match (contrainte, norm_subtyp_ref) with
        | (None, Unconstrained(typ)) ->
            (Unconstrained(typ), None)
        | (None, Constrained(typ, const, static)) ->
            (Constrained(typ, const, static), None)
        | (Some(const), Unconstrained(typ)) ->
            let norm_contrainte =
              normalize_contrainte const typ
            in
              (subtyp_of_constraint norm_contrainte typ true,
               Some(norm_contrainte))
        | (Some(const), Constrained(typ,const_ref,stat_ref)) ->
            let norm_contrainte =
              normalize_contrainte const typ
            in
              if not
                (constraint_is_constraint_compatible
                   const_ref norm_contrainte)
              then
                Npkcontext.report_error
                  "Ada_normalize.normalize_subtyp_indication"
                  "constraint error : uncompatible constraint";
              (subtyp_of_constraint norm_contrainte typ stat_ref,
               Some(norm_contrainte))
        | (_, SubtypName _ ) ->
            Npkcontext.report_error
              "Ada_normalize.normalize_subtyp_indication"
              "internal error : unexpected subtyp name"
    in
      (norm_subtyp_ref, norm_contrainte, Some(norm_subtyp))

and normalize_subtyp subtyp =
  let norm_typ typp =
    match typp with
        Declared (Array(id,ConstrainedArray(st1_ind, st2_ind, _)),x) ->
          let n_st1 =  normalize_subtyp_indication st1_ind in
          let n_st2 =  normalize_subtyp_indication st2_ind in

        let subtyp = extract_subtyp n_st1 in

        let contrainte = match subtyp with
          | Constrained(_,contrainte,_) -> contrainte
          | Unconstrained _ ->
              Npkcontext.report_error
                "Ada_normalize.normalize_typ_decl"
                "array error : no range provided"
          | SubtypName _ ->
              Npkcontext.report_error
                "Ada_normalize.normalize_typ_decl"
                "internal error : unexpected subtyp name"
        in

        let taille = match contrainte with
          | IntegerRangeConstraint(inf, sup) ->
              Some(Nat.to_int (Nat.add Nat.one (Nat.sub sup inf)))

          | FloatRangeConstraint _ ->
              Npkcontext.report_error
                "Ada_normalize.normalize_typ_decl"
                "array error : range isn't discret"
          | RangeConstraint _ ->
              Npkcontext.report_error
                "Ada_normalize.normalize_typ_decl"
                "array error : range isn't static"
        in
          Declared (Array(id,ConstrainedArray(n_st1, n_st2, taille)),
                    x )
      |  _ -> typp
  in
    match subtyp with (* For array norm_typ is used here*)
      | Unconstrained(typ) -> Unconstrained(norm_typ typ)
      | Constrained(typ,const,static) ->
          Constrained(norm_typ typ,const,static)
      | SubtypName(name) ->
          fst (find_subtyp (normalize_name name))

and arraytyp_to_contrainte (typ:subtyp) :contrainte option =
    match typ with
      | Unconstrained(Declared(Array(_,ConstrainedArray(
            (Constrained(_, contr, _), None,_),_,_)),_)) -> Some contr
      | Unconstrained(Declared(Array(_,ConstrainedArray((SubtypName _, None,_)
            ,_,_)),_)) -> arraytyp_to_contrainte (normalize_subtyp typ)
      | Unconstrained(Declared(Array(_,ConstrainedArray((_, c,_),_,_)),_)) -> c
      | SubtypName _ -> arraytyp_to_contrainte (normalize_subtyp typ)
      | Constrained ( _, contr, true) -> Some contr
      | _  -> Npkcontext.report_error "Ada_normalize Length contraint"
                                      "Length not implemented for type /= array"

(**
 * Normalize an actual argument.
 * The identifier does not have to be normalized (it is just a plain string),
 * but normalize the expression.
 *)
and normalize_arg (id,e:argument) :argument = id,normalize_exp e

(**
 * Normalize an expression.
 *)
and normalize_exp (exp:expression) :expression = match exp with
  | Qualified(subtyp, exp) -> Qualified(normalize_subtyp subtyp,
                                        normalize_exp exp)
  | NullExpr | CInt _ | CFloat _ | CBool _ | CChar _
  | CString _ | Var _  -> exp
  | Unary (uop, exp)    -> Unary(uop, normalize_exp exp)
  | Binary(bop, e1, e2) -> Binary(bop, normalize_exp e1,
                                  normalize_exp e2)
  | FunctionCall(nom, params) ->
      FunctionCall(nom, List.map normalize_arg params)

  | Attribute (subtype, AttributeDesignator(attr, _))-> match attr with
     | "first" -> begin

                    match arraytyp_to_contrainte subtype with
                       None -> Npkcontext.report_error
                         "Ada_normalize First contraint"
                         "constraint is not IntegerRange"

                     | Some(IntegerRangeConstraint(a, b)) ->
                         if (Nat.compare a b <=0)
                         then
                             CInt a
                         else
                           Npkcontext.report_error
                         "Ada_normalize First contraint"
                         "Zero length"


                     | _ ->  Npkcontext.report_error
                         "Normalize: in Array access"
                           "constraint is not IntegerRange"
                    end
     | "last" -> begin
                    match arraytyp_to_contrainte subtype with
                       None -> Npkcontext.report_error
                         "Ada_normalize Last contraint"
                         "constraint is not IntegerRange"

                     | Some(IntegerRangeConstraint(a, b)) ->
                         if (Nat.compare a b <=0)
                         then
                              CInt b
                         else
                           Npkcontext.report_error
                         "Ada_normalize Length contraint"
                         "Zero length"


                     | _ ->  Npkcontext.report_error
                         "Normalize: in Array access"
                           "constraint is not IntegerRange"
                    end

  | "length" ->
         (*    Array or Range type only for attributes Length *)
        begin
         match arraytyp_to_contrainte subtype with
             None -> Npkcontext.report_error
               " Ada_normalize Length contraint"
               "constraint is not IntegerRange"

           | Some(IntegerRangeConstraint(a, b)) ->
               if (Nat.compare a b <=0)
               then
             CInt (Nat.add (Nat.sub b a) Nat.one)
               else         Npkcontext.report_error
             "Ada_normalize Length contraint"
             "Zero length"

           | Some(RangeConstraint _) ->
               Npkcontext.report_error
             "Ada_normalize Length contraint"
             "Range Constraint fo Length"

           | _ ->  Npkcontext.report_error
               "Normalize: in Array access"
             "constraint is not IntegerRange"
        end
  | _ -> Npkcontext.report_error "normalize:attr"
                ("No such attribute : '" ^ attr ^ "'")



(* normalize la contrainte contrainte
   le type des bornes est typ
   static indique si
   on lance une erreur en cas de borne non-static
   et si on verifie l'ordre des bornes
   (autrement dit, on attend une contrainte statique non nulle
   en retour. uniquement utilise dans le cas entier)
*)


(**
 * Normalize a constraint.
 *)
and normalize_contrainte (contrainte:contrainte) (typ:typ) :contrainte =
  let eval_range exp1 exp2 =
    let norm_exp1 = normalize_exp exp1
    and norm_exp2 = normalize_exp exp2 in
      (* on essaye d'evaluer les bornes *)
      (try
         let (val1,_) = eval_static
           norm_exp1 (Some(typ)) csttbl package#get_use
           package extern
         and (val2,_) = eval_static
           norm_exp2 (Some(typ)) csttbl package#get_use
           package extern in
         let contrainte =  match (val1, val2) with
           | (FloatVal(f1),FloatVal(f2)) ->
               if f1<=f2
               then FloatRangeConstraint(f1, f2)
               else
                 Npkcontext.report_error
                   "Ada_normalize.normalize_contrainte"
                   "null range not accepted"

           | (IntVal(i1), IntVal(i2)) ->
               if (Nat.compare i1 i2)<=0
               then
                 IntegerRangeConstraint(i1, i2)
               else
                 Npkcontext.report_error
                   "Ada_normalize.normalize_contrainte"
                   "null range not accepted"

           | (BoolVal(b1), BoolVal(b2)) ->
               let i1 = nat_of_bool b1
               and i2 = nat_of_bool b2
               in
                 if b1 <= b2
                 then IntegerRangeConstraint(i1, i2)
                 else
                   Npkcontext.report_error
                     "Ada_normalize.normalize_contrainte"
                     "null range not accepted"

           | _ ->
               (* ce cas n'est pas cense se produire :
                  on a verifie que les deux bornes sont de meme
                  type.*)
               Npkcontext.report_error
                 "Ada_normalize.normalize_contrainte"
                 ("internal error : range error : expected static "
                  ^"float or integer constant")
         in contrainte
       with
         | NonStaticExpression ->
             Npkcontext.report_error
               "Ada_normalize.normalize_contrainte"
                 "non-static constraint are not yet supported"

         | AmbiguousTypeException ->
             Npkcontext.report_error
               "Ada_normalize.normalize_contrainte"
               "internal error : uncaught ambiguous type exception")
  in
    match contrainte with
      | RangeConstraint(exp1, exp2) ->
          eval_range exp1 exp2

      | IntegerRangeConstraint _
      | FloatRangeConstraint _ ->
          Npkcontext.report_error
            "Ada_normalize.eval_contrainte"
            "internal error : unexpected Numeric Range"

in
  let add_extern_typdecl typ_decl loc = match typ_decl with
    | DerivedType (ident, _)
    | Array       (ident, _)
    | Record      (ident, _)
    | IntegerRange(ident,_,_)
    | Enum        (ident, _, _) -> add_typ (normalize_ident ident
                                                            package#current
                                                            true)
                                           typ_decl
                                           loc
                                           ~extern:true
                                           ~global:true
  in

let interpret_enumeration_clause agregate assoc cloc loc =
    Npkcontext.set_loc cloc;
    let new_rep = match agregate with
      | NamedArrayAggregate(assoc_list) ->
          let rep_assoc =
            List.map
              (fun (ident, exp) ->
                 let v = eval_static_integer_exp exp csttbl package#get_use
                   package false
                 in (ident, v))
              assoc_list in
          let find_val ident =
            try  List.assoc ident rep_assoc
            with Not_found -> Npkcontext.report_error
                    "Ada_normalize.interpret_enumeration_clause"
                    ("missing representation for "^ident) in
          let make_new_assoc (l, last) (ident,_) =
            let v = find_val ident in
            let new_l = l@[(ident, v)]
            in match last with
                | None -> (new_l, Some(v))
                | Some(v0) when (Nat.compare v0 v) < 0 -> (new_l, Some(v))
                | Some _ ->
                    Npkcontext.report_error
                      "Ada_normalize.interpret_enumeration_clause"
                      "enumeration value not ordered" in
          let (new_assoc, max) =
            List.fold_left make_new_assoc ([], None) assoc in
          let max = match max with
            | None -> Npkcontext.report_error
                      "Ada_normalize.interpret_enumeration_clause"
                      "internal error : empty enumeration"
            | Some(max) -> max in
          let size = Ada_config.size_of_enum (snd (List.hd new_assoc)) max
          in (new_assoc, size)
    in
      Npkcontext.set_loc loc;
      new_rep

in
  let enumeration_representation ident symbs size represtbl loc =
    let (symbs, size) =
      (* this should be modified if we want to accept several
         kind of representation clause. *)
      if Hashtbl.mem represtbl ident then
        begin
          let clause = Hashtbl.find represtbl ident
          in match clause with
            | (EnumerationRepresentation(_, agregat), rloc) ->
                interpret_enumeration_clause agregat symbs rloc loc
            | AttributeDefinitionClause _,_ ->
                     Npkcontext.report_error "normalize"
                  "AttributeDefinitionClause is not yet implemented" (* FIXME *)

        end
      else
        (symbs, size)
    in Enum(ident, symbs, size)

  in

  let normalize_integer_range ident taille contrainte =
    match (taille, contrainte) with
      | (None, RangeConstraint(_)) ->
          begin
            try
              let norm_contrainte =
                normalize_contrainte contrainte IntegerConst
              in match norm_contrainte with
                | IntegerRangeConstraint(min, max) ->
                    let ikind = ikind_of_range min max
                    in IntegerRange(ident, norm_contrainte, Some(ikind))

                | _ ->
                    Npkcontext.report_error
                      "Ada_normalize.normalize_integer_range"
                      "internal error : uncompatible constraint type"
            with
                NonStaticExpression ->
                  Npkcontext.report_error
                    "Ada_normalize.normalize_integer_range"
                    "expected static expression"
          end
      | _ ->
          Npkcontext.report_error
            "Ada_normalize.normalize_integer_range"
            "internal error : size or constraint already provided"
  in

  (* this check is specific to Ada2Newspeak *)
  let check_represent_clause_order ident represtbl (_,decl_line,_) =
    let clauses = Hashtbl.find_all represtbl ident
    in
      List.iter
        (fun (_, (_,cl_line,_)) ->
           if cl_line>=decl_line
           then
             Npkcontext.report_error
               "Ada_normalize.check_represent_clause_order"
               ("a representation clause has been found for "
                ^ident^" after its first use"))
        clauses
  in

  let normalize_typ_decl typ_decl loc global represtbl = match typ_decl with
    | Enum(ident, symbs, size) ->
        let typ_decl = enumeration_representation ident symbs size represtbl loc
        in
          add_typ (normalize_ident_cur ident) typ_decl loc global extern;
          typ_decl
    | DerivedType(ident, subtyp_ind) ->
        let update_contrainte contrainte symbs new_assoc =
          let find_ident v = List.find (fun (_, v') -> v'=v) symbs
          and find_new_val (ident,_) = List.assoc ident new_assoc in
          let change_val v =
              find_new_val (find_ident v)
          in match contrainte with
            | IntegerRangeConstraint(v1, v2) ->
                IntegerRangeConstraint(change_val v1, change_val v2)
            | _ -> Npkcontext.report_error
                "Ada_normalize.normalize_typ_decl"
                  ("internal error :"
                   ^" constraint isnt integer range for enumeration type")
        in
        let norm_subtyp_ind = normalize_subtyp_indication subtyp_ind in
        let parent_type = extract_typ norm_subtyp_ind in
        let typ_decl = match parent_type with
            (* base type cases : we still have a derived type *)
          | Integer | Float | Boolean
          | Character -> DerivedType(ident, norm_subtyp_ind)
          | (String|IntegerConst) ->
              Npkcontext.report_error
                "Ada_normalize.normalize_typ_decl"
                "internal error : incorrect type"
          (* declared types : simplification *)
          | Declared(Enum(parent, symbs, size),_) ->
              check_represent_clause_order parent represtbl loc;
              enumeration_representation ident symbs size represtbl loc
          | Declared(IntegerRange(_,contrainte,taille),_) ->
              IntegerRange(ident, contrainte, taille)
          | Declared(Array (_, def),_) -> Array (ident, def)
          | Declared(Record(_, def),_) -> Record(ident, def)
          | Declared(DerivedType(_, subtyp_ind),_) ->
              DerivedType(ident, subtyp_ind) in


        (*constitution of the subtype representing the current declaration *)
        let norm_subtyp =
            match (extract_subtyp norm_subtyp_ind ) with
              | Unconstrained(_) ->
                  Unconstrained(Declared(typ_decl, loc))
              | Constrained(_, contrainte, static) ->
              (* for enumeration types, we update the value of the constraint *)
                  let contrainte = match (typ_decl, parent_type) with
                    | (Enum(_, new_assoc,_), Declared(Enum(_, symbs, _),_)) ->
                        update_contrainte contrainte symbs new_assoc
                    | _ -> contrainte
                  in Constrained(Declared(typ_decl, loc), contrainte, static)
              | SubtypName _ ->
                  Npkcontext.report_error
                    "Ada_normalize.normalize_typ_decl"
                    "internal error : unexpected subtyp name" in

        let new_subtyp_ind =
          let (subtyp, contrainte, _) = norm_subtyp_ind in
          (subtyp, contrainte, Some(norm_subtyp))
        in
        let norm_typ_decl = DerivedType(ident, new_subtyp_ind) in
          add_typ (normalize_ident_cur ident) norm_typ_decl loc global extern;
          norm_typ_decl
    | IntegerRange(ident,contrainte,taille) ->
        let decl = normalize_integer_range ident taille contrainte in
          add_typ (normalize_ident_cur ident) decl loc global extern;
          decl
    | Array(ident, ConstrainedArray(intervalle_discret, subtyp_ind , None)) ->
        let norm_inter =  normalize_subtyp_indication intervalle_discret
        and norm_subtyp_ind = normalize_subtyp_indication subtyp_ind in
        let subtyp = extract_subtyp norm_inter in

        let contrainte = match subtyp with
          | Constrained(_,contrainte,_) -> contrainte
          | Unconstrained _ ->
              Npkcontext.report_error
                "Ada_normalize.normalize_typ_decl"
                "array error : no range provided"
          | SubtypName _ ->
              Npkcontext.report_error
                "Ada_normalize.normalize_typ_decl"
                "internal error : unexpected subtyp name" in
        let taille = match contrainte with
          | IntegerRangeConstraint(inf, sup) ->
              Some(Nat.to_int (Nat.add Nat.one (Nat.sub sup inf)))
          | FloatRangeConstraint _ ->
              Npkcontext.report_error
                "Ada_normalize.normalize_typ_decl"
                "array error : range isn't discret"
          | RangeConstraint _ ->
              Npkcontext.report_error
                "Ada_normalize.normalize_typ_decl"
                "array error : range isn't static" in
        let norm_typ = Array(ident,
                             ConstrainedArray(norm_inter,
                                              norm_subtyp_ind,
                                              taille))
        in
          add_typ (normalize_ident_cur ident) norm_typ loc global extern;
          norm_typ

    | Array(_, ConstrainedArray(_, _, Some _)) ->
        Npkcontext.report_error
          "Ada_normalize.normalize_typ_decl"
          "internal error : size of array already provided"

    | Record (ident, fls) ->
        let norm_field (ids, sbtyp_ind, e_opt) =
          (ids, normalize_subtyp_indication sbtyp_ind, e_opt)
        in
        let  norm_typ =
          Record (ident, List.map norm_field fls)
        in
          add_typ (normalize_ident_cur ident) norm_typ loc global extern;
          norm_typ

  in

  let remove_typ_decl typ_decl = match typ_decl with
    | Enum(nom, symbs, _) -> types#remove (normalize_ident_cur nom);
        List.iter (fun (symb, _) -> remove_cst (normalize_ident_cur symb))
                  symbs
    | DerivedType  (nom,_)
    | IntegerRange (nom,_,_)
    | Array        (nom, _)
    | Record       (nom, _) -> types#remove (normalize_ident_cur nom)

  in

  let remove_decl_part (_,decl_part) =
    (* incomplet *)
    List.iter
      (function (item,_) -> match item with
      | BasicDecl(TypeDecl(typ_decl)) ->    remove_typ_decl typ_decl
      | BasicDecl(SubtypDecl(ident, _))->types#remove(normalize_ident_cur ident)
      | BasicDecl(ObjectDecl(ident_list,_, _, _)) ->
          List.iter
            (fun ident -> remove_cst (normalize_ident_cur ident))
            ident_list
      | BasicDecl(UseDecl(use_clause)) -> package#remove_use
                                                (make_package use_clause)
      | BasicDecl(NumberDecl(ident,_,_)) ->
            remove_cst (normalize_ident_cur ident)
      | BasicDecl(RepresentClause _)
      | BasicDecl(SpecDecl _)
      | BodyDecl _ -> ()
      )
    decl_part

  in

  let remove_params subprogram_decl =
    let params = match subprogram_decl with
      | Function(_,param_list,_) -> param_list
      | Procedure(_,param_list) -> param_list in
          List.iter
            (fun param -> remove_cst (normalize_ident_cur param.formal_name))
            params
  in

  let normalize_sub_program_spec subprog_spec ~addparam =
    let normalize_params param_list func =
      List.map
        (fun param ->
           if func && (param.mode <> In)
           then Npkcontext.report_error
              "Ada_normalize.normalize_sub_program_spec"
             ("invalid parameter mode : functions can only have"
             ^" \"in\" parameters");
           if addparam then
                add_cst (normalize_ident_cur param.formal_name)
                        (VarSymb(false))
                        false;
            {param with param_type = normalize_subtyp param.param_type}
        )
        param_list
    in match subprog_spec with
        | Function(name, [], return_type)  ->
            let norm_name = normalize_name name
            and norm_subtyp = normalize_subtyp return_type in
              add_function
                norm_name (Some(base_typ norm_subtyp)) false;
              Function(norm_name, [], norm_subtyp)
        | Function(name,param_list,return_type) ->
            let norm_name = normalize_name name in
              add_function norm_name None false;
              Function(norm_name,
                       normalize_params param_list true,
                       normalize_subtyp return_type)
        | Procedure(name,param_list) ->
            let norm_name = normalize_name name in
              add_function norm_name None false;
              Procedure(norm_name,
                        normalize_params param_list false)
  in

  let rec normalize_basic_decl item loc global reptbl = match item with
    | UseDecl(use_clause) -> package#add_use (make_package use_clause);
        item
    | ObjectDecl(ident_list,subtyp_ind,def, Variable) ->
        let norm_subtyp_ind =
          normalize_subtyp_indication subtyp_ind in
          (List.iter
             (fun x -> add_cst (normalize_ident_cur x)
                (VarSymb(global)) global)
             ident_list);
          ObjectDecl(ident_list, norm_subtyp_ind, def, Variable)
    | ObjectDecl(ident_list,subtyp_ind, Some(exp), Constant) ->
        (* constantes *)
        let norm_subtyp_ind =
          normalize_subtyp_indication subtyp_ind in

        let subtyp = extract_subtyp norm_subtyp_ind in
        let typ = base_typ subtyp in
        let add_ident v x = add_cst (normalize_ident_cur x)
          (StaticConst(v, typ, global)) global in
        let status =
          try
            let (v,_) = eval_static exp (Some(typ)) csttbl
              package#get_use package extern in

              (* on verifie que la valeur obtenue est conforme
                 au sous-type *)
              check_static_subtyp subtyp v;
              List.iter (add_ident v) ident_list;
              StaticVal(v)
          with
            | AmbiguousTypeException -> Npkcontext.report_error
                                        "Ada_normalize.normalize_basic_decl"
                                        "uncaught ambiguous type exception"
            | NonStaticExpression -> (List.iter
                                       (fun x -> add_cst (normalize_ident_cur x)
                                                         (VarSymb global)
                                                         global
                                       ) ident_list);
                    (*la constante n'est pas statique *) Constant

        in
          ObjectDecl(ident_list, norm_subtyp_ind, Some exp,status)
    | ObjectDecl(_) ->
        Npkcontext.report_error
          "Ada_normalize.normalize_basic_decl"
          ("internal error : constant without default value"
           ^"or already evaluated")
    | TypeDecl(typ_decl) ->
        let norm_typ_decl = normalize_typ_decl typ_decl loc global reptbl
        in TypeDecl(norm_typ_decl)
    | SpecDecl(spec) -> SpecDecl(normalize_spec spec)
    | NumberDecl(ident, exp, None) ->
        let norm_exp = normalize_exp exp in
        let v = eval_static_number norm_exp csttbl package#get_use
          package extern in
          (*ajouts dans la table*)
            add_cst (normalize_ident_cur ident)
                    (Number(v, global))
                    global;
          NumberDecl(ident, norm_exp, Some v)
    | NumberDecl(ident, exp, Some(v)) ->
        (* cas jamais emprunte *)
        NumberDecl(ident, normalize_exp exp, Some v)
    | SubtypDecl(ident, subtyp_ind) ->
        let norm_subtyp_ind = normalize_subtyp_indication subtyp_ind  in
        let subtyp = extract_subtyp norm_subtyp_ind in
          types#add (normalize_ident_cur ident) subtyp loc global;
          SubtypDecl(ident, norm_subtyp_ind)
    | RepresentClause _ -> item

  and normalize_package_spec (nom, list_decl) =
    package#set_current ((fst nom)@[snd nom]);
    let represtbl = Hashtbl.create 50 in
    let list_decl = List.filter (function
                                  | RepresentClause(rep), loc ->
                                      Hashtbl.add represtbl
                                        (extract_representation_clause_name rep)
                                        (rep, loc); false
                                  | _ -> true)
      list_decl in
    let rec normalize_decls decls =
      List.map (fun (decl, loc) ->
                  Npkcontext.set_loc loc;
                  let decl = normalize_basic_decl decl loc true represtbl
                  in (decl,loc)
               ) decls in
    let norm_spec = normalize_decls list_decl in
      package#reset_current;
      (nom,norm_spec)

  and normalize_spec spec = match spec with
    | SubProgramSpec(subprogr_spec) -> SubProgramSpec(
          normalize_sub_program_spec subprogr_spec ~addparam:false)
    | PackageSpec(package_spec) ->
        PackageSpec(normalize_package_spec package_spec)

  in

  let rec normalize_instr (instr,loc) =
    Npkcontext.set_loc loc;
    match instr with
      | NullInstr | ReturnSimple -> (instr, loc)
      | Assign(nom, exp) -> (Assign(nom, normalize_exp exp), loc)
      | Return(exp) -> (Return(normalize_exp exp), loc)
      | If(exp, instr_then, instr_else) ->
          (If(normalize_exp exp, normalize_block instr_then,
              normalize_block instr_else), loc)
      | Loop(NoScheme,instrs) -> (Loop(NoScheme, normalize_block instrs), loc)
      | Loop(While(exp), instrs) -> (Loop(While(normalize_exp exp),
                       normalize_block instrs), loc)
      | Loop(For(iter, exp1, exp2, is_rev), instrs) ->
                   (Loop(For(iter, exp1, exp2, is_rev),
                                   normalize_block instrs), loc)
      | Exit -> (Exit, loc)
      | ProcedureCall(nom, params) ->
         (ProcedureCall(nom, List.map normalize_arg params), loc)
      | Case (e, choices, default) ->
                Case (normalize_exp e,
                      List.map (function e,block->
                              normalize_exp e,
                              normalize_block block)
                          choices,
                      (match default with
                         | None -> None
                         | Some block -> Some(normalize_block block)
                      )),loc
      | Block (dp,blk) -> let ndp = normalize_decl_part dp ~global:false in
                          remove_decl_part dp;
                          Block (ndp, normalize_block blk), loc

  and normalize_block block =
    List.map normalize_instr block



  and normalize_decl_part (tbl,decl_part) ~global =
    let represtbl = Hashtbl.create 50 in
    let decl_part = List.filter (function
        | BasicDecl(RepresentClause(rep)), loc ->
          Hashtbl.add represtbl
            (extract_representation_clause_name rep)
            (rep, loc); false
        | _ -> true
      ) decl_part in
    let normalize_decl_items items =
      List.map (function
        | BasicDecl(SpecDecl (SubProgramSpec _) as sp),loc ->
            if (find_body_for_spec ~specification:sp
                                        ~bodylist:(List.map fst items)) then
            begin Npkcontext.set_loc loc;
            BasicDecl(normalize_basic_decl sp     loc
                                           global represtbl),loc
            end
            else Npkcontext.report_error "Normalize.normalize_decl_part"
                 ("Declaration of "^(name_of_spec sp)^" requires completion")
        | BasicDecl(basic),loc ->
            Npkcontext.set_loc loc;
            BasicDecl(normalize_basic_decl basic  loc
                                           global represtbl),loc
        | BodyDecl(body),loc ->
            Npkcontext.set_loc loc;
            BodyDecl(normalize_body body), loc
      ) items
    in tbl,normalize_decl_items decl_part

  and normalize_body body  = match body with
    | SubProgramBody(subprog_decl,decl_part,block) ->
        let norm_subprog_decl =
          normalize_sub_program_spec subprog_decl ~addparam:true
        and norm_decl_part = normalize_decl_part decl_part ~global:false in
        let norm_block = normalize_block block
        in
          remove_decl_part decl_part;
          remove_params subprog_decl;
          SubProgramBody(norm_subprog_decl,norm_decl_part, norm_block)
    | PackageBody(name, package_spec, decl_part, block) ->
        let norm_spec = normalize_package_spec
                           (with_default package_spec
                               (parse_package_specification name)
                           )
        in
          package#set_current ((fst name)@[snd name]);
          let ndp = normalize_decl_part decl_part ~global:true in
          remove_decl_part decl_part;
          check_package_body_against_spec ~body:ndp ~spec:norm_spec;
          package#reset_current;
          PackageBody(name, Some norm_spec, ndp, normalize_block block)

  in
  let normalize_lib_item lib_item loc =
    Npkcontext.set_loc loc;
    match lib_item with
      | Spec(spec) -> Spec(normalize_spec spec)
      | Body(body) -> Body(normalize_body body)

  in

  (* ajoute toutes les declarations contenues dans la
     spec, sans normalisation (puisque deja normalise).
     Ajoute egalement le nom du package
     a la liste de package accessible. *)
(* TODO *)
  let add_extern_spec spec =
    let add_extern_basic_decl (basic_decl, loc) =
      Npkcontext.set_loc loc;
      match basic_decl with
        | TypeDecl(typ_decl) ->
            add_extern_typdecl typ_decl loc
        | ObjectDecl(ident_list, _, _,
                     (Variable | Constant)) ->
            (List.iter
               (fun x -> add_cst (normalize_ident x package#current true)
                                 (VarSymb(true))
                                 true
               )
               ident_list
            )

        | ObjectDecl(ident_list,subtyp_ind, _, StaticVal(v)) ->
            (* constante statique *)

            let subtyp = extract_subtyp subtyp_ind in
            let typ = base_typ subtyp
              (*extract_subtyp subtyp_ind*) in
              List.iter
                (fun x -> add_cst (normalize_ident x package#current true)
                   (StaticConst(v, typ, true)) true)
                ident_list

        | NumberDecl(ident, _, Some v) ->
            (*ajouts dans la table*)
            add_cst (normalize_ident ident package#current true)
                    (Number(v, true))
                    true

        | NumberDecl(_, _, None) ->
            Npkcontext.report_error
              "Ada_normalize.add_extern_spec.add_extern_basic_decl"
              "internal error : external number declaration without value"
        | SpecDecl(SubProgramSpec
                     (Function(name, [], return_typ))) ->
            add_function name (Some(base_typ return_typ)) true
        | SpecDecl(SubProgramSpec(Function(name, _, _) |
                                      Procedure(name, _))) ->
            add_function name None true
        | SubtypDecl(ident, subtyp_ind) ->
            let subtyp = extract_subtyp subtyp_ind in
              types#add (normalize_ident ident package#current true)
                (*extract_subtyp subtyp_ind*) subtyp
                loc true
        | SpecDecl _ | UseDecl  _ | RepresentClause _ -> ()

    in match spec with
      | SubProgramSpec(Function(name, [], return_typ)) ->
          add_function name (Some(base_typ return_typ)) true
      | SubProgramSpec(Function(name, _, _)|Procedure(name, _)) ->
          add_function name None true

      | PackageSpec(nom, basic_decls) ->
          package#set_current ((fst nom)@[snd nom]);
          List.iter add_extern_basic_decl basic_decls;
          package#reset_current;
          package#add_with ((fst nom)@[snd nom])

  in

  (* normalise le context, en supprimant les doublons with *)
  let rec normalize_context context previous_with =
    match context with
      | With(nom, _, _)::r when (List.mem nom previous_with) ->
          (* doublon *)
          normalize_context r previous_with
      | With(nom, _, spec)::r ->

          let (norm_spec, loc) = match spec with
            | None   -> parse_extern_specification nom
            | Some _ -> Npkcontext.report_error
                "Ada_normalize.normalize_context"
                  "internal error : spec provided"
          in
            add_extern_spec norm_spec;
            With(nom, loc, Some(norm_spec, loc))
            ::normalize_context r (nom::previous_with)
      | UseContext(n)::r -> package#add_use (make_package n);
                            UseContext n::normalize_context r previous_with
      | [] -> []
  in

  let (context,lib_item,loc) = compil_unit in
  let norm_context = normalize_context context [] in
  let norm_lib_item = normalize_lib_item lib_item loc in
    Npkcontext.forget_loc ();
    (norm_context
    ,norm_lib_item
    ,loc)
