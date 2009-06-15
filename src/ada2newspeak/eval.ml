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

let mk_float(f:float):float_number = (f, string_of_float f)

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


(** Evaluate (at compile-time) an expression. *)
let eval_static (exp:Ast.expression) (expected_typ:typ option)
                (csttbl:(name,constant_symb) Hashtbl.t)
                (package:package_manager)
                (extern:bool)
   :(value*typ) =

  let find_all_cst nom = Hashtbl.find_all csttbl nom in

  let find_all_use ident =
    List.flatten
      (List.map
         (fun pack -> find_all_cst (pack, ident))
         package#get_use) in

  let rec eval_static_exp (exp,_:Ast.expression) (expected_typ:typ option)
          :(Syntax_ada.value*Syntax_ada.typ) =
    match exp with
    | Ast.CInt   (i)  ->IntVal(i),           check_typ expected_typ IntegerConst
    | Ast.CFloat (f,s)->FloatVal(f,s),       check_typ expected_typ Float
    | Ast.CChar  (c)  ->IntVal(Nat.of_int c),check_typ expected_typ Character
    | Ast.CBool  (b)  ->BoolVal(b),          check_typ expected_typ Boolean
    | Ast.Var(v) -> eval_static_const (package#normalize_name v extern)
                                      expected_typ
    | Ast.FunctionCall _  -> raise NonStaticExpression
    | Ast.Unary (op,exp)   -> eval_static_unop  op  exp  expected_typ
    | Ast.Binary(op,e1,e2) -> eval_static_binop op e1 e2 expected_typ
    | Ast.CondExp(cond,iftrue,iffalse) ->
        begin
            match eval_static_exp cond (Some Boolean) with
            | BoolVal true , Boolean -> eval_static_exp iftrue  expected_typ
            | BoolVal false, Boolean -> eval_static_exp iffalse expected_typ
            | _ -> Npkcontext.report_error "eval_static.exp"
                   "unexpected type for conditional expression"
        end

    | Ast.Qualified(subtyp, exp) ->
        let typ = check_typ expected_typ
          (base_typ subtyp) in
        let (value,typ) = eval_static_exp exp (Some(typ)) in
          check_static_subtyp subtyp value;
          (value, typ)

    | Ast.Attribute  (_(*name*), AttributeDesignator (id, _(*param*))) ->
          match id with
          | "first" | "last" | "length" ->
                      Npkcontext.report_error "eval_static.exp"
                                      "First, last, length not implemented"
          | _ ->      Npkcontext.report_error "eval_static.exp"
                                          ("unknown attribute " ^ id)

  (**
   * Evaluate statically a binary expression.
   * expected_typ is the expected type of the result, not the operands.
   *)
  and eval_static_binop (op:Ast.binary_op) (e1:Ast.expression)
                        (e2:Ast.expression) (expected_typ:typ option)
        :value*typ =
    let expected_typ1 = typ_operand op expected_typ in
    let (val1, val2, typ) =
      try
        let (val1, typ1) = eval_static_exp e1 expected_typ1 in
          match op with
            | Ast.Power ->
                let (val2, _) = eval_static_exp e2 (Some(Integer))
                in (val1, val2, typ1)
            | _ ->
                let (val2, typ2) = eval_static_exp e2 (Some(typ1))
                in (val1, val2, typ2)
      with AmbiguousTypeException ->
            try
              let (val2, typ2) = eval_static_exp e2 expected_typ1 in
              let (val1, typ1) = eval_static_exp e1 (Some(typ2))  in
              (val1, val2, typ1)
            with AmbiguousTypeException ->
                  Npkcontext.report_error "eval_static.binop"
                                          "ambiguous operands"
    in
    check_operand_typ op typ;
    match (op,val1,val2) with
      (* operations sur entiers ou flottants *)
    | Ast.Plus , IntVal   v1  , IntVal   v2   -> IntVal  (Nat.add v1 v2),    typ
    | Ast.Minus, IntVal   v1  , IntVal   v2   -> IntVal  (Nat.sub v1 v2),    typ
    | Ast.Mult , IntVal   v1  , IntVal   v2   -> IntVal  (Nat.mul v1 v2),    typ
    | Ast.Div  , IntVal   v1  , IntVal   v2   -> IntVal  (Nat.div v1 v2),    typ
    | Ast.Power, IntVal   v1  , IntVal   v2   -> IntVal  (puiss   v1 v2),    typ
    | Ast.Plus , FloatVal(a,_), FloatVal(b,_) -> FloatVal(mk_float(a +. b)), typ
    | Ast.Minus, FloatVal(a,_), FloatVal(b,_) -> FloatVal(mk_float(a -. b)), typ
    | Ast.Mult , FloatVal(a,_), FloatVal(b,_) -> FloatVal(mk_float(a *. b)), typ
    | Ast.Div  , FloatVal(a,_), FloatVal(b,_) -> FloatVal(mk_float(a /. b)), typ
    | Ast.Power, FloatVal(v1,_), IntVal(v2) ->
              FloatVal(mk_float (v1 ** (float_of_int (Nat.to_int v2)))), typ

    (*operations sur les entiers*)
    | (Ast.Rem, IntVal v1, IntVal v2) -> (IntVal(rem_ada v1 v2), typ)
    | (Ast.Mod, IntVal v1, IntVal v2) -> (IntVal(mod_ada v1 v2), typ)

    (* comparaisons *)
    | Ast.Eq,  v1, v2 -> (BoolVal( eq_val v1 v2), Boolean)
    | Ast.Gt,  v1, v2 -> (BoolVal(inf_val v2 v1), Boolean)

    (* operations sur les booleens *)
    | Ast.And,BoolVal b1,BoolVal b2 -> BoolVal(b1 && b2), Boolean
    | Ast.Or ,BoolVal b1,BoolVal b2 -> BoolVal(b1 || b2), Boolean
    | _ -> Npkcontext.report_error "eval_static.binop"
                                  "invalid operator and argument"

  (**
   * Evaluate statically the "- E" expression.
   *)
  and eval_static_uminus (exp:Ast.expression) :value*typ =
      match (eval_static_exp exp expected_typ) with
        | IntVal i, t when (integer_class t) -> (IntVal(Nat.neg i), t)
        | (FloatVal(f,_), Float) -> (FloatVal(mk_float (-.f)), Float)
        | _ -> Npkcontext.report_error "eval_static.uminus"
                                   "invalid operator and argument"

  (**
   * Evaluate statically the "abs E" expression.
   *)
  and eval_static_abs (exp:Ast.expression) :value*typ =
      match (eval_static_exp exp expected_typ) with
        | IntVal i, t when (integer_class t) ->
            let abs = if (Nat.compare i Nat.zero)<0 then Nat.neg i else i
            in (IntVal(abs), t)
        | (FloatVal(f,_), Float) -> (FloatVal(mk_float (abs_float f)), Float)
        | _ -> Npkcontext.report_error "eval_static.abs"
                                   "invalid operator and argument"

  (**
   * Evaluate statically the "op E" expressions.
   *)
  and eval_static_unop (op:Ast.unary_op) (exp:Ast.expression)
      (expected_typ:typ option) :value*typ =
  match (op, expected_typ) with
    | Ast.UPlus, Some t when integer_class t -> eval_static_exp exp expected_typ
    | Ast.UPlus, Some Float                  -> eval_static_exp exp expected_typ
    | Ast.UPlus, None ->
                    let (tr_exp, typ) = eval_static_exp exp expected_typ in
                    (match typ with
                       | Float                  -> tr_exp, typ
                       | t when integer_class t -> tr_exp, typ
                       | _ -> Npkcontext.report_error "eval_static.unop"
                             "Unexpected unary operator and argument"
                    )

    | Ast.UMinus, None
    | Ast.UMinus, Some Float                  -> eval_static_uminus exp
    | Ast.UMinus, Some t when integer_class t -> eval_static_uminus exp
    | Ast.Abs,    None
    | Ast.Abs,    Some Float                  -> eval_static_abs exp
    | Ast.Abs,    Some t when integer_class t -> eval_static_abs exp
    | Ast.Not,    None
    | Ast.Not,    Some Boolean ->
            (match (eval_static_exp exp expected_typ) with
               | BoolVal(b), Boolean -> BoolVal(not b), Boolean
               | _ -> Npkcontext.report_error "eval_static_unop"
                                    "Unexpected unary operator and argument"
            )
    | _ ->  Npkcontext.report_error
        "eval_static.unop"
          "Unexpected unary operator and argument"

  and eval_static_const (name:name) (expected_typ:typ option) :value*typ =

    (********** mem_other_cst **********)
    let mem_other_cst (list_cst:constant_symb list) ?(filter=(fun _ -> true))
                           (use:string option) (var_masque:bool)
            :bool =
         let is_it_ok cst = begin match cst with
           | (Number _|StaticConst _|VarSymb _) -> if (var_masque) then false
             else Npkcontext.report_error "eval_static.mem_other_cst"
                      ((name_to_string name)^" is not visible : "
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

    let sans_selecteur (ident:string) (name:name) :value*typ =
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
                  Npkcontext.report_error "eval_static.find_use"
                                            (ident^" is not visible : " ^
                                        "multiple use clauses cause hiding")
              | EnumLitteral(typ,v,_)::r
                        when known_compatible_typ expected_typ typ ->
                  if (mem_other_cst r
                                    ~filter:(known_compatible_typ expected_typ)
                                    None
                                    var_masque
                                    )
                  then (Npkcontext.report_error "eval_static.find_use"
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
                      "eval_static.find_use"
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
                     Npkcontext.report_error "eval_static.find_use"
                                          "uncompatible types"
                    else Npkcontext.report_error "eval_static.find_use"
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
                  "eval_static.avec_selecteur"
                  "uncompatible types" in
      let list_symb = find_all_cst name in
      match list_symb with
            | Number(IntVal    i,_)::_ ->
                        IntVal i, check_typ expected_typ IntegerConst
            | Number(FloatVal(f),_)::_ ->
                        FloatVal f, check_typ expected_typ Float
            | StaticConst(v, typ, _)::_ -> v, check_typ expected_typ typ
            | VarSymb _::_ -> raise NonStaticExpression
            | [] -> Npkcontext.report_error "eval_static.avec_selecteur"
                                  ("cannot find symbol "^(name_to_string name))
            | _ -> find_enum list_symb

        (********* avec_selecteur_courant) *********)
  and avec_selecteur_courant (ident:name) (name:name) :value*typ =

        (*********  --> find_global ) *********)
      let rec find_global (list_symb:constant_symb list) :value*typ =
          match list_symb with
            | [] -> Npkcontext.report_error "eval_static.find_global"
                                   ("cannot find symbol "^(name_to_string name))
            | Number(IntVal i,true)::_ ->
                  IntVal i, check_typ expected_typ IntegerConst
            | Number(FloatVal(f),true)::_ ->
                  FloatVal f, check_typ expected_typ Float
            | Number(BoolVal _, _)::_ -> Npkcontext.report_error
                                      "eval_static.find_global"
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
        | (pack, _) -> Npkcontext.report_error "eval_static.const"
              ("unknown package " ^(Ada_utils.ident_list_to_string pack))
  in
      eval_static_exp exp expected_typ

(**
 * Evaluate statically an integer expression. FIXME
 *)
let eval_static_integer_exp (exp:Ast.expression)
                            (csttbl:(name, constant_symb) Hashtbl.t)
                            (package:package_manager)
                            (extern:bool)
    :nat =
    try
        let (v,_) =
          eval_static
              exp (Some IntegerConst)
              csttbl
              package
              extern in
            match v with
              | FloatVal _
              | BoolVal  _ -> Npkcontext.report_error
                          "eval_static.integer_exp"
                          "expected static integer constant"
              | IntVal i -> i
    with
      | NonStaticExpression -> Npkcontext.report_error
                          "eval_static.integer_exp"
                          "expected static expression"
      | AmbiguousTypeException -> Npkcontext.report_error
                          "eval_static.integer_exp"
                          "uncaught ambiguous type exception"

(**
 * Evaluate statically an constant number.
 *)
let eval_static_number (exp:Ast.expression)
                       (csttbl:(name, constant_symb) Hashtbl.t)
                       (package:package_manager)
                       (extern:bool)
    :value =
     try
         let (v,_) = eval_static exp None
                                     csttbl
                                     package
                                     extern in
             match v with
               | BoolVal _ -> Npkcontext.report_error
                     "eval_static.integer_exp"
                     "expected static float or integer constant"
               | FloatVal _ | IntVal _ -> v
     with
       | NonStaticExpression -> Npkcontext.report_error
          "eval_static.integer_exp"
          "expected static expression"
       | AmbiguousTypeException -> Npkcontext.report_error
         "eval_static.integer_exp"
         "uncaught ambiguous type exception"

