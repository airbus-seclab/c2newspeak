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
module  T  = Ada_types

(* variable booleenne :
   cas fonction : extern
   autres : global *)
(** Constant symbols *)
type constant_symb =
  | Number       of T.data_t*bool      (** bool = global? *)
  | StaticConst  of T.data_t*typ*bool  (** bool = global? *)
  | EnumLitteral of typ*nat*bool    (** bool = global? *)
  | VarSymb      of bool            (** bool = global? *)
  | FunSymb      of typ option*bool (** bool = extern? *)


(** Evaluate (at compile-time) an expression. *)
let eval_static (exp:Ast.expression) (expected_typ:typ option)
                (csttbl:(name,constant_symb) Hashtbl.t)
                (tbl:Symboltbl.SymStack.t)
                (extern:bool)
   :(T.data_t*typ) =

  let find_all_cst nom = Hashtbl.find_all csttbl nom in

  let find_all_use ident =
    List.flatten
      (List.map
         (fun pack -> find_all_cst (Some pack, ident))
         (Symboltbl.SymStack.s_get_use tbl)) in

  let rec eval_static_exp (exp,_:Ast.expression) (expected_typ:typ option)
          :(Ada_types.data_t*Syntax_ada.typ) =
    match exp with
    | Ast.CInt   i -> T.IntVal(i)            , check_typ expected_typ IntegerConst
    | Ast.CFloat f -> T.FloatVal(f)          , check_typ expected_typ Float
    | Ast.CChar  c -> T.IntVal (Nat.of_int c), check_typ expected_typ Character
    | Ast.CBool  b -> T.BoolVal b            , check_typ expected_typ Boolean
    | Ast.Var    v -> eval_static_const (Symboltbl.SymStack.normalize_name tbl v extern)
                                      expected_typ
    | Ast.FunctionCall _  -> raise NonStaticExpression
    | Ast.Unary (op,exp)   -> eval_static_unop  op  exp  expected_typ
    | Ast.Binary(op,e1,e2) -> eval_static_binop op e1 e2 expected_typ
    | Ast.CondExp(cond,iftrue,iffalse) ->
        begin
            match eval_static_exp cond (Some Boolean) with
            | T.BoolVal true , Boolean -> eval_static_exp iftrue  expected_typ
            | T.BoolVal false, Boolean -> eval_static_exp iffalse expected_typ
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
        :T.data_t*typ =
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
    | Ast.Plus , T.IntVal   a, T.IntVal   b -> T.IntVal (Nat.add a b), typ
    | Ast.Minus, T.IntVal   a, T.IntVal   b -> T.IntVal (Nat.sub a b), typ
    | Ast.Mult , T.IntVal   a, T.IntVal   b -> T.IntVal (Nat.mul a b), typ
    | Ast.Div  , T.IntVal   a, T.IntVal   b -> T.IntVal (Nat.div a b), typ
    | Ast.Power, T.IntVal   a, T.IntVal   b -> T.IntVal (puiss   a b), typ
    | Ast.Plus , T.FloatVal a, T.FloatVal b -> T.FloatVal  (a  +.  b), typ
    | Ast.Minus, T.FloatVal a, T.FloatVal b -> T.FloatVal  (a  -.  b), typ
    | Ast.Mult , T.FloatVal a, T.FloatVal b -> T.FloatVal  (a  *.  b), typ
    | Ast.Div  , T.FloatVal a, T.FloatVal b -> T.FloatVal  (a  /.  b), typ
    | Ast.Rem  , T.IntVal   a, T.IntVal   b -> T.IntVal (rem_ada a b), typ
    | Ast.Mod  , T.IntVal   a, T.IntVal   b -> T.IntVal (mod_ada a b), typ
    | Ast.Eq   ,            a,            b -> T.BoolVal(T.data_eq a b), Boolean
    | Ast.Gt   ,            a,            b -> T.BoolVal(T.data_lt b a), Boolean
    | Ast.And  , T.BoolVal  a, T.BoolVal  b -> T.BoolVal(a && b)     , Boolean
    | Ast.Or   , T.BoolVal  a, T.BoolVal  b -> T.BoolVal(a || b)     , Boolean
    | Ast.Power, T.FloatVal a, T.IntVal   b ->           
              T.FloatVal(a ** (float_of_int (Nat.to_int b))), typ
    | _ -> Npkcontext.report_error "eval_static.binop"
                                  "invalid operator and argument"

  (**
   * Evaluate statically the "- E" expression.
   *)
  and eval_static_uminus (exp:Ast.expression) :T.data_t*typ =
      match (eval_static_exp exp expected_typ) with
        | T.IntVal i, t when (integer_class t) -> (T.IntVal(Nat.neg i), t)
        | (T.FloatVal f , Float) -> (T.FloatVal(-.f), Float)
        | _ -> Npkcontext.report_error "eval_static.uminus"
                                   "invalid operator and argument"

  (**
   * Evaluate statically the "op E" expressions.
   *)
  and eval_static_unop (op:Ast.unary_op) (exp:Ast.expression)
      (expected_typ:typ option) :T.data_t*typ =
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
    | Ast.Not,    None
    | Ast.Not,    Some Boolean ->
            (match (eval_static_exp exp expected_typ) with
               | T.BoolVal(b), Boolean -> T.BoolVal(not b), Boolean
               | _ -> Npkcontext.report_error "eval_static_unop"
                                    "Unexpected unary operator and argument"
            )
    | _ ->  Npkcontext.report_error
        "eval_static.unop"
          "Unexpected unary operator and argument"

  and eval_static_const (name:name) (expected_typ:typ option) :T.data_t*typ =

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

    let sans_selecteur (ident:string) (name:name) :T.data_t*typ =
      (* les variables masquees le sont par un symbole de fonction
         ou enum interne.
         var_possible indique si on peut avoir une variable :
         si on a rencontre un symbole d'enumeration ou de fonction,
         (qui ne convenait pas, sinon, on a appele mem_other symb)
         on declenche une erreur si on rencontre une variable *)

                (******* --> find_use *******)
      let rec find_use (list_cst:constant_symb list) (var_masque:bool)
                        (var_possible:bool) :T.data_t*typ =
            match list_cst with
              | (Number _|StaticConst _|VarSymb _)::r when var_masque ->
                                            find_use r var_masque var_possible
              | Number(T.IntVal(i),_)::[] when var_possible ->
                      T.IntVal i, check_typ expected_typ IntegerConst
              | Number(T.FloatVal(f),_)::[] when var_possible ->
                      T.FloatVal f, check_typ expected_typ Float
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
                  else (T.IntVal v, typ)

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
                  else (T.IntVal v, typ)

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
            :T.data_t*typ =
          match list_cst with
            | Number(T.IntVal i,_)::_ when not var_masque->
                T.IntVal(i), check_typ expected_typ IntegerConst
            | Number(T.FloatVal(f),_)::_ when not var_masque ->
                T.FloatVal(f), check_typ expected_typ Float
            | StaticConst(v, typ, _)::_ when not var_masque ->
                v, check_typ expected_typ typ
            | VarSymb(_)::_ when not var_masque ->
                raise NonStaticExpression
            | EnumLitteral(typ, v, _)::_  when
                known_compatible_typ expected_typ typ -> T.IntVal v, typ
            | FunSymb(Some typ,_)::_  when
                known_compatible_typ expected_typ typ ->
                                    raise NonStaticExpression
            | EnumLitteral(typ, v, _)::r when expected_typ=None ->
                if (mem_other_cst r (Some ident) true)
                then raise AmbiguousTypeException
                else (T.IntVal(v), typ)
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
  and avec_selecteur (name:name):T.data_t*typ =

        (********* --> find_enum *********)
      (* les variables sont masquees *)
      let rec find_enum (list_cst:constant_symb list) :T.data_t*typ =
          match list_cst with
            | (Number _|StaticConst _|VarSymb _)::r -> find_enum r
            | EnumLitteral(typ,v,_)::_ when
                known_compatible_typ expected_typ typ ->
                T.IntVal v, typ
            | FunSymb(Some(typ), _)::_
                when known_compatible_typ expected_typ typ ->
                raise NonStaticExpression
            | EnumLitteral(typ,v,_)::r when expected_typ=None ->
                if mem_other_cst r None true
                then raise AmbiguousTypeException
                else T.IntVal v, typ
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
            | Number(T.IntVal    i,_)::_ ->
                        T.IntVal i, check_typ expected_typ IntegerConst
            | Number(T.FloatVal(f),_)::_ ->
                        T.FloatVal f, check_typ expected_typ Float
            | StaticConst(v, typ, _)::_ -> v, check_typ expected_typ typ
            | VarSymb _::_ -> raise NonStaticExpression
            | [] -> Npkcontext.report_error "eval_static.avec_selecteur"
                                  ("cannot find symbol "^(name_to_string name))
            | _ -> find_enum list_symb

        (********* avec_selecteur_courant) *********)
  and avec_selecteur_courant (ident:name) (name:name) :T.data_t*typ =

        (*********  --> find_global ) *********)
      let rec find_global (list_symb:constant_symb list) :T.data_t*typ =
          match list_symb with
            | [] -> Npkcontext.report_error "eval_static.find_global"
                                   ("cannot find symbol "^(name_to_string name))
            | Number(T.IntVal i,true)::_ ->
                  T.IntVal i, check_typ expected_typ IntegerConst
            | Number(T.FloatVal(f),true)::_ ->
                  T.FloatVal f, check_typ expected_typ Float
            | Number(T.BoolVal _, _)::_ -> Npkcontext.report_error
                                      "eval_static.find_global"
                                   "internal error : number cannot have EnumVal"
            | StaticConst(v, typ, true)::_ -> v, check_typ expected_typ typ
            | EnumLitteral(typ,v,true)::_
                when known_compatible_typ expected_typ typ ->
                T.IntVal v, typ
            | FunSymb(Some typ, false)::_
                when known_compatible_typ expected_typ typ ->
                raise NonStaticExpression
            | EnumLitteral(typ, v, true)::r when expected_typ=None ->
                if mem_other_cst r None false
                then raise AmbiguousTypeException
                else (T.IntVal(v), typ)
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
        | None, ident -> sans_selecteur ident name
        | Some pack, ident when extern || Symboltbl.SymStack.is_with tbl pack ->
                                            avec_selecteur (Some pack,ident)
        | (pack, ident) when pack = Symboltbl.SymStack.current tbl ->
                                        avec_selecteur_courant (None,ident) name
        | (Some pack, _) -> Npkcontext.report_error "eval_static.const"
              ("unknown package " ^pack)
  in
      eval_static_exp exp expected_typ

(**
 * Evaluate statically an integer expression. FIXME
 *)
let eval_static_integer_exp (exp:Ast.expression)
                            (csttbl:(name, constant_symb) Hashtbl.t)
                            (tbl:Symboltbl.SymStack.t)
                            (extern:bool)
    :nat =
    try
        let (v,_) =
          eval_static
              exp (Some IntegerConst)
              csttbl
              tbl
              extern in
            match v with
              | T.FloatVal _
              | T.BoolVal  _ -> Npkcontext.report_error
                          "eval_static.integer_exp"
                          "expected static integer constant"
              | T.IntVal i -> i
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
                       (tbl:Symboltbl.SymStack.t)
                       (extern:bool)
    :T.data_t =
     try
         let (v,_) = eval_static exp None
                                     csttbl
                                     tbl
                                     extern in
             match v with
               | T.BoolVal _ -> Npkcontext.report_error
                     "eval_static.integer_exp"
                     "expected static float or integer constant"
               | T.FloatVal _ | T.IntVal _ -> v
     with
       | NonStaticExpression -> Npkcontext.report_error
          "eval_static.integer_exp"
          "expected static expression"
       | AmbiguousTypeException -> Npkcontext.report_error
         "eval_static.integer_exp"
         "uncaught ambiguous type exception"

