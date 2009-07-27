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
module TC  = Typecheck
module Sym = Symboltbl

let (%+) = Nat.add
let (%-) = Nat.sub

let gtbl : Sym.t = Sym.create ()

let string_of_name = Ada_utils.name_to_string

let subtyp_to_adatyp st = subtyp_to_adatyp gtbl st
let merge_types sti = merge_types gtbl sti

let assert_known t =
  if (T.is_unknown t) then
    Npkcontext.report_error "assert_known" "fail";
    t

let assert_known_i sti =
  ignore (assert_known (merge_types sti));
  sti

let return_type_of subprogram = match subprogram with
  | Ast.Function  (_,_,st) -> Some st
  | Ast.Procedure _        -> None

(**
 * Try to find a body for a given specification.
 *
 * @param ~specification is the [basic_declaration] associated to the
 *                       specification, as found in a [package_spec].
 * @param ~bodylist is a list of possible bodies, as found
 *                  in a [declarative_part].
 * @return a boolean describing whether a body could be found.
 *)
let find_body_for_spec ~specification ~bodylist =
  (* Try to match a spec and a body *)
  let match_ok s b = match (s,b) with
    | Ast.SpecDecl(Ast.SubProgramSpec sps),
        Ast.BodyDecl(Ast.SubProgramBody (spsb,_,_,_,_)) -> sps = spsb
    | Ast.ObjectDecl _ as x,Ast.BasicDecl (Ast.ObjectDecl _ as y) -> x = y
    | _ -> false
  in
  List.exists (function bd -> match_ok specification bd) bodylist

(** Return the name for a specification. *)
let name_of_spec spec = match spec with
  | Ast.ObjectDecl (i,_,_)
  | Ast.NumberDecl (i,_) -> i
  | Ast.SpecDecl (Ast.SubProgramSpec (Ast.Function  (n,_,_)))
  | Ast.SpecDecl (Ast.SubProgramSpec (Ast.Procedure (n,_))) -> name_to_string n
  | Ast.SpecDecl (Ast.PackageSpec (n,_,_,_)) -> n
  | Ast.UseDecl _ -> "<no name>"

let check_package_body_against_spec ~body ~spec =
  let (pkgname,spec_and_loc,_,_) = spec in
  let (        body_and_loc    ) = body in
  let speclist = List.map fst spec_and_loc in
  let bodylist = List.map fst body_and_loc in
  (* Filter on specifications : only sp such as
   * filterspec sp = true will be checked.      *)
  let filterspec = function
    | Ast.NumberDecl _ | Ast.SpecDecl _ -> true
    | Ast.ObjectDecl _ | Ast.UseDecl  _ -> false
  in
  List.iter (function sp ->
    if (filterspec sp) then
      begin
        if not (find_body_for_spec ~specification:sp ~bodylist)
        then Npkcontext.report_error "Ada_utils.check_package_body_against_spec"
          ("Body for package " ^pkgname
          ^" does not match its specification : cannot find a body for \""
          ^(name_of_spec sp)^"\"")
      end
  ) speclist

let insert_constant ?t s =
  match s with
  | T.IntVal   x -> Ast.CInt   x, Ada_utils.with_default t T.universal_integer
  | T.BoolVal  x -> Ast.CBool  x, Ada_utils.with_default t T.boolean
  | T.FloatVal x -> Ast.CFloat x, Ada_utils.with_default t T.universal_real

let add_numberdecl ident value loc =
  let t = match value with
    | T.BoolVal  _ -> Npkcontext.report_error "add_numberdecl"
                      "Unexpected boolean value"
    | T.IntVal   _ -> T.universal_integer
    | T.FloatVal _ -> T.universal_real
  in
  Sym.add_variable gtbl ident loc t ~value ~no_storage:true

let normalize_ident ident package extern =
  if extern then (package, ident)
            else (None   , ident)

let build_init_stmt (x,exp,loc) =
  Ast.Assign(Ast.Lval ( Symboltbl.Lexical
                      , x
                      , assert_known(snd exp))
            , exp
            , true)
  , loc

let extract_subprog_spec ast =
    match ast with
      | (context, Body(SubProgramBody(spec,_,_)), loc) ->
        (context, Spec(SubProgramSpec(spec)),     loc)
      | (_, Spec _, _) -> Npkcontext.report_error
          "Ada_normalize.extract_subprog_spec"
        "body expected, specification found"
      | (_, Body(PackageBody _), _) -> Npkcontext.report_error
          "Ada_normalize.extract_subprog_spec"
        "subprogram body expected, package body found"

(* renvoie la specification correspondant a name,
   extrait eventuellement cette specification d'un corps
   de sous-programme, dans le cas ou aucun fichier de specification
   n'est fourni.*)
let parse_specification name =
  let spec_name = name^".ads" in
  let spec_ast =
    if Sys.file_exists spec_name
    then
      let res = File_parse.parse spec_name in
      if (!Npkcontext.verb_ast) then
        begin
          print_endline "Abstract Syntax Tree (extern)";
          print_endline "-----------------------------";
          Print_syntax_ada.print_ast [res];
          print_newline ();
        end;
      res
    else
      let body_name = name^".adb" in
        extract_subprog_spec (File_parse.parse body_name)
  in
    match spec_ast with
      | (_, Spec(_), _) -> spec_ast
      | (_, Body(_), _) -> Npkcontext.report_error
          "normalize.parse_specification"
            "specification expected, body found"

(* renvoie la specification du package correspondant a name.
   cette specification est normalisee *)
let parse_package_specification name =
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
 * Normalize an expression.
 *)
let rec normalize_exp ?expected_type exp =
  match exp with
    | CInt   x -> Ast.CInt   x,
                    (match expected_type with
                     | Some t -> t
                     | None ->
(*                       Npkcontext.report_warning "normalize_exp"
                             "Typing constant as universal_integer"; *)
                         T.universal_integer
                     )
    | CFloat x -> Ast.CFloat x,T.universal_real
    | CBool  x -> Ast.CBool  x,T.boolean
    | CChar  x -> Ast.CChar  x,T.character
    | Var    n -> begin (* n may denote the name of a parameterless function *)
                    try
                      let (sc,(t,_)) = Sym.find_variable ?expected_type gtbl n in
                      Ast.Var(sc,(snd n),(assert_known t)) ,t
                    with
                    | Sym.Parameterless_function rt ->
                                      Ast.FunctionCall( Sym.Lexical
                                                      , snd n
                                                      , []) ,rt
                    | Sym.Variable_no_storage (t,v) -> insert_constant ~t v
                  end
    | Unary (uop, exp)    -> normalize_uop uop exp
    | Binary(bop, e1, e2) -> normalize_binop bop e1 e2
    | Qualified(stn, exp) -> let t = snd (Sym.find_type gtbl stn) in
                                fst (normalize_exp ~expected_type:t exp),t
    | FunctionCall(n, params) -> normalize_fcall (n, params)
    | Attribute (st, attr, Some exp) ->
        begin
          let t = subtyp_to_adatyp (SubtypName st) in
          if attr = "succ" && T.is_scalar t then
            begin
              let (e',t') = normalize_exp exp in
              ( Ast.Binary ( Ast.Plus
                           , (e', t')
                           , ( Ast.CInt (Newspeak.Nat.one)
                             , T.universal_integer
                             )
                           )
              , t')
            end
          else Npkcontext.report_error "Normalize"
               "No such attribute"
        end
    | Attribute (st, attr, None) ->
        begin
          let t = subtyp_to_adatyp (SubtypName st) in
          let (t,v) = T.attr_get t attr in
          (fst (insert_constant v)), t
        end

(**
 * Normalize an actual argument.
 * The identifier does not have to be normalized (it is just a plain string),
 * but normalize the expression.
 *)
and normalize_arg (id,e) = id,normalize_exp e

and normalize_binop bop e1 e2 =
  let direct_op_trans =
    function
      | Plus  -> Ast.Plus  | Minus -> Ast.Minus | Div   -> Ast.Div
      | Mult  -> Ast.Mult  | Or    -> Ast.Or    | And   -> Ast.And
      | Gt    -> Ast.Gt    | Eq    -> Ast.Eq    | Rem   -> Ast.Rem
      | Mod   -> Ast.Mod   | Power -> Ast.Power
      |_ -> invalid_arg "direct_op_trans"
  in
  (* Is the operator overloaded ? *)
  if (Sym.is_operator_overloaded gtbl (Ada_utils.make_operator_name bop)) then
    let ovl_opname = (None,make_operator_name bop) in
    normalize_exp (FunctionCall(ovl_opname,[(None,e1);(None,e2)]))
  else
  match bop with
  (* Operators that does not exist in AST *)
  | Lt     -> normalize_exp (          Binary(Gt, e2, e1) )
  | Le     -> normalize_exp (Unary(Not,Binary(Gt, e1, e2)))
  | Ge     -> normalize_exp (Unary(Not,Binary(Gt, e2, e1)))
  | Neq    -> normalize_exp (Unary(Not,Binary(Eq, e1, e2)))
  | Xor    -> let (e1',t1) = normalize_exp e1 in
              let (e2',t2) = normalize_exp e2 in
              Ast.CondExp ((e1',t1)
                          ,(Ast.Not(e2',t2),t2)
                          ,(e2',t2)
                          )
              ,TC.type_of_xor t1 t2
  | OrElse -> let (e1',t1) = normalize_exp e1 in
              let (e2',t2) = normalize_exp e2 in
              Ast.CondExp ((e1',t1)
                          ,(Ast.CBool true,T.boolean)
                          ,(e2',t2)
                          )
              ,TC.type_of_binop Ast.Or t1 t2
  | AndThen-> let (e1',t1) = normalize_exp e1 in
              let (e2',t2) = normalize_exp e2 in
              Ast.CondExp ((e1',t1)
                          ,(e2',t2)
                          ,(Ast.CBool false,T.boolean)
                          )
              ,TC.type_of_binop Ast.And t1 t2
  (* Otherwise : direct translation *)
  | _ ->  let bop' = direct_op_trans bop in
          let expected_type = match (e1, e2) with
          | Var v1 , Var v2 -> Some (Sym.type_ovl_intersection gtbl
                                                              (snd v1)
                                                              (snd v2))
          | _      , Qualified (n,_) -> Some (snd (Sym.find_type gtbl n))
          | _               -> None
          in
          let (e1',t1) = normalize_exp ?expected_type e1 in
          let (e2',t2) = normalize_exp ?expected_type e2 in
          let t = T.coerce_types t1 t2 in
          Ast.Binary (bop', (e1',t), (e2',t)),
          TC.type_of_binop bop' t1 t2

and make_abs (exp,t) =
  let x = (exp,t) in
  let zero =
    if (T.is_integer t) then
      (Ast.CInt Nat.zero,T.universal_integer)
    else
      (Ast.CFloat (0.0), T.universal_real)
  in
  Ast.CondExp(
              (Ast.Binary(Ast.Gt, x, zero),T.boolean)
             , x
             ,(Ast.Binary(Ast.Minus, zero, x),t)
             )

and normalize_uop uop exp =
  let (ne,t) = normalize_exp exp in
  match uop with
     | Abs    -> make_abs (ne,TC.type_of_abs t),t
     | UMinus ->
         let zero =
           if (T.is_integer t) then
             CInt (Nat.zero)
           else if (T.is_float t) then
             CFloat (0.0)
           else Npkcontext.report_error "Normalize_uop"
             "Unary minus is definied for integer and floating-point types"
         in
           normalize_exp (Binary(Minus,zero,exp))
     | UPlus  -> (ne,TC.type_of_uplus t)
     | Not    -> Ast.Not(ne,t), TC.type_of_not t

and normalize_fcall (n, params) =
  let (sc,(_,top)) = Sym.find_subprogram gtbl n in
  let t = match top with
  | None -> Npkcontext.report_error "normalize_exp"
            "Expected function, got procedure"
  | Some top -> top
  in
  Ast.FunctionCall(sc, (snd n), List.map normalize_arg params),t

(**
 * Normalize a constraint.
 *)
and normalize_contrainte contrainte =
  let eval_range exp1 exp2 =
    let norm_exp1 = normalize_exp exp1
    and norm_exp2 = normalize_exp exp2 in
      (* on essaye d'evaluer les bornes *)
      (try
         let val1 = Eval.eval_static norm_exp1 gtbl in
         let val2 = Eval.eval_static norm_exp2 gtbl in
         let contrainte =  match (val1, val2) with
           | (T.FloatVal(f1),T.FloatVal(f2)) ->
               if f1<=f2
               then FloatRangeConstraint(f1, f2)
               else
                 Npkcontext.report_error
                   "Ada_normalize.normalize_contrainte"
                   "null range not accepted"

           | (T.IntVal(i1), T.IntVal(i2)) ->
               if (Nat.compare i1 i2)<=0
               then
                 IntegerRangeConstraint(i1, i2)
               else
                 Npkcontext.report_error
                   "Ada_normalize.normalize_contrainte"
                   "null range not accepted"

           | (T.BoolVal(b1), T.BoolVal(b2)) ->
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

let normalize_subtyp_ind (st,cst,w) =
  (st, Ada_utils.may normalize_contrainte cst, w)

let normalize_integer_range taille contrainte =
  match (taille, contrainte) with
    | (None, RangeConstraint(_)) ->
        begin
          try
            let norm_contrainte =
              normalize_contrainte contrainte
            in match norm_contrainte with
              | IntegerRangeConstraint(min, max) ->
                  let ikind = ikind_of_range min max
                  in IntegerRange(norm_contrainte, Some(ikind))

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

let normalize_typ_decl ident typ_decl loc =
 match typ_decl with
  | Enum symbs ->
      let ids = fst (List.split symbs) in
      let t = T.new_enumerated ids in
      Sym.add_type gtbl ident loc t;
      List.iter (fun (i,v) -> Sym.add_variable gtbl i loc t
                                                 ~value:(T.IntVal v)
                                                 ~no_storage:true
      ) symbs;
      ()
  | DerivedType(subtyp_ind) ->
      let norm_subtyp_ind = normalize_subtyp_ind subtyp_ind in
      let t = merge_types norm_subtyp_ind in
      let new_t = T.new_derived t in
        Sym.add_type gtbl ident loc new_t;

      begin
        match (T.extract_symbols t) with
          | None   -> ()
          | Some s -> 
                List.iter (fun (i,v) -> Sym.add_variable gtbl i loc
                                                   new_t
                                                   ~value:(T.IntVal (Newspeak.Nat.of_int v))
                                                   ~no_storage:true
                ) s
      end;

  | IntegerRange(contrainte,taille) ->
      let decl = normalize_integer_range taille contrainte in
      let range = match decl with
        | IntegerRange (IntegerRangeConstraint (min,max), _)
            -> T.(@...) min max
        | _ -> failwith "unreachable"
      in
      let t = T.new_range range in
      Sym.add_type gtbl ident loc t
  | Record r -> begin
                  let r' = List.map
                             (fun (id, st) ->
                               (id, subtyp_to_adatyp st)
                             ) r in
                  let t = T.new_record r' in
                  Sym.add_type gtbl ident loc t;
                end

(*
 * renvoie la specification normalisee du package correspondant
 * a name, les noms etant traites comme extern a la normalisation
 *)
let rec parse_extern_specification name =
  Npkcontext.print_debug "Parsing extern specification file";
  let spec_ast = parse_specification name in
  let norm_spec = (normalization spec_ast true) in
  Npkcontext.print_debug "Done parsing extern specification file";
  match norm_spec with
    | (_, Ast.Spec(spec), loc) -> (spec, loc)
    | (_, Ast.Body(_), _) -> Npkcontext.report_error
        "normalize.parse_extern_specification"
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
and normalization compil_unit extern =

  let normalize_ident_cur ident =
    normalize_ident ident (Sym.current gtbl) extern
  in

  let normalize_sub_program_spec subprog_spec ~addparam =
    let normalize_params param_list func =
      if addparam then
        Sym.enter_context ~desc:"SP body (parameters)" gtbl;
      List.map
        (fun param ->
           if func && (param.mode <> In)
           then Npkcontext.report_error
              "Normalize.normalize_params"
             ("invalid parameter mode : functions can only have"
             ^" \"in\" parameters");
           if (param.default_value <> None && param.mode <> In) then
             Npkcontext.report_error "Normalize.normalize_params"
             "default values are only allowed for \"in\" parameters";
           if addparam then begin
              Sym.add_variable gtbl param.formal_name (Newspeak.unknown_loc)
                                          (subtyp_to_adatyp param.param_type)
                                          ~ro:(param.mode = In)
              ;
           end;
          { Ast.param_type    = assert_known (
                                  subtyp_to_adatyp (
                                    param.param_type))
          ; Ast.formal_name   = param.formal_name
          ; Ast.mode          = param.mode
          ; Ast.default_value = may normalize_exp param.default_value
        }
        )
        param_list
    in
    let mk_param p = ( p.formal_name
                     , (p.mode = In  || p.mode = InOut)
                     , (p.mode = Out || p.mode = InOut)
                     , subtyp_to_adatyp p.param_type
                     )
    in match subprog_spec with
        | Function(name,param_list,return_type) ->
            let norm_name = normalize_ident_cur name in
            let t = subtyp_to_adatyp return_type in
              Sym.add_subprogram gtbl name (Newspeak.unknown_loc)
                                       (List.map mk_param param_list)
                                       (Some t)
                                       ;
              Ast.Function(norm_name,
                       normalize_params param_list true,
                       assert_known t)
        | Procedure(name,param_list) ->
            let norm_name = normalize_ident_cur name in
              Sym.add_subprogram gtbl name (Newspeak.unknown_loc)
                              (List.map mk_param param_list) None;
              Ast.Procedure(norm_name,
                        normalize_params param_list false)
  in
  let rec normalize_basic_decl item loc handle_init =
    match item with
    | UseDecl(use_clause) -> Sym.add_use gtbl use_clause;
                             [Ast.UseDecl use_clause]
    | ObjectDecl(ident_list,subtyp_ind,def, Variable) ->
        let norm_subtyp_ind = normalize_subtyp_ind subtyp_ind in
        let t = merge_types norm_subtyp_ind in
        begin match def with
          | None -> ()
          | Some exp -> List.iter (fun x -> handle_init x (normalize_exp
                                            ~expected_type:t exp) loc)
                          ident_list
        end;
          List.iter (fun x -> Sym.add_variable gtbl x loc t) ident_list;
          List.map (fun ident ->
            Ast.ObjectDecl(ident, t, Ast.Variable)
          ) ident_list
    | ObjectDecl(ident_list,subtyp_ind, Some(exp), Constant) ->
        let t = merge_types subtyp_ind in
        let normexp = normalize_exp ~expected_type:t exp in
        let status =
          try
            let value = Eval.eval_static normexp gtbl in
              List.iter (fun x -> Sym.add_variable gtbl x loc t ~value)
                        ident_list;
              Ast.StaticVal value
          with
            | AmbiguousTypeException -> Npkcontext.report_error
                                        "Ada_normalize.normalize_basic_decl"
                                        "uncaught ambiguous type exception"
            | NonStaticExpression -> List.iter
                                      (fun x -> Sym.add_variable gtbl x loc t
                                      ) ident_list;
                                      Ast.Constant

        in
          List.iter (fun x -> handle_init x normexp loc) ident_list;
          List.map (fun ident ->
            Ast.ObjectDecl(ident, t, status)
          ) ident_list
    | ObjectDecl _ -> Npkcontext.report_error
                     "Ada_normalize.normalize_basic_decl"
                     ("internal error : constant without default value"
                      ^"or already evaluated")
    | TypeDecl(id,typ_decl) ->
        normalize_typ_decl id typ_decl loc;
        []
    | SpecDecl(spec) -> [Ast.SpecDecl(normalize_spec spec)]
    | NumberDecl(ident, exp) ->
       let value = Eval.eval_static_number (normalize_exp exp) gtbl in
       add_numberdecl ident value loc;
       [Ast.NumberDecl(ident, value)]
    | SubtypDecl(ident, subtyp_ind) ->
        let norm_subtyp_ind = normalize_subtyp_ind subtyp_ind in
        Sym.add_type gtbl ident loc (merge_types norm_subtyp_ind);
        []
    | RenamingDecl (n, o) -> Sym.add_renaming_decl gtbl n o;
                             []
    | RepresentClause _ -> []

  and normalize_package_spec (name, list_decl) =
    Sym.set_current gtbl name;
    Sym.enter_context ~name ~desc:"Package spec" gtbl;
    let represtbl = Hashtbl.create 50 in
    List.iter (function
               | RepresentClause(id, aggr), loc ->
                   Hashtbl.add represtbl
                     (id)
                     ((id, aggr), loc)
               | _ -> ())
    list_decl;
    let init = ref [] in
    let add_init x exp _loc =
      init := (x,exp)::!init
    in
    let rec normalize_decls decls =
      List.flatten (List.map (fun (decl, loc) ->
                  Npkcontext.set_loc loc;
                  List.map (fun x -> (x,loc))
                         (normalize_basic_decl decl loc add_init)
               ) decls) in
    let norm_spec = normalize_decls list_decl in
    Sym.reset_current gtbl;
    let ctx = Sym.exit_context gtbl in
    (name, norm_spec, ctx, !init)

  and normalize_spec spec = match spec with
    | SubProgramSpec(subprogr_spec) -> Ast.SubProgramSpec(
          normalize_sub_program_spec subprogr_spec ~addparam:false)
    | PackageSpec(package_spec) ->
        Ast.PackageSpec(normalize_package_spec package_spec)

  in

  let rec normalize_lval = function
    | Lval n ->
        begin
        try
          let (sc, (t, ro)) = (Sym.find_variable gtbl n) in
          if ro then begin
            Npkcontext.report_error "normalize_instr"
               ("Invalid left value : '"^name_to_string n^"' is read-only")
          end;
          (Ast.Lval (sc, snd n,t), t)
        with
          Sym.Variable_no_storage _ ->
            Npkcontext.report_error "normalize_lval"
              "unexpected Variable_no_storage"
        end
    | ArrayAccess (_lv, _e) ->
 (*     Ast.ArrayAccess(fst (normalize_lval lv), normalize_exp  e),None; *)
        failwith "array assignment"
  in

  (**
   * The optional parameter return_type helps disambiguate return_statements :
   * while translating a block in a function, it is set to this function's
   * return type.
   * When translating other blocks, it shall be set to None.
   *)
  let rec normalize_instr ?return_type (instr,loc) =
    Npkcontext.set_loc loc;
    match instr with
    | NullInstr    -> None
    | ReturnSimple -> Some (Ast.ReturnSimple, loc)
    | Assign(lv, exp) -> begin
                           let (lv', t_lv) = normalize_lval lv in
                           let (e', t_exp) = normalize_exp exp in
                           if (not (T.is_compatible t_lv t_exp)) then
                             begin
                               Npkcontext.print_debug ("LV = "^T.print t_lv);
                               Npkcontext.print_debug ("EX = "^T.print t_exp);
                               Npkcontext.report_error "normalize_instr"
                                 "Incompatible types in assignment";
                             end;
                           let t_exp' = T.coerce_types t_lv (T.base_type t_exp) in
                           Some (Ast.Assign( lv'
                                           , (e',t_exp')
                                           , false
                                           ), loc)
                         end
    | Return(exp) -> Some (Ast.Return(normalize_exp ?expected_type:return_type
                                      exp), loc)
    | If(exp, instr_then, instr_else) ->
        Some (Ast.If( normalize_exp ~expected_type:T.boolean exp
                    , normalize_block ?return_type instr_then
                    , normalize_block ?return_type instr_else), loc)
    | Loop(NoScheme,instrs) -> Some (Ast.Loop(Ast.NoScheme,
                                              normalize_block ?return_type instrs),loc)
    | Loop(While(exp), instrs) -> Some (Ast.Loop(Ast.While(normalize_exp exp),
                     normalize_block ?return_type instrs), loc)
    | Loop(For(iter, exp1, exp2, is_rev), block) ->
        let dp = [BasicDecl (ObjectDecl ( [iter]
                             , ( SubtypName (None,"integer")
                               , None
                               , None
                               )
                             , Some (if is_rev then exp2 else exp1)
                             , Constant
                             )
                      )
              , loc]
        in
      Sym.enter_context gtbl;
      let (ndp,init) = normalize_decl_part dp in
      let nblock = (List.map build_init_stmt init)@normalize_block ?return_type block in
      let loop =
        [Ast.Loop
            ( Ast.While
               ( normalize_exp (if is_rev then Binary(Ge,Var(None,iter),exp1)
                                          else Binary(Le,Var(None,iter),exp2)))
               , nblock@[Ast.Assign ( Ast.Lval (Sym.Lexical,iter,T.integer)
                                    , normalize_exp( Binary((if is_rev
                                                               then Minus
                                                               else Plus)
                                                   , Var (None,iter)
                                                   , CInt (Nat.one)))
                                    , true (* unchecked *)
                                    )
                        , loc]
            )
            , loc]
      in Some (Ast.Block (ndp, Sym.exit_context gtbl, loop), loc)
    | Exit -> Some (Ast.Exit, loc)
    | ProcedureCall(n, params) ->
        let (sc,_) = Sym.find_subprogram gtbl n in
         Some (Ast.ProcedureCall( sc, snd n,List.map normalize_arg params), loc)
    | Case (e, choices, default) ->
              Some (Ast.Case (normalize_exp e,
                    List.map (function e,block->
                            normalize_exp e,
                            normalize_block ?return_type block)
                        choices,
                    Ada_utils.may (fun x -> normalize_block ?return_type x) default
                    ),loc)
    | Block (dp,blk) -> Sym.enter_context ~desc:"Declare block" gtbl;
                        let (ndp,init) = normalize_decl_part dp in
                        let norm_block = normalize_block ?return_type blk in
                        let init_stmts = List.map build_init_stmt init in
                        let ctx = Sym.exit_context gtbl in
                        Some (Ast.Block (ndp, ctx, init_stmts@norm_block), loc)

  and normalize_block ?return_type block =
    List_utils.filter_map (fun x -> normalize_instr ?return_type x) block

  and normalize_decl_part decl_part =
    let represtbl = Hashtbl.create 50 in
    List.iter (function
        | BasicDecl(RepresentClause(id, aggr)), loc ->
          Hashtbl.add represtbl id ((id, aggr), loc)
        | _ -> ()
    ) decl_part;
    let initializers = ref [] in
    let add_init x exp loc =
      initializers := (x,exp,loc) :: !initializers
    in
    let normalize_decl_items items =
      List.map (function
        | BasicDecl(basic),loc ->
            begin
              Npkcontext.set_loc loc;
                List.map (fun x -> Ast.BasicDecl x,loc)
                     (normalize_basic_decl basic loc add_init)
            end
        | BodyDecl(body),loc ->
            Npkcontext.set_loc loc;
            [Ast.BodyDecl(normalize_body body), loc]
      ) items in
    let ndp = List.flatten(normalize_decl_items decl_part) in
    List.iter (function
      | Ast.BasicDecl(Ast.SpecDecl (Ast.SubProgramSpec _) as sp),loc ->
            begin Npkcontext.set_loc loc;
              if not (find_body_for_spec ~specification:sp
                                          ~bodylist:(List.map fst ndp)) then
                   Npkcontext.report_error "normalize_decl_part"
                                           ("Declaration of \""
                                           ^(name_of_spec sp)
                                           ^"\" requires completion")
              end
      | _ -> ()
    ) ndp;
    let init = List.rev !initializers in
    (ndp,init)

  and normalize_body body  = match body with
    | SubProgramBody(subprog_decl,decl_part,block) ->
        let norm_subprog_decl =
          normalize_sub_program_spec subprog_decl ~addparam:true in
        Sym.enter_context ~desc:"SP body (locals)" gtbl;
        let return_type = return_type_of norm_subprog_decl in
        let (norm_decl_part,init) = normalize_decl_part decl_part in
        let norm_block = normalize_block ?return_type block in
        let init_stmts = List.map build_init_stmt init in
          let ctx1 = Sym.exit_context gtbl in
          let ctx2 = Sym.exit_context gtbl in (* params *)
          Ast.SubProgramBody( norm_subprog_decl
                            , norm_decl_part
                            , ctx1
                            , ctx2
                            , init_stmts@norm_block)
    | PackageBody(name, package_spec, decl_part) ->
        let (nname,nspec,ctx,_) = normalize_package_spec
                                    (with_default package_spec
                                        (parse_package_specification name)
                                    )
        in
          Sym.set_current gtbl name;
          Sym.enter_context ~name ~desc:"Package body" gtbl;
          let (ndp,init) = normalize_decl_part decl_part in

          let norm_spec = (nname,nspec,ctx,List.map (fun (x,y,_) -> (x,y)) init)
          in
          check_package_body_against_spec ~body:ndp ~spec:norm_spec;
          Sym.reset_current gtbl;
          let ctxb = Sym.exit_context gtbl in
          Ast.PackageBody(name, Some norm_spec, ctxb, ndp)

  in
  let normalize_lib_item lib_item loc =
    Npkcontext.set_loc loc;
    match lib_item with
      | Spec(spec) -> Ast.Spec(normalize_spec spec)
      | Body(body) -> Ast.Body(normalize_body body)

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
        | Ast.ObjectDecl(ident, t, (Ast.Variable | Ast.Constant)) ->
            Sym.add_variable gtbl ident loc t
        | Ast.ObjectDecl(ident, t, Ast.StaticVal value) ->
            Sym.add_variable gtbl ident loc t ~value;
        | Ast.NumberDecl(ident, value) ->
            add_numberdecl ident value loc
        | Ast.SpecDecl _
        | Ast.UseDecl  _ -> ()

    in match spec with
      | Ast.SubProgramSpec(Ast.Function(_name, [], _return_typ)) -> ()
      | Ast.SubProgramSpec(Ast.Function(_name, _, _)|Ast.Procedure(_name, _)) ->
          ()
      | Ast.PackageSpec(name, basic_decls,_,_) ->
          Sym.set_current gtbl name;
          Sym.enter_context ~name ~desc:"Package spec (extern)" gtbl;
          List.iter add_extern_basic_decl basic_decls;
          Sym.reset_current gtbl;
          ignore (Sym.exit_context gtbl);
          Sym.add_with gtbl name

  in

  (* normalise le context, en supprimant les doublons with *)
  let rec normalize_context context previous_with =
    match context with
      | [] -> []
      | With(nom, _, spec)::r ->
          if (List.mem nom previous_with) then
            normalize_context r previous_with
          else begin
            let (norm_spec, loc) = match spec with
              | None   -> parse_extern_specification nom
              | Some _ -> Npkcontext.report_error
                  "Ada_normalize.normalize_context"
                    "internal error : spec provided"
            in
              add_extern_spec norm_spec;
              Ast.With(nom, loc, Some(norm_spec, loc))
              ::normalize_context r (nom::previous_with)
          end
      | UseContext(n)::r -> Sym.add_use gtbl n;
                            Ast.UseContext n::normalize_context r previous_with
  in

  let (context,lib_item,loc) = compil_unit in
  let norm_context = normalize_context context [] in
  let norm_lib_item = normalize_lib_item lib_item loc in
    Npkcontext.forget_loc ();
    (norm_context
    ,norm_lib_item
    ,loc
    )
