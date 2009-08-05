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
let g_extern = ref false

let mangle_sname = function
  | []       -> failwith "unreachable @ normalize:mangle_sname"
  | x::[]    -> None  , x
  | x::y::[] -> Some x, y
  | _        -> Npkcontext.report_error "mangle_sname"
                  "chain of selected names is too deep"

let rec make_name_of_lval = function
  | Var x -> [x]
  | SName (pf, tl) ->make_name_of_lval pf@[tl]
  | _ -> invalid_arg "make_name_of_lval"

let subtyp_to_adatyp gtbl n =
  let n' = mangle_sname n in
  try
    snd(Symboltbl.find_type gtbl n')
  with Not_found ->
    begin
      Npkcontext.report_warning "ST2AT"
        ("Cannot find type '"
        ^name_to_string n
        ^"'");
      T.new_unknown "Cannot find type (subtyp_to_adatyp)";
    end

let merge_types gtbl (tp, cstr) =
  let t = subtyp_to_adatyp gtbl tp in
  if (T.is_unknown t) then
    Npkcontext.report_warning "merge_types"
    ("merged subtype indication into unknown type ("^T.get_reason t^")");
  match cstr with
  | None -> t
  | Some c -> T.new_constr t c

let subtyp_to_adatyp st = subtyp_to_adatyp gtbl st
let merge_types sti = merge_types gtbl sti

let return_type_of subprogram = match subprogram with
  | Ast.Function  (_,_,st) -> Some st
  | Ast.Procedure _        -> None

(**************************************************
 *                                                *
 *    Selected_name -> package/var, record, etc   *
 *                                                *
 **************************************************)

type selected =
  | SelectedVar    of Sym.scope          (* Package       *)
                    * string             (* Varname       *)
                    * T.t                (* Type          *)
                    * bool               (* Ro            *)

  | SelectedFCall  of Sym.scope * string (* Function name *)
                    * T.t                (* Return type   *)

  | SelectedRecord of Ast.lval           (* Varname       *)
                    * int                (* Offset        *)
                    * T.t                (* Field type    *)

  | SelectedConst  of T.t                (* Type          *)
                    * T.data_t           (* Value         *)


let rec resolve_selected ?expected_type n =
  let resolve_variable pkg id =
    begin
      try
        let (sc,(t, ro)) = Sym.find_variable ?expected_type
                                             gtbl (pkg,id) in
        SelectedVar(sc,id,t,ro)
      with
      | Sym.Parameterless_function (sc,rt) -> SelectedFCall( sc , id , rt)
      | Sym.Variable_no_storage (t,v) -> SelectedConst (t, v)
    end
  in
  match n with
  | SName(Var pfx, fld) ->
      begin
        try
          let (_,(t,_)) = Sym.find_variable ~silent:true gtbl (None, pfx) in
          let (off, tf) = T.record_field t fld in
          let lv = Ast.Var (Sym.Lexical, pfx, t) in
          SelectedRecord (lv , off, tf)
        with Not_found -> resolve_variable (Some pfx) fld
      end
  | Var id -> resolve_variable None id
  | SName (SName (Var _, _) as pf, z) -> begin
      match (resolve_selected pf) with
        | SelectedConst _ -> failwith "a constant cannot have a field"
        | SelectedFCall _ -> failwith "a function call cannot have a field"
        | SelectedVar   _ -> failwith "a variable cannot have a field"
        | SelectedRecord (lv, off0, tf0) ->
            let (off,tf) = T.record_field tf0 z in
            let lv = Ast.RecordAccess (lv,off0,tf0) in
            SelectedRecord (lv, off, tf)
    end
  | _ -> Npkcontext.report_error "normalize"
                    "Chain of selected_names too long"

(**************************************************
 *                                                *
 *             Body vs spec functions             *
 *                                                *
 **************************************************)

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
  | Ast.ObjectDecl (i,_,_,_)
  | Ast.NumberDecl (i,_) -> i
  | Ast.SpecDecl (Ast.SubProgramSpec (Ast.Function  (n,_,_)))
  | Ast.SpecDecl (Ast.SubProgramSpec (Ast.Procedure (n,_))) -> name_to_string n
  | Ast.SpecDecl (Ast.PackageSpec (n,_,_)) -> n

let check_package_body_against_spec ~body ~spec =
  let (pkgname,spec_and_loc,_) = spec in
  let (        body_and_loc  ) = body in
  let speclist = List.map fst spec_and_loc in
  let bodylist = List.map fst body_and_loc in
  (* Filter on specifications : only sp such as
   * filterspec sp = true will be checked.      *)
  let filterspec = function
    | Ast.NumberDecl _ | Ast.SpecDecl _ -> true
    | Ast.ObjectDecl _ -> false
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


(**
 * Compute the actual argument list for a subprogram call.
 * Given an (optionnaly named)-argument list and a function specification
 * (describing formal parameters' name, default values, etc.)
 *
 * Algorithm :
 *   - first, the positional arguments are extracted and put in the
 *     beginning of the to-be-translated parameter list.
 *   - then, the remaining (named) arguments are put in their right place,
 *     according to the specification of the subprogram.
 *   - finally, the missing named parameters are replaced with their
 *     default values, if provided.
 *
 * An error may occur in one of the following cases :
 *   - A positional parameter follows a named parameter.
 *   - A parameter without default value is not given an actual value.
 *   - A named parameter is specified more than once.
 *
 * @param args the name => value association list
 * @param spec the function specification (holding default values, etc)
 * @return a list of expressions which are the actual parameters.
 *)
let make_arg_list args spec =
  let argtbl = Hashtbl.create 5 in

  (**
   * Step 1 : extract positional parameters. Named parameters go into the
   * argtbl hashtable.
   *
   * Non-leading positional parameters, if any, remain in the "positional"
   * list and will lead to errors.
   *
   * /!\ Side-effects : this function references the argtbl variable.
   *)
  let rec extract_positional_parameters ar =
    match ar with
      |             []   -> []
      | (Some  _, _)::_  ->
        (* don't stop at first named argument : populate argtbl *)
            List.iter
                (function
                   | None   , _ -> Npkcontext.report_error "firstpass.fcall"
                             "Named parameters shall follow positional ones"
                   | Some id, e ->
                        if (Hashtbl.mem argtbl id) then
                            Npkcontext.report_error "firstpass.fcall"
                            ("Parameter "^id^" appears twice")
                        else
                            Hashtbl.add argtbl id e;
                )
                ar;
            []
      | (None   , e)::tl -> e::(extract_positional_parameters tl)
  in

  (**
   * Step 2 : merge this list with the function specification, in order to
   * name the (formerly) positional parameters.
   * For the remaining parameters :
   *   - try to fetch them from the argtbl hashtable
   *   - try to assign their default value
   *
   * /!\ Side-effects : this function references the argtbl variable.
   *)
  let rec merge_with_specification (pos_list : Ast.expression list)
                                   (spec     : Ast.param      list)
      :Ast.argument list =
          match pos_list, spec with
            |  [],_  -> (* end of positional parameters *)
                        List.map (function x ->
                          ( x.Ast.formal_name
                          , x.Ast.param_type
                          , (
                                 try Hashtbl.find argtbl x.Ast.formal_name
                                 with Not_found -> begin
                                     match x.Ast.default_value with
                                       | Some value -> value
                                       | None ->
                                      Npkcontext.report_error
                                      "firstpass.fcall"
                                      ("No value provided for "
                                      ^"parameter "^x.Ast.formal_name
                                      ^", which has no default one.")
                                 end
                             )))
                             spec
            | (ev,t)::pt,s::st -> let t' = T.coerce_types t s.Ast.param_type in
                                  (s.Ast.formal_name, s.Ast.param_type, (ev,t'))
                                  ::(merge_with_specification pt st)
            | _::_,[]     -> Npkcontext.report_error "Firstpass.function_call"
                            "Too many actual arguments in function call"
  in
      (* Step 1... *)
      let pos      = extract_positional_parameters args in
      (* Step 2... *)
      let eff_args = merge_with_specification pos spec in
      eff_args

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
    | Lval(ParExp(n, params)) -> normalize_fcall (n, params)
    | Lval(PtrDeref _ as lv) -> let nlv, tlv = normalize_lval lv in
                                Ast.Lval (nlv), tlv
    | Lval lv ->
        begin
          match resolve_selected ?expected_type lv with
          | SelectedVar   (sc, id,  t, _) -> Ast.Lval(Ast.Var(sc,id,t)) ,t
          | SelectedFCall (sc, id, rt) -> Ast.FunctionCall (sc, id, [], rt), rt
          | SelectedConst (t,v) -> insert_constant ~t v
          | SelectedRecord (lv, off, tf) ->
              Ast.Lval(Ast.RecordAccess(lv, off, tf)), tf
        end
    | Unary (uop, exp)    -> normalize_uop uop exp
    | Binary(bop, e1, e2) -> normalize_binop bop e1 e2
    | Qualified(lv, exp) -> let stn = make_name_of_lval lv in
                            let t = subtyp_to_adatyp stn in
                                fst (normalize_exp ~expected_type:t exp),t
    | Attribute (lv , "address", None) ->
        let (nlv, tlv) = normalize_lval lv in
        Ast.AddressOf (nlv, tlv), T.system_address
    | Attribute (lv, attr, Some exp) ->
        begin
          let st = make_name_of_lval lv in
          let t = subtyp_to_adatyp st in
          let one = Ast.CInt (Newspeak.Nat.one) in
          match attr with
            | "succ" -> ( Ast.Binary ( Ast.Plus
                                     , normalize_exp exp
                                     , ( one, t)
                                     )
                        , t)
            | "pred" -> ( Ast.Binary ( Ast.Minus
                                     , normalize_exp exp
                                     , ( one, t)
                                     )
                        , t)
            | "pos"  -> ( fst (normalize_exp exp)
                        , T.universal_integer
                        )
            | _      -> Npkcontext.report_error "normalize"
                          ("No such function-attribute : '"^attr^"'")
        end
    | Attribute (lv, attr, None) ->
        begin
          let st = make_name_of_lval lv in
          let t = subtyp_to_adatyp st in
          let (exp,t') = T.attr_get t attr in
          let (exp',_) = normalize_exp exp in
          (exp',t')
        end
    | Aggregate _ ->
        Npkcontext.report_error "normalize_exp"
          "Array aggregate found without direct lvalue"

(**
 * Normalize an actual argument.
 * The identifier does not have to be normalized (it is just a plain string),
 * but normalize the expression.
 *)
and normalize_arg (id,e) = id,normalize_exp e

and normalize_param param =
  { Ast.param_type    = subtyp_to_adatyp param.param_type
  ; Ast.formal_name   = param.formal_name
  ; Ast.mode          = param.mode
  ; Ast.default_value = may normalize_exp param.default_value
  }

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
    let ovl_opname = make_operator_name bop in
    normalize_exp (Lval(ParExp(Var ovl_opname,[(None,e1);(None,e2)])))
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
          let expected_type =
            match (e1, e2) with
          | Lval l1 , Lval l2 -> let v1 = make_name_of_lval l1 in
                                 let v2 = make_name_of_lval l2 in
                                 Some (Sym.type_ovl_intersection gtbl
                                      (ListUtils.last v1)
                                      (ListUtils.last v2))
          | _      , Qualified (lvn,_) -> let n = make_name_of_lval lvn in
                                          Some (subtyp_to_adatyp n)
          | _               -> None
          in
          let (e1',t1) = normalize_exp ?expected_type e1 in
          let (e2',t2) = normalize_exp ?expected_type e2 in
          Ast.Binary (bop', (e1',t1), (e2',t2)),
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
  (* Maybe this indexed expression is an array-value *)
  let n = make_name_of_lval n in
  let n = mangle_sname n in
  try
    let (sc,(spec,top)) = Sym.find_subprogram ~silent:true gtbl n in
    let t = match top with
    | None -> Npkcontext.report_error "normalize_exp"
              "Expected function, got procedure"
    | Some top -> top
    in
    let norm_args = List.map normalize_arg params in
    let norm_spec = List.map normalize_param spec in
    let effective_args = make_arg_list norm_args norm_spec in
    Ast.FunctionCall(sc, (snd n), effective_args, t),t
  with Not_found ->
    begin
      let (sc,(t,_)) = Sym.find_variable gtbl n in
      let tc = fst (T.extract_array_types t) in
      let params' = List.map snd params in
      let lv = Ast.Var (sc, snd n, t) in
      Ast.Lval (Ast.ArrayAccess(lv, List.map normalize_exp params')),tc
    end

and eval_range (exp1, exp2) =
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
       | Eval.NonStaticExpression ->
           Npkcontext.report_error
             "Ada_normalize.normalize_contrainte"
               "non-static constraint are not yet supported"
    )

and normalize_subtyp_ind (st,cst) =
  (st, Ada_utils.may eval_range cst)

and normalize_typ_decl ident typ_decl loc =
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
                List.iter (fun (i,v) ->
                  let value = T.IntVal (Newspeak.Nat.of_int v) in
                  Sym.add_variable gtbl i loc new_t ~value ~no_storage:true
                ) s
      end
  | IntegerRange(min, max) ->
      let range = eval_range (min, max) in
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
  | Array (i, c) ->
      let prepare x =
        merge_types (normalize_subtyp_ind x)
      in
      let index     = List.map prepare i in
      let component =          prepare c in
      let t = T.new_array ~component ~index in
      Sym.add_type gtbl ident loc t
  | Access stn ->
      let te = subtyp_to_adatyp stn in
      let t = T.new_access te in
      Sym.add_type gtbl ident loc t

(*
 * renvoie la specification normalisee du package correspondant
 * a name, les noms etant traites comme extern a la normalisation
 *)
and parse_extern_specification name =
  Npkcontext.print_debug "Parsing extern specification file";
  let spec_ast = parse_specification name in
  let norm_spec = (normalization spec_ast true) in
  Npkcontext.print_debug "Done parsing extern specification file";
  match norm_spec with
    | (_, Ast.Spec(spec), loc) -> (spec, loc)
    | (_, Ast.Body(_), _) -> Npkcontext.report_error
        "normalize.parse_extern_specification"
          "internal error : specification expected, body found"

and normalize_ident_cur ident =
  match (!g_extern, Sym.current gtbl) with
  | true, Some x -> [x;ident]
  | true, None   -> [ident]
  | false, _     -> [ident]

and normalize_sub_program_spec subprog_spec ~addparam =
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
           normalize_param param
        )
        param_list
    in
    match subprog_spec with
        | Function(name,param_list,return_type) ->
            let norm_name = normalize_ident_cur name in
            let t = subtyp_to_adatyp return_type in
              Sym.add_subprogram gtbl name (Newspeak.unknown_loc)
                                       param_list
                                       (Some t)
                                       ;
              Ast.Function( norm_name
                          , normalize_params param_list true
                          , t
                          )
        | Procedure(name,param_list) ->
            let norm_name = normalize_ident_cur name in
              Sym.add_subprogram gtbl name (Newspeak.unknown_loc)
                              param_list None;
              Ast.Procedure(norm_name,
                        normalize_params param_list false)

  and normalize_basic_decl item loc =
    match item with
    | UseDecl(use_clause) -> Sym.add_use gtbl use_clause;
                             []
    | ObjectDecl(ident_list,subtyp_ind,def, Variable) ->
        let norm_subtyp_ind = normalize_subtyp_ind subtyp_ind in
        let t = merge_types norm_subtyp_ind in
        List.iter (fun x -> Sym.add_variable gtbl x loc t) ident_list;
        List.map (fun ident ->
          Ast.ObjectDecl( ident
                        , t
                        , Ast.Variable
                        , build_init_stmt (ident, def, loc)
                        )
        ) ident_list
    | ObjectDecl(ident_list,subtyp_ind, Some(exp), Constant) ->
        let norm_subtyp_ind = normalize_subtyp_ind subtyp_ind in
        let t = merge_types norm_subtyp_ind in
        let status = begin
          match exp with
          | Aggregate _ ->
              List.iter (fun x -> Sym.add_variable gtbl x loc t) ident_list;
              Ast.Constant
          | _ ->
            try
              let normexp = normalize_exp ~expected_type:t exp in
              let value = Eval.eval_static normexp gtbl in
                List.iter (fun x -> Sym.add_variable gtbl x loc t ~value)
                          ident_list;
                Ast.StaticVal value
            with
              | Eval.NonStaticExpression -> List.iter
                                        (fun x -> Sym.add_variable gtbl x loc t
                                        ) ident_list;
                                        Ast.Constant
        end
        in
          List.map (fun ident ->
            Ast.ObjectDecl( ident
                          , t
                          , status
                          , build_init_stmt (ident, Some exp, loc)
                          )
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
    | RepresentClause _   -> []
    | GenericInstanciation (_,n,_) -> Npkcontext.report_warning "normalize"
                                        ("ignoring generic instanciation of '"
                                                         ^name_to_string n^"'");
                                      []


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
    let rec normalize_decls decls =
      List.flatten (List.map (fun (decl, loc) ->
                  Npkcontext.set_loc loc;
                  List.map (fun x -> (x,loc))
                         (normalize_basic_decl decl loc)
               ) decls) in
    let norm_spec = normalize_decls list_decl in
    Sym.reset_current gtbl;
    let ctx = Sym.exit_context gtbl in
    (name, norm_spec, ctx)

  and normalize_spec spec = match spec with
    | SubProgramSpec(subprogr_spec) -> Ast.SubProgramSpec(
          normalize_sub_program_spec subprogr_spec ~addparam:false)
    | PackageSpec(package_spec) ->
        Ast.PackageSpec(normalize_package_spec package_spec)

  and normalize_lval ?(force=false) ?expected_type = function
    | (Var _ | SName _) as lv->
        (* Only in write contexts *)
        begin match resolve_selected ?expected_type lv with
        | SelectedVar    (sc, id,  t, ro) ->
            if (ro && not force) then
            Npkcontext.report_error "normalize_instr"
               ("Invalid left value : '"^id^"' is read-only");
            Ast.Var  (sc, id, t), t
        | SelectedRecord (lv, off, tf) ->
            Ast.RecordAccess (lv, off, tf), tf
        | SelectedFCall _
        | SelectedConst _ -> Npkcontext.report_error "normalize_lval"
                               ("Invalid left-value")
        end
    | ParExp (lv, e) ->
        let (lv',t) = normalize_lval lv in
        Ast.ArrayAccess(lv' , List.map (fun x -> normalize_exp (snd x)) e), t
    | PtrDeref lv ->
        begin
          let (nlv, tlv) = normalize_lval lv in
          let te = T.extract_access_type tlv in
          Ast.PtrDeref (nlv, tlv), te
        end

  and build_init_stmt (x,exp,loc) =
    match exp with
    | None -> None
    | Some def ->
        Some (normalize_block ~force_lval:true [Assign(Var x, def), loc])

  (**
   * The optional parameter return_type helps disambiguate return_statements :
   * while translating a block in a function, it is set to this function's
   * return type.
   * When translating other blocks, it shall be set to None.
   *)
  and normalize_instr ?return_type ?(force_lval=false) (instr,loc) =
    Npkcontext.set_loc loc;
    match instr with
    | NullInstr    -> []
    | ReturnSimple -> [Ast.ReturnSimple, loc]
    | Assign(lv, Aggregate (NamedAggregate bare_assoc_list)) ->
        let (nlv, tlv) = normalize_lval lv in
        normalize_assign_aggregate nlv tlv bare_assoc_list loc
    | Assign(lv, Aggregate (PositionalAggregate exp_list)) ->
        let (lv', t_lv) = normalize_lval lv in
        let (_tc, ti)   = T.extract_array_types t_lv in
        let all_values  = T.all_values ti in
        List.map2 (fun type_val exp ->
          let k = insert_constant (T.IntVal type_val) in
          let v = normalize_exp exp in
          Ast.Assign (Ast.ArrayAccess (lv', [k]), v), loc
        ) all_values exp_list
    | LvalInstr(ParExp(lv, params)) ->
        let n = make_name_of_lval lv in
        let n = mangle_sname n in
        let (sc,(spec,_)) = Sym.find_subprogram gtbl n in
        let norm_args = List.map normalize_arg params in
        let norm_spec = List.map normalize_param spec in
        let effective_args = make_arg_list norm_args norm_spec in
         [Ast.ProcedureCall( sc, snd n,effective_args), loc]
    | LvalInstr((Var _|SName (Var _,_)) as lv) -> normalize_instr (LvalInstr (ParExp(lv, [])),loc)
    | LvalInstr _ -> Npkcontext.report_error "normalize_instr"
                       "Statement looks like a procedure call but is not one"
    | Assign(lv, exp) ->
        begin
          let (lv', t_lv) = normalize_lval ~force:force_lval lv in
          let (e', t_exp) = normalize_exp ~expected_type:t_lv exp in
          if (not (T.is_compatible t_lv t_exp)) then
            begin
              Npkcontext.print_debug ("LV = "^T.print t_lv);
              Npkcontext.print_debug ("EX = "^T.print t_exp);
              Npkcontext.report_error "normalize_instr"
                "Incompatible types in assignment";
            end;
          [Ast.Assign( lv'
                     , (e',t_exp)
                     ), loc]
        end
    | Return(exp) -> [Ast.Return(normalize_exp ?expected_type:return_type
                                 exp), loc]
    | If(exp, instr_then, instr_else) ->
        [Ast.If( normalize_exp ~expected_type:T.boolean exp
               , normalize_block ?return_type instr_then
               , normalize_block ?return_type instr_else), loc]
    | Loop(NoScheme,instrs) -> [Ast.Loop(Ast.NoScheme,
                                  normalize_block ?return_type instrs),loc]
    | Loop(While(exp), instrs) -> [Ast.Loop(Ast.While(normalize_exp exp),
                     normalize_block ?return_type instrs), loc]
    | Loop(For(iter, range, is_rev), block) ->
      let (exp1, exp2) = match range with
        | DirectRange (min, max) -> (min, max)
        | ArrayRange n -> begin
                            let n = make_name_of_lval n in
                            let n = mangle_sname n in
                            let (_,(t,_)) = Sym.find_variable gtbl n in
                            ( fst (T.attr_get t "first")
                            , fst (T.attr_get t "last"))
                          end
        | SubtypeRange lv -> begin
                               let st = make_name_of_lval lv in
                               let t = subtyp_to_adatyp st in
                                 ( fst (T.attr_get t "first")
                                 , fst (T.attr_get t "last"))
                             end
      in
      let dp = [BasicDecl (ObjectDecl ( [iter]
                           , ( ["standard";"integer"]
                             , None
                             )
                           , Some (if is_rev then exp2 else exp1)
                           , Constant
                           )
                    )
            , loc]
      in
      Sym.enter_context gtbl;
      let ndp = normalize_decl_part dp in
      let nblock = normalize_block ?return_type block in
      let loop =
        [Ast.Loop
            ( Ast.While
              ( normalize_exp (if is_rev then Binary(Ge,Lval(Var iter),exp1)
                                         else Binary(Le,Lval(Var iter),exp2))
                               )
               , nblock@[Ast.Assign ( Ast.Var (Sym.Lexical,iter,T.integer)
                                    , normalize_exp( Binary((if is_rev
                                                               then Minus
                                                               else Plus)
                                                   , Lval(Var iter)
                                                   , CInt (Nat.one)))
                                    )
                        , loc]
            )
            , loc]
      in [Ast.Block (ndp, Sym.exit_context gtbl, loop), loc]
    | Exit -> [Ast.Exit, loc]
    | Case (e, choices, default) ->
              [Ast.Case (normalize_exp e,
                    List.map (function e,block->
                            normalize_exp e,
                            normalize_block ?return_type block)
                        choices,
                    Ada_utils.may (fun x -> normalize_block ?return_type x)
                                  default
                    ),loc]
    | Block (dp,blk) -> Sym.enter_context ~desc:"Declare block" gtbl;
                        let ndp = normalize_decl_part dp in
                        let norm_block = normalize_block ?return_type blk in
                        let ctx = Sym.exit_context gtbl in
                        [Ast.Block (ndp, ctx, norm_block), loc]

  and normalize_assign_aggregate nlv t_lv bare_assoc_list loc =
    let array_case _ =
      let module NatSet = Set.Make (Newspeak.Nat) in
      (*
       * From bare_assoc_list : (selector * exp) list
       * we want to build some assoc_list : (exp * exp) list * exp option.
       * In this step we can check that :
       *   - there is at most one others clause.
       *   - if present, it is the last one.
       *)
      let (assoc_list, others_opt) =
        let compute_val x =
          let x' = normalize_exp x in
          match (Eval.eval_static x' gtbl) with
          | T.IntVal x -> x
          | _ -> Npkcontext.report_error "normalize"
                  ("Within an aggregate, selectors"
                  ^"should evaluate as integers")
        in
        List.fold_left (fun (kvl, others_exp) (selector, value) ->
          let rec handle = function
          | AggrExp e ->
              begin
                if others_exp <> None then
                  Npkcontext.report_error "normalize"
                  "In an aggregate, \"others\" shall be the last clause";
                let e' = compute_val e in
                ((e', value)::kvl, None)
              end
          | AggrRange (e1, e2) ->
              begin
                if others_exp <> None then
                  Npkcontext.report_error "normalize"
                  "In an aggregate, \"others\" shall be the last clause";
                let e1' = compute_val e1 in
                let e2' = compute_val e2 in
                let rec interval a b =
                  if (Newspeak.Nat.compare b a < 0) then []
                  else (a,value)::(interval (Newspeak.Nat.add_int 1 a) b)
                in
                (interval e1' e2')@kvl, None
              end
          | AggrOthers ->
              begin
                if others_exp <> None then
                  Npkcontext.report_error "normalize"
                  "In an aggregate, there shall be only one \"others\" clause";
                (kvl, Some value)
              end
          | AggrField f -> handle (AggrExp (Lval(Var f)))
          in handle selector
        ) ([],None) bare_assoc_list
      in
      (*
       * Now, using lv's type, we can :
       *   - compute missing elements
       *   - replace 'others' with them
       *)
      let (_tc, ti)   = T.extract_array_types t_lv in
      let other_list = match others_opt with
        | None         -> []
        | Some oth_exp ->
            begin
              let all_values  = T.all_values ti in
              let mk_set l =
                List.fold_left (fun x y -> NatSet.add y x)
                               NatSet.empty l
              in
              let all_values_set = mk_set all_values in
              let defined_values_set =
                mk_set (List.map fst assoc_list)
              in
              let missing_others =
                NatSet.elements (NatSet.diff all_values_set
                                             defined_values_set)
              in
              List.rev_map (function x -> (CInt x, oth_exp)) missing_others
            end
      in
      let assoc_list' = List.map (fun (x,y) -> CInt x,y) assoc_list in
      List.rev_map (fun (aggr_k, aggr_v) ->
        (* id[aggr_k] <- aggr_v *)
        let key   = normalize_exp aggr_k in
        let value = normalize_exp aggr_v in
        Ast.Assign (Ast.ArrayAccess (nlv, [key]), value), loc
      ) (other_list@assoc_list')
      (* end of array_case *)
    in
    let record_case _ =
      let module FieldSet = Set.Make (String) in
      (* (selector*exp) list --> (string*exp) list*exp option *)
      let (assoc_list,other) = List.fold_left (fun (fvl,others_opt) (selector, value) ->
        match selector with
        | AggrField  f -> begin
                            if (others_opt) <> None then
                              Npkcontext.report_error "normalize:aggregate"
                                "'others' clause should be the last one";
                            (f, value)::fvl,others_opt
                          end
        | AggrOthers   -> begin
                            if (others_opt) <> None then
                              Npkcontext.report_error "normalize:aggregate"
                              "There shall be only one 'others' clause";
                            fvl,Some value
                          end
        | _ -> Npkcontext.report_error "normalize:aggregate"
                 "Expected a field name in aggregate"
      ) ([],None) bare_assoc_list in
      let other_list = match other with
        | None           -> []
        | Some other_exp ->
            begin
              let flds = T.all_record_fields t_lv in
              let mk_set l =
                List.fold_left (fun x y -> FieldSet.add y x)
                               FieldSet.empty l
              in
              let all_fields = mk_set flds in
              let defined    = mk_set (List.map fst assoc_list) in
              let missing_others =
                FieldSet.elements (FieldSet.diff all_fields defined) in
              List.map (fun f -> (f, other_exp)) missing_others
            end
      in
      List.rev_map (fun (aggr_fld, aggr_val) ->
        (* id.aggr_fld <- aggr_v *)
        let (off, tf) = T.record_field t_lv aggr_fld in
        let v = normalize_exp aggr_val in
        Ast.Assign (Ast.RecordAccess (nlv, off, tf), v), loc
      ) (assoc_list@other_list)
      (* end of record_case *)
    in
      if      T.is_array  t_lv then array_case  ()
      else if T.is_record t_lv then record_case ()
      else Npkcontext.report_error "normalize_assign_aggregate"
             "Expecting an array or a record as lvalue"

  and normalize_block ?return_type ?(force_lval=false) block =
    List.flatten (List.map (normalize_instr ?return_type ~force_lval) block)

  and normalize_decl_part decl_part =
    let represtbl = Hashtbl.create 50 in
    List.iter (function
        | BasicDecl(RepresentClause(id, aggr)), loc ->
          Hashtbl.add represtbl id ((id, aggr), loc)
        | _ -> ()
    ) decl_part;
    let normalize_decl_items items =
      List.map (function
        | BasicDecl(basic),loc ->
            begin
              Npkcontext.set_loc loc;
                List.map (fun x -> Ast.BasicDecl x,loc)
                     (normalize_basic_decl basic loc)
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
    ndp

  and normalize_body body  = match body with
    | SubProgramBody(subprog_decl,decl_part,block) ->
        let norm_subprog_decl =
          normalize_sub_program_spec subprog_decl ~addparam:true in
        Sym.enter_context ~desc:"SP body (locals)" gtbl;
        let return_type = return_type_of norm_subprog_decl in
        let norm_decl_part = normalize_decl_part decl_part in
        let norm_block = normalize_block ?return_type block in
        let ctx1 = Sym.exit_context gtbl in
        let ctx2 = Sym.exit_context gtbl in (* params *)
        Ast.SubProgramBody( norm_subprog_decl
                          , norm_decl_part
                          , ctx1
                          , ctx2
                          , norm_block)
    | PackageBody(name, package_spec, decl_part) ->
        let (nname,nspec,ctx) = normalize_package_spec
                                    (with_default package_spec
                                        (parse_package_specification name)
                                    )
        in
          Sym.set_current gtbl name;
          Sym.enter_context ~name ~desc:"Package body" gtbl;
          let ndp = normalize_decl_part decl_part in
          let norm_spec = (nname,nspec,ctx)
          in
          check_package_body_against_spec ~body:ndp ~spec:norm_spec;
          Sym.reset_current gtbl;
          let ctxb = Sym.exit_context gtbl in
          Ast.PackageBody(name, Some norm_spec, ctxb, ndp)




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
  g_extern := extern;

  let normalize_lib_item lib_item loc =
    Npkcontext.set_loc loc;
    match lib_item with
      | Spec(spec) -> Ast.Spec(normalize_spec spec)
      | Body(body) -> Ast.Body(normalize_body body)

  in

  let add_extern_spec spec =
    let add_extern_basic_decl (basic_decl, loc) =
      Npkcontext.set_loc loc;
      match basic_decl with
        | Ast.ObjectDecl(ident, t, (Ast.Variable | Ast.Constant),_) ->
            Sym.add_variable gtbl ident loc t
        | Ast.ObjectDecl(ident, t, Ast.StaticVal value,_) ->
            Sym.add_variable gtbl ident loc t ~value;
        | Ast.NumberDecl(ident, value) ->
            add_numberdecl ident value loc
        | Ast.SpecDecl _ -> ()

    in match spec with
      | Ast.SubProgramSpec _ -> ()
      | Ast.PackageSpec(name, basic_decls,_) ->
          Sym.set_current gtbl name;
          Sym.enter_context ~name ~desc:"Package spec (extern)" gtbl;
          List.iter add_extern_basic_decl basic_decls;
          Sym.reset_current gtbl;
          ignore (Sym.exit_context gtbl);
          Sym.add_with gtbl name

  in

  let normalize_context context =
    List.fold_left (fun ctx item -> match item with
      | With(nom, spec) ->
          if (not (Sym.is_with gtbl nom)) then
          begin
            let (norm_spec, loc) = match spec with
              | None   -> parse_extern_specification nom
              | Some _ -> Npkcontext.report_error
                  "Ada_normalize.normalize_context"
                    "internal error : spec provided"
            in
              add_extern_spec norm_spec;
              Ast.With(nom, loc, Some(norm_spec, loc))::ctx
          end
          else ctx
      | UseContext n  -> Sym.add_use gtbl n; ctx
    ) [] context
  in

  let (context,lib_item,loc) = compil_unit in
  let norm_context = normalize_context context in
  let norm_lib_item = normalize_lib_item lib_item loc in
    Npkcontext.forget_loc ();
    (norm_context
    ,norm_lib_item
    ,loc
    )
