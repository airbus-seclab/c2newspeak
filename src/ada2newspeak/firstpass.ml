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
  email: jasmine . duchon AT free . fr

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org
*)

module C   = Cir
module N   = Newspeak
module Nat = Newspeak.Nat
module A   = Syntax_ada
module T   = Ada_types
module Sym = Symboltbl

open Ast

let make_offset t index base size =
  C.Binop ( N.MultI
          , C.Binop ( N.MinusI
                    , T.check_exp t index
                    , base
                    )
          , size
          )

(** Builds a string from a name *)
let string_of_name = Ada_utils.name_to_string

let concat_resolved_name sc  n = match sc with
  | Sym.Lexical      ->        n
  | Sym.In_package p -> (p ^ "." ^ n)

let translate_resolved_name sc n = match sc with
  | Sym.Lexical      -> C.Local         n
  | Sym.In_package p -> C.Global (p ^ "." ^ n)

(**
 * A boolean flip flop.
 *)
let (extern, do_as_extern) =
  let ext = ref false in
    ((fun _ -> !ext),
     (fun f arg -> ext := true; f arg ; ext := false))

let translate_nat i =
  C.Const(C.CInt i)

let translate_int x =
  translate_nat (N.Nat.of_int x)

(**
 * Main translating function.
 * Takes an Ada program as and returns a CIR tree.
 *)
let translate compil_unit =

  let fun_decls = Hashtbl.create 10 in
  let globals   = Hashtbl.create 10 in
  let init      = ref []            in
  let curpkg    = ref None          in

  let normalize_ident ident package extern =
    if extern then (package, ident)
              else (None   , ident)
  in

  let translate_name (pack,id) =
    let tr_name = match (extern(), pack, !curpkg) with
    | true, Some p, _      -> [p;id]
    | true, None  , _      ->   [id]
    | false, _    , Some p -> [p;id]
    | false, _    , None   ->   [id]
    in
      string_of_name tr_name
  in


  (** Used to generate temporary variables. *)
  let temp =
    object (s)
        val mutable count = 0

        (**
         * Build a fresh identifier.
         * Several calls will yield "tmp0", "tmp1", and so on.
         *)
        method private new_id =
            let res = count in
            count <- count + 1;
            "tmp" ^ (string_of_int res)

        (**
         * Create a new temporary variable.
         * It will have the specified location and type.
         * The return value is a triplet of :
         *   - an identifier
         *   - a declaration ([C.Decl])
         *   - a CIR lvalue
         *
         * /!\ Side-effects : this method
         *   - alters the internal state of the [temp] object
         *   - calls [add_var] to register this variable
         *)
        method create loc t =
            let id = s#new_id in
              let decl = (C.Decl (T.translate t, id), loc) in
                (id, decl, C.Local id)
    end
  in

  (* fonctions de traductions *)
  let translate_subtyp_option subtyp = match subtyp with
    | None -> C.Void
    | Some(subtyp) -> T.translate subtyp

  in

  let rec translate_if_exp e_cond e_then e_else =
    let loc = Npkcontext.get_loc () in
    let (tmp, decl, vid) = temp#create loc T.boolean in
    let instr_if = If (e_cond,
           [Assign(Var (Sym.Lexical, tmp, snd e_then), e_then),loc],
           [Assign(Var (Sym.Lexical, tmp, snd e_else), e_else),loc])
    in let tr_instr_if =
        translate_block [(instr_if,loc)]
    in
      (C.BlkExp (decl::tr_instr_if,
                 C.Lval (vid, T.translate T.boolean), false),
       T.boolean)

  and translate_and e1 e2 =
    let loc = Npkcontext.get_loc () in
    let (tr_e2,_ ) = translate_exp e2 in
    let (_, decl, vid) = temp#create loc T.boolean in
    let assign = C.Set (vid, T.translate T.boolean, tr_e2) in
    let tr_ifexp = fst (translate_if_exp e1
                                         e2
                                         (CBool false,T.boolean)) in
      C.BlkExp (decl::(assign,loc)::[C.Exp tr_ifexp,loc]
      , C.Lval (vid, T.translate T.boolean), false), T.boolean

  and translate_or e1 e2 =
    let loc = Npkcontext.get_loc () in
    let (tr_e2,_ ) = translate_exp e2 in
    let (_, decl, vid) = temp#create loc T.boolean in
    let assign = C.Set (vid, T.translate T.boolean, tr_e2) in
    let tr_ifexp = fst (translate_if_exp e1
                                         (CBool true,T.boolean)
                                         e2) in
      C.BlkExp (decl::(assign,loc)::[C.Exp tr_ifexp,loc]
      , C.Lval (vid, T.translate T.boolean), false), T.boolean

  and translate_binop op e1 e2 =
    let (c1, c2, typ) =
      let (c1, _) = translate_exp e1 in
      let (c2, typ2) = translate_exp e2 in
      (c1, c2, typ2)
    in
      match (op,T.translate typ) with
      (* Numeric operations *)
      | Plus ,C.Scalar(N.Int   _) -> C.Binop(N.PlusI   , c1, c2),typ
      | Plus ,C.Scalar(N.Float n) -> C.Binop(N.PlusF  n, c1, c2),typ
      | Minus,C.Scalar(N.Int   _) -> C.Binop(N.MinusI  , c1, c2),typ
      | Minus,C.Scalar(N.Float n) -> C.Binop(N.MinusF n, c1, c2),typ
      | Mult ,C.Scalar(N.Int   _) -> C.Binop(N.MultI   , c1, c2),typ
      | Mult ,C.Scalar(N.Float n) -> C.Binop(N.MultF  n, c1, c2),typ
      | Div  ,C.Scalar(N.Int   _) -> C.Binop(N.DivI    , c1, c2),typ
      | Div  ,C.Scalar(N.Float n) -> C.Binop(N.DivF   n, c1, c2),typ
      | Rem  ,C.Scalar(N.Int   _) -> C.Binop(N.Mod     , c1, c2),typ

      (* Comparisons *)
      | Eq, C.Scalar t -> C.Binop (N.Eq t, c1, c2), T.boolean
      | Gt, C.Scalar t -> C.Binop (N.Gt t, c1, c2), T.boolean

      | And, C.Scalar _ -> translate_and e1 e2
      | Or , C.Scalar _ -> translate_or  e1 e2

      | (Power | Mod ) ,_ ->
          Npkcontext.report_error "Firstpass.translate_binop"
            "run-time operator not implemented"

      | _ -> Npkcontext.report_error "Firstpass.translate_binop"
            "invalid operator and argument"

  and translate_not exp =
        let (exp, _) = translate_exp exp
        in (C.Unop (Npkil.Not, exp), T.boolean)

  (** Returns ftyp & list of params *)
  and translate_subprogram_parameters params =
    let translate_parameter (_id, t, exp) =
        let (tr_exp, _) = translate_exp exp in
        ( T.translate t
        , T.check_exp t tr_exp
        )
    in
    List.split (
        List.map translate_parameter params
    )

  (** Translates a function call.  *)
  and translate_function_call fname arg_list rt =
    let (ftyp0, tr_params) = translate_subprogram_parameters arg_list in
    C.Call((ftyp0, T.translate rt), fname, tr_params), rt

  and translate_lv lv =
      match lv with
        | Var (sc,lv,t) ->
            let clv = translate_resolved_name sc lv in
            (clv, t)
        | ArrayAccess (lv, [expr]) ->
            let (x_lv,t_lv ) = translate_lv lv in
            let (x_exp,_) = translate_exp expr in
            let (tc, ti) = T.extract_array_types t_lv in
            let x_typ = T.translate tc in
            let size = C.size_of_typ x_typ in
            let base = T.extract_array_base t_lv in
            let index = translate_nat base in
            let offset = make_offset ti x_exp index
                                           (translate_int size) in
            C.Shift (x_lv, offset),tc
        | RecordAccess (lv, off_pos, tf) ->
            let (record, _) = translate_lv lv in
            let offset = translate_int off_pos in
            C.Shift (record, offset), tf
        | PtrDeref (lv, t) ->
            let (xlv, _) = translate_lv lv in
            let xt = T.translate t in
            C.Deref (C.Lval (xlv, xt), xt), t
        | ArrayAccess(_, _) -> failwith "matrix access"

  and translate_exp (exp,typ) :C.exp * T.t =
    match exp with
    | CFloat f -> C.Const(C.CFloat(f,string_of_float f)), T.std_float
    | CInt   i -> translate_nat i, typ
    | CChar  c -> translate_nat (Nat.of_int c), T.character
    | CBool  b -> translate_nat (Ada_utils.nat_of_bool b), T.boolean
    | Lval   l -> let (tlv,tp) = translate_lv l in
                  C.Lval (tlv,T.translate tp), typ
    | Not    exp              -> translate_not exp
    | Binary(binop,exp1,exp2) ->
        let (bop, rtyp) = translate_binop binop exp1 exp2 in
        (T.check_exp typ bop), rtyp
    | CondExp(e1,e2,e3)       -> translate_if_exp e1 e2 e3
    | FunctionCall(sc, name, arg_list, rt) ->
        let fname = C.Fname (concat_resolved_name sc name) in
        translate_function_call fname arg_list rt
    | AddressOf (lv, _) ->
        let (lv', tlv) = translate_lv lv in
        C.AddrOf(lv', T.translate tlv), T.system_address

  (**
   * Make a C assignment.
   *)
  and make_affect id exp typ_lv loc =
    Npkcontext.set_loc loc;
    let typ = T.translate typ_lv in
    let checked_exp = T.check_exp typ_lv exp in
    Npkcontext.print_debug ("Assign : LV  = "^T.print typ_lv);
    (C.Set(id,typ,checked_exp),loc)

  (**
   * Translate a [Syntax_ada.Assign].
   *)
  and translate_affect lv exp loc =
    let (tr_lv,subtyp_lv) = translate_lv lv in
    let (tr_exp,_) = translate_exp exp in
    make_affect tr_lv tr_exp subtyp_lv loc

  (**
   * Translate a [Syntax_ada.block].
   *)
  and translate_block block = match block with
    | [] -> []
    | (instr,loc)::r ->
        begin
        Npkcontext.set_loc loc;
         match instr with
           | Return(exp) ->
               translate_block
                 ((Assign( Var ( Sym.Lexical, Params.ret_ident, snd exp)
                               , exp
                               ),loc)
                  ::(ReturnSimple,loc)::r)
           | ReturnSimple ->
               let tr_reste =
                 match r with
                   | [] -> []
                   | (_,next_loc)::_ ->
                       Npkcontext.set_loc next_loc;
                       Npkcontext.report_warning
                         "Firstpass.translate_block"
                          "Code after return statement can't be reached";
                       Npkcontext.set_loc loc;
                       translate_block r
             in
                 (C.Goto Params.ret_lbl, loc)::tr_reste
           | Exit -> (C.Goto Params.brk_lbl, loc)::(translate_block r)
           | Assign(lv,exp) ->
               (translate_affect lv exp loc)::(translate_block r)
           | If(condition,instr_then,instr_else) ->
               let (tr_exp, typ) = translate_exp condition in
                 if (not (T.is_boolean typ ))then begin
                   Npkcontext.report_error "Firstpass.translate_block"
                                         "expected a boolean type for condition"
                 end;
                 let tr_then = translate_block instr_then in
                 let tr_else = translate_block instr_else in
                     (C.build_if loc (tr_exp, tr_then, tr_else))
                   @ (translate_block r)
           | Loop(NoScheme, body) ->
               let tr_body = translate_block body in
                 (C.Block([C.Loop(tr_body), loc], Some (Params.brk_lbl,[])),loc)
                 ::(translate_block r)
           | Loop(While(cond), body) ->
               translate_block
                 ((Loop(NoScheme,(If(cond,[],[Exit,loc]),loc)::body),
                   loc)::r)
           | ProcedureCall (sc, name, args) -> begin
               let fname = C.Fname (concat_resolved_name sc name) in
                 let (ftyp0, tr_args) = translate_subprogram_parameters args in
                 let ftyp = ftyp0, C.Void in
                   (C.Exp(C.Call(ftyp, fname, tr_args)), loc)
                    ::(translate_block r)
             end

           | Case (e, choices, default) ->
                   (C.Switch(fst(translate_exp e),
                         (List.map (function exp,block ->
                             let (value,typ) = translate_exp exp in
                             ( value
                             , C.scalar_of_typ (T.translate typ)
                             )
                             , translate_block block
                         ) choices),
                           translate_block
                             (Ada_utils.with_default default [])
                           ),loc)::(translate_block r)
            | Block (dp, blk) ->
                   let (t_dp, init) = translate_declarative_part dp in
                   let res = (C.Block ((t_dp @ (translate_block (init @ blk))),
                                  None),loc) in
                   let r = res::(translate_block r) in
                   r
            end

  and translate_param param = match param.mode with
      | A.In    -> T.translate param.param_type
      |   A.Out
      | A.InOut -> C.Scalar N.Ptr

  and translate_param_list param_list =
    (List.map translate_param param_list)
  and add_params subprog_spec =
    let param_list =
      match subprog_spec with
        | Function (_, param_list, _) -> param_list
        | Procedure(_, param_list)    -> param_list
    in
    let params = List.map (fun x -> x.formal_name) param_list in
    (params, (Params.ret_ident, params))

  and translate_sub_program_spec subprog_spec =
    let (param_list, return_type) =
      match subprog_spec with
        | Function(_,param_list,return_type) ->
            (param_list, Some(return_type))
        | Procedure(_,param_list) ->
            (param_list, None)
    in let params_typ = translate_param_list param_list in
      (params_typ, translate_subtyp_option return_type)

  and add_fundecl subprogspec =
    translate_sub_program_spec subprogspec

  and translate_basic_declaration basic loc = match basic with
    | ObjectDecl (id,t,_,blkopt) ->
        [C.Decl (T.translate t, id), loc],(match blkopt with
                                           | Some b -> b
                                           | None   -> []
                                          )
    | SpecDecl _ -> Npkcontext.report_error
        "Firstpass.translate_basic_declaration"
          ( "declaration de sous-fonction, sous-procedure ou "
          ^ "sous package non implemente")
    | NumberDecl _ -> [],[]

  and translate_declarative_item (item,loc) =
    Npkcontext.set_loc loc;
    match item with
      | BasicDecl basic -> translate_basic_declaration basic loc
      | BodyDecl _ -> Npkcontext.report_error "Firstpass.translate_block"
            "sous-fonction, sous-procedure ou sous package non implemente"

  and translate_declarative_part decl_part =
    List.fold_left (fun (decls,iblk) dp ->
      let (ndecls,niblk) = translate_declarative_item dp
      in
      (decls @ ndecls),
      (iblk  @ niblk )
    ) ([],[]) decl_part

  and add_funbody subprogspec decl_part block loc =
    let name = match subprogspec with
      | Function (n,_,_) -> n
      | Procedure(n,_)   -> n in
    let (_, (ret_id, args_ids)) = add_params subprogspec in
    let (body_decl,init) = translate_declarative_part decl_part in
    let ftyp = add_fundecl subprogspec in
    let body = translate_block (init @ block) in
    let mangle_sname = function
      | []       -> failwith "unreachable @ firstpass:mangle_sname"
      | x::[]    -> None  , x
      | x::y::[] -> Some x, y
      | _        -> Npkcontext.report_error "mangle_sname"
                      "chain of selected names is too deep"
    in
    let body = (C.Block (body_decl @ body, Some (Params.ret_lbl,[])), loc)::[] in
      Hashtbl.replace fun_decls (translate_name (mangle_sname name))
                      (ret_id, args_ids, ftyp, body);
  in

  let add_global loc tr_typ i x =
    let name = normalize_ident x (!curpkg) (extern ()) in
    let tr_name = translate_name name in
    let storage = match i with
      | None -> Npkil.Declared false
      | Some init_stmt ->
          begin
            init := (translate_block init_stmt) @ (!init);
            Npkil.Declared true
          end
    in
    Hashtbl.add globals tr_name (tr_typ, loc, storage)
  in

  let rec translate_global_basic_declaration (basic, loc) =
    match basic with
      | ObjectDecl(ident, subtyp, _, init_o) ->
          let tr_typ = T.translate subtyp in
          let init =
            begin
            match (init_o, extern ()) with
              | (_, true) | (None, _) -> None
              | (Some blk, false) ->
                  Some blk
            end
          in
          add_global loc tr_typ init ident
      | SpecDecl spec -> translate_spec spec false
      | NumberDecl _ -> ()

  and translate_global_decl_item (item,loc) =
    Npkcontext.set_loc loc;
    match item with
      | BasicDecl(basic) ->
          translate_global_basic_declaration (basic, loc)
      | BodyDecl(body) -> translate_body body false loc

  and translate_spec spec glob = match spec with
    | SubProgramSpec(subprog_spec) ->
        ignore (add_fundecl subprog_spec )
    | PackageSpec (nom, basic_decl_list) ->
        if (not glob) then begin
    Npkcontext.report_error "Firstpass.translate_spec"
            "declaration de sous package non implemente"
        end;
        curpkg := Some nom;
        List.iter translate_global_basic_declaration basic_decl_list;
        curpkg := None

  and translate_body body glob loc =
    Npkcontext.set_loc loc;
    match (body, glob) with
      | (SubProgramBody(subprog_decl,decl_part, block), _) ->
          add_funbody subprog_decl decl_part block loc
      | PackageBody(name, package_spec, decl_part), true ->
          curpkg := Some name;
          (match package_spec with
             | None -> ()
             | Some(_, basic_decls) ->
                 begin
                   List.iter translate_global_basic_declaration basic_decls;
                 end
          );
          List.iter translate_global_decl_item decl_part;

      | PackageBody _, false -> Npkcontext.report_error
          "Firstpass.translate_body"
            "declaration de sous package non implemente"

  in

  let translate_library_item lib_item loc =
    Npkcontext.set_loc loc;
    match lib_item with
      | Body body -> translate_body body true loc
      | Spec _    -> Npkcontext.report_error
            "Firstpass.translate_library_item"
            "Rien a faire pour les specifications"
  in

  let rec translate_context =
    List.iter (function
      | With(_, loc, spec) ->
          Npkcontext.set_loc loc;
          (match spec with
            | Some(spec, _loc) ->
                translate_spec spec true
            | None -> Npkcontext.report_error
                "Firstpass.translate_context"
                  "internal error : no specification provided")
    );
  in

  let normalized_compil_unit = Normalize.normalization compil_unit false in
  let (ctx, lib_item, loc) = normalized_compil_unit in
    Npkcontext.set_loc loc;
    do_as_extern translate_context ctx;
    translate_library_item lib_item loc;
    Npkcontext.forget_loc ();
    { C.globals = globals; C.init = !init; C.fundecs = fun_decls }

