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
module Nat = Newspeak.Nat
module A   = Syntax_ada
module T   = Ada_types
module Sym = Symboltbl

open Ast

(**
 * Symbols.
 * A symbol may represent a variable or an enumeration litteral.
 * The boolean parameter indicates whether :
 *   - the variable is global or local (true means global) in the cases
 *     variable and enum.
 *   - if the subprogram is internal or external (external : true)
 *   - for the variables : the last boolean parameter indicates whether a
 *     variable is read-only.
 *)
type symb =
  | VarSymb  of C.lv * Ada_types.t * bool * bool (** name, type, global?, ro? *)
  | EnumSymb of C.exp   * Ada_types.t * bool (** TODO, typename, ro? *)
  | FunSymb  of C.funexp * Ast.sub_program_spec * bool * C.ftyp (** XXX *)
  | NumberSymb of T.data_t*bool (** XXX *)

type qualified_symbol = symb*C.typ*Newspeak.location

(** Promotes an identifier to a name *)
let ident_to_name ident = (None, ident)

let make_offset index size =
  C.Binop (Newspeak.MultI
          , index
          , size
          )

(** Builds a string from a name *)
let string_of_name = Ada_utils.name_to_string

let unbox_resolved sc x = match sc with
  | Sym.Lexical      -> None,x
  | Sym.In_package p -> Some p,x

let concat_resolved_name sc  n = match sc with
  | Sym.Lexical      ->        n
  | Sym.In_package p -> (p^"."^n)

let translate_resolved_name sc n = match sc with
  | Sym.Lexical      -> C.Local         n
  | Sym.In_package p -> C.Global (p^"."^n)

(**
 * A boolean flip flop.
 *)
let (extern, do_as_extern) =
  let ext = ref false in
    ((fun _ -> !ext),
     (fun f arg -> ext := true; f arg ; ext := false))

let translate_saved_context ctx =
  List.map (fun (id,t,loc) ->
    C.Decl (T.translate t, id), loc
  ) (Sym.extract_variables ctx)

(**
 * Main translating function.
 * Takes an Ada program as and returns a CIR tree.
 *)
let translate compil_unit =

  (*
   * Global hashtables and references.
   * /!\ Side-effects !
   *)

  (** Symbol table for lexically-scoped variables. *)
  let symbtbl   = Hashtbl.create 100

  (* Function declarations. *)
  and fun_decls = Hashtbl.create 100

  (* FIXME Global (?) symbol table. Strangely used. *)
  and globals   = Hashtbl.create 100

  and init      = ref []

  and gtbl = Sym.create () in

  let find_all_symb x = Hashtbl.find_all symbtbl x
  and mem_symb x = Hashtbl.mem symbtbl x
  in

  let (add_global_init, get_global_init) =
    let global_init = Hashtbl.create 0
    in
    (fun id exp -> Npkcontext.print_debug ("add_global_init ("^id^")");
                   Hashtbl.add global_init id exp)
    ,
    (fun id ->
       let str="get_global_init ("^id^") : " in
       try
         let r = Hashtbl.find global_init id in
         Npkcontext.print_debug (str^"got value");
         Some r
       with Not_found -> Npkcontext.print_debug (str^"no value") ;None
    )
  in

  let translate_int i = C.Const(C.CInt i) in
  let trans_int x = translate_int (Newspeak.Nat.of_int x) in

  (* fonctions de traduction des noms*)
  (* on ajoute le nom du package courant dans le cas ou on
     etudie les declarations internes *)
  (* fonction appelee dans add_fundecl, add_funbody, add_global *)
  let translate_name (pack,id) =
    let tr_name =
        if extern() then pack,            id
                             else (Sym.current gtbl), id
    in
      string_of_name tr_name
  in

  (* gestion de la table de symboles *)

  (* declaration d'une variable locale *)
  let add_var loc st ident ~deref ~ro =
    let x = ident_to_name ident in
      (if Hashtbl.mem symbtbl x then
         match Hashtbl.find symbtbl x with

           (* si la declaration precedente est globale, on peut
              redefinir x*)
           | (VarSymb   (_,_, true,_),_,_)
           | (NumberSymb(_,true),_,_)
           | (EnumSymb  (_,_,true),_,_)
           | (FunSymb    _,_,_)              -> ()

           (* sinon la declaration est locale,
              cela cause une erreur *)
           | ((VarSymb(_)|EnumSymb(_)|NumberSymb (_)),_,_)->
               Npkcontext.report_error "Firstpass.add_var"
                (* FIXME should be warning *)
                 ("Variable "^(string_of_name x)
                  ^" hides former definition.")
      );
      let tr_typ = T.translate st in
      let (lv, typ_cir) =
        if deref
        then (C.Deref(C.Lval(C.Local ident, tr_typ), tr_typ),
                C.Scalar Newspeak.Ptr)
        else (C.Local ident, tr_typ)
      in
        Hashtbl.add symbtbl x (VarSymb (lv, st, false, ro),
                               typ_cir, loc)

  (* declaration d'un nombre local *)
  and add_number loc value lvl ident =
    let x =  Normalize.normalize_ident
      ident (Sym.current gtbl) (extern ()) in
      (if Hashtbl.mem symbtbl x then
         match Hashtbl.find symbtbl x with

           (* erreur : declaration de meme niveau deja existante *)
           | (VarSymb(_, _, global,_),_,_)
           | (EnumSymb(_,_, global),_,_)
           | (NumberSymb(_,global),_,_) when global=lvl ->
               Npkcontext.report_error "Firstpass.add_number"
                 ("conflict : "^(string_of_name x)
                  ^" already declared")
           | (FunSymb _,_,_) when lvl ->
               Npkcontext.report_error "Firstpass.add_number"
                 ("conflict : "^(string_of_name x)
                  ^" already declared")

           (* les declarations ne sont pas de meme niveau *)
           | ((VarSymb(_) | EnumSymb(_)
              | NumberSymb(_) | FunSymb _),_,_) -> ()
      );
      let typ_cir = match value with
        | T.IntVal _   -> T.translate T.integer
        | T.FloatVal _ -> T.translate T.std_float
        | T.BoolVal _ ->
            Npkcontext.report_error
              "Firstpass.add_number"
              "internal error : number cannot have Enum val"
      in
        Hashtbl.add symbtbl x
          (NumberSymb(value, lvl), typ_cir, loc)



  and add_global loc typ tr_typ i ro x =
    let name = Normalize.normalize_ident x (Sym.current gtbl) (extern ()) in

    let tr_name = translate_name name in
      (if mem_symb name
       then
         (List.iter
            (fun symb -> match symb with
                 (* il y a une declaration de variable
                    ou de nombre globale *)
               | (NumberSymb(_, true),_,_)
               | (VarSymb(_, _, true,_),_,_) ->
                   Npkcontext.report_error "Firstpass.add_global"
                     ("conflict : "^x^" already declared")

               (* declaration d'un symbol d'enumeration global *)
               | (EnumSymb(_, _, true),_,_) ->
                   Npkcontext.report_error "Firstpass.add_global"
                     ("conflict : "^x^" already declared")

               (* fonction interne *)
               | (FunSymb (_, Function(_, _, _), false, _), _, _) ->
                   Npkcontext.report_error "Firstpass.add_enum"
                     ("conflict : "^x^" already declared")

               | ((EnumSymb _ |VarSymb _
                  | FunSymb _ |NumberSymb(_)),_,_) -> ())
            (find_all_symb name)));

      let storage =
  match i with
    | None -> Npkil.Declared false
    | Some (e, t) ->
        init := (C.Set (C.Global tr_name, t, e), loc)::!init;
        Npkil.Declared true
      in

  Hashtbl.add globals tr_name (tr_typ, loc, storage);
  Hashtbl.add symbtbl name
          (VarSymb (C.Global(tr_name), typ, true, ro),
           tr_typ, loc);

  and remove_symb x =
    Hashtbl.remove symbtbl (ident_to_name x) in

  let remove_formals args =
    remove_symb Params.ret_ident;
    List.iter remove_symb args
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
            count<-count+1;
            "tmp"^(string_of_int res)

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
              add_var loc t id false false;
              Sym.add_variable gtbl id loc t;
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
           [Assign(Lval (Sym.Lexical, tmp, snd e_then), e_then, false),loc],
           [Assign(Lval (Sym.Lexical, tmp, snd e_else), e_else, false),loc])
    in let tr_instr_if =
        translate_block [(instr_if,loc)]
    in
      remove_symb tmp;
      (C.BlkExp (decl::tr_instr_if,
                 C.Lval (vid, T.translate T.boolean), false),
       T.boolean)

  and translate_and e1 e2 =
    let loc = Npkcontext.get_loc () in
    let (tr_e2,_ ) = translate_exp e2 in
    let (tmp, decl, vid) = temp#create loc T.boolean in
    let assign = C.Set (vid, T.translate T.boolean, tr_e2) in
    let tr_ifexp = fst (translate_if_exp e1
                                         e2
                                         (CBool false,T.boolean)) in
      remove_symb tmp;
      C.BlkExp (decl::(assign,loc)::[C.Exp tr_ifexp,loc]
      , C.Lval (vid, T.translate T.boolean), false), T.boolean

  and translate_or e1 e2 =
    let loc = Npkcontext.get_loc () in
    let (tr_e2,_ ) = translate_exp e2 in
    let (tmp, decl, vid) = temp#create loc T.boolean in
    let assign = C.Set (vid, T.translate T.boolean, tr_e2) in
    let tr_ifexp = fst (translate_if_exp e1
                                         (CBool true,T.boolean)
                                         e2) in
      remove_symb tmp;
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
      | Plus ,C.Scalar(Newspeak.Int   _)->C.Binop(Newspeak.PlusI   , c1, c2),typ
      | Plus ,C.Scalar(Newspeak.Float n)->C.Binop(Newspeak.PlusF  n, c1, c2),typ
      | Minus,C.Scalar(Newspeak.Int   _)->C.Binop(Newspeak.MinusI  , c1, c2),typ
      | Minus,C.Scalar(Newspeak.Float n)->C.Binop(Newspeak.MinusF n, c1, c2),typ
      | Mult ,C.Scalar(Newspeak.Int   _)->C.Binop(Newspeak.MultI   , c1, c2),typ
      | Mult ,C.Scalar(Newspeak.Float n)->C.Binop(Newspeak.MultF  n, c1, c2),typ
      | Div  ,C.Scalar(Newspeak.Int   _)->C.Binop(Newspeak.DivI    , c1, c2),typ
      | Div  ,C.Scalar(Newspeak.Float n)->C.Binop(Newspeak.DivF   n, c1, c2),typ
      | Rem  ,C.Scalar(Newspeak.Int   _)->C.Binop(Newspeak.Mod     , c1, c2),typ

      (* Comparisons *)
      | Eq, C.Scalar t -> C.Binop (Newspeak.Eq t, c1, c2), T.boolean
      | Gt, C.Scalar t -> C.Binop (Newspeak.Gt t, c1, c2), T.boolean

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
    let ftyp = ftyp0, T.translate rt in
    try (C.Call(ftyp, fname, tr_params), rt)
    with
      | Invalid_argument _ -> Npkcontext.report_error
                            "Firstpass.translate_function_call"
                            "wrong number of arguments"

  and translate_var scope name ty =
    let n    = translate_resolved_name scope name in
    let xtyp = T.translate ty in
    C.Lval (n, xtyp), ty

  and translate_lv lv _write _trans_exp =
      match lv with
        | Lval (sc,lv,t) ->
            let clv = translate_resolved_name sc lv in
            (clv, t)

        (*Assignation dans un tableau*)
        | ArrayAccess (lv, expr) ->
            let (x_lv,t_lv ) = translate_lv lv _write _trans_exp in
            let (x_exp,t) = translate_exp expr in
            let x_typ = T.translate t in
            let size = C.size_of_typ x_typ in
            let offset = make_offset x_exp (trans_int size) in
            let offset' = T.check_exp (t_lv) offset in
            C.Shift (x_lv, offset'),t

  and translate_exp (exp,typ) :C.exp*T.t=
    match exp with
    | CFloat f -> C.Const(C.CFloat(f,string_of_float f)), T.std_float
    | CInt   i -> translate_int i, typ
    | CChar  c -> translate_int (Nat.of_int c), T.character
    | CBool  b -> translate_int (Ada_utils.nat_of_bool b), T.boolean
    | Var    (scope,name,ty)  -> translate_var scope name ty
    | Not    exp              -> translate_not exp
    | Binary(binop,exp1,exp2) ->
        let (bop, rtyp) = translate_binop binop exp1 exp2 in
        (T.check_exp typ bop), rtyp
    | CondExp(e1,e2,e3)       -> translate_if_exp e1 e2 e3
    | ArrayValue  (sc, name, arg_list, t) ->
        let index = match arg_list with
          | [x] -> x
          | _ -> failwith "array value : unexpected matrix"
        in
        let arr = translate_resolved_name sc name in
        let (ex_index, t_index) = (translate_exp index) in
        let ctyp = T.translate t_index in
        let offset = make_offset ex_index
                                 (trans_int (C.size_of_typ ctyp))
        in
        C.Lval (C.Shift (arr, offset), ctyp), t
    | FunctionCall(sc, name, arg_list, rt) ->
        let fname = C.Fname (concat_resolved_name sc name) in
        translate_function_call fname arg_list rt

  (**
   * Make a C assignment.
   *)
  and make_affect id exp typ_lv loc =
    let typ = T.translate typ_lv in
    let checked_exp = T.check_exp typ_lv exp in
    (C.Set(id,typ,checked_exp),loc)

  (**
   * Translate a [Syntax_ada.Assign].
   *)
  and translate_affect lv exp loc unchecked =
    let (tr_lv,subtyp_lv) = translate_lv lv (not unchecked) translate_exp in
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
                 ((Assign( Lval ( Sym.Lexical, Params.ret_ident, snd exp)
                                , exp
                                , false),loc)
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
           | Assign(lv,exp,unchecked) ->
               (translate_affect lv exp loc unchecked)::(translate_block r)
           | If(condition,instr_then,instr_else) ->
               let (tr_exp, typ) = translate_exp condition in
                 if (not (T.is_boolean typ ))then begin
                   Npkcontext.report_error "Firstpass.translate_block"
                                         "expected a boolean type for condition"
                 end;
                 let tr_then = translate_block instr_then in
                 let tr_else = translate_block instr_else in
                   (C.build_if loc (tr_exp, tr_then, tr_else))
                   @(translate_block r)
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
                 let (ftyp0, tr_params) = translate_subprogram_parameters args in
                 let ftyp = ftyp0, C.Void in
                   (C.Exp(C.Call(ftyp, fname, tr_params)), loc)
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
            | Block (dp, ctx, blk) ->
                          Sym.push_saved_context gtbl ctx;
                          translate_declarative_part dp;
                          let t_ctx = translate_saved_context ctx in
                          let res = (C.Block ((t_ctx@(translate_block blk)),
                                         None),loc) in
                          let r = res::(translate_block r) in
                          remove_declarative_part dp;
                          ignore (Sym.exit_context gtbl);
                          r
            end

  and translate_param param = match param.mode with
      | A.In    -> T.translate param.param_type
      |   A.Out
      | A.InOut -> C.Scalar Newspeak.Ptr

  and add_param loc param =
    let (deref,ro) = match param.mode with
      | A.In    -> (false, true)
      |   A.Out
      | A.InOut -> (true, false)
    in
        add_var loc
                param.param_type
                param.formal_name
                deref
                ro;
        (param.formal_name,
         param.formal_name)

    (* prend une liste de parametres en argument
       et renvoie liste de typ *)
  and translate_param_list param_list =
    (List.map translate_param param_list)
  and add_params subprog_spec loc =
    let param_list =
      match subprog_spec with
        | Function(_, param_list, return_type) ->
            Sym.add_variable gtbl Params.ret_ident loc return_type;
            add_var loc return_type Params.ret_ident false false;
            param_list
        | Procedure(_, param_list) ->
            param_list
    in
    let (param_names, vids) =
      (List.split (List.map (add_param loc) param_list))
    in
      (param_names, (Params.ret_ident, vids))


  and translate_sub_program_spec subprog_spec =
    let (name, param_list, return_type) =
      match subprog_spec with
        | Function(name,param_list,return_type) ->
            (name, param_list, Some(return_type))
        | Procedure(name,param_list) ->
            (name, param_list, None)
    in let params_typ = translate_param_list param_list in
      (name, (params_typ, translate_subtyp_option return_type))

  and add_fundecl subprogspec loc =
    let check_ident name =
     if Hashtbl.mem symbtbl name
     then
       let list_ident = Hashtbl.find_all symbtbl name in
         List.iter
           (fun symb -> match symb with
              | (FunSymb (_,_,extern', _),_,_)
                  when extern'= extern() ->
                  Npkcontext.report_error "Firstpass.add_fundecl"
                    ("conflict : "^(string_of_name name)
                     ^" already declared")
              | (FunSymb _, _, _) -> ()
              | ((VarSymb(_)|NumberSymb(_)),_,_) ->
                  Npkcontext.report_error "Firstpass.add_fundecl"
                    ("conflict : "^(string_of_name name)
                     ^" already declared")
              | (EnumSymb(_,etyp,_),_,_) ->
                  begin
                    match subprogspec with
                      | Function(_, [], rtyp)
                          when etyp = rtyp ->
                          Npkcontext.report_error "Firstpass.add_fundecl"
                            ("conflict : "^(string_of_name name)
                              ^" already declared")
                      | _ -> ()
                  end)
           list_ident
    in
    let (name, ftyp) = translate_sub_program_spec subprogspec in
      check_ident name;
      Hashtbl.add symbtbl name
        (FunSymb (C.Fname(translate_name name), subprogspec,
                  extern(), ftyp), C.Fun, loc);
      ftyp

  and translate_basic_declaration basic loc = match basic with
    | ObjectDecl(ident, st, const) ->
        let read_only = const <> Variable in
        add_var loc st ident false read_only
    | SpecDecl _ -> Npkcontext.report_error
        "Firstpass.translate_basic_declaration"
          ("declaration de sous-fonction, sous-procedure ou "
           ^"sous package non implemente")
    | UseDecl (use_clause) -> Sym.add_use gtbl use_clause
    | NumberDecl(ident, v) -> add_number loc v false ident

  and translate_declarative_item (item,loc) =
    Npkcontext.set_loc loc;
    match item with
      | BasicDecl(basic) -> translate_basic_declaration basic loc
      | BodyDecl _ -> Npkcontext.report_error "Firstpass.translate_block"
            "sous-fonction, sous-procedure ou sous package non implemente"

  and translate_declarative_part decl_part =
    List.iter translate_declarative_item decl_part

  and remove_basic_declaration basic = match basic with
    | ObjectDecl(ident, _, _) -> remove_symb ident
    | SpecDecl(_) -> Npkcontext.report_error
        "Firstpass.remove_basic_declaration"
          ("declaration de sous-fonction, sous-procedure ou "
           ^"sous package non implemente")
    | UseDecl _ -> ()
    | NumberDecl(ident, _) -> remove_symb ident

  and remove_declarative_item (item,_) = match item with
    | BasicDecl(basic) -> remove_basic_declaration basic
    | BodyDecl(_) -> ()

  and remove_declarative_part decl_part =
    List.iter remove_declarative_item decl_part

  and add_funbody subprogspec decl_part ctx_dp ctx_param block loc =
    let search_spec name =
        try
          let symb =
            (List.find
               (fun symb ->
                  match symb with
                    | (FunSymb (_, spec, false, _),_,_) ->
                        spec = subprogspec
                    | ((FunSymb _ | VarSymb _
                    | EnumSymb _ | NumberSymb _),_,_) -> false)
               (Hashtbl.find_all symbtbl name)
               )
          in
            match symb with
              | (FunSymb (_, _, _, ftyp), C.Fun, _) -> ftyp
              | _ ->
                  Npkcontext.report_error "Firstpass.add_funbody.seach_spec"
                    "internal error : typ is not a fun typ"
        with Not_found -> add_fundecl subprogspec loc
    in
    let name = match subprogspec with
      | Function (n,_,_) -> n
      | Procedure(n,_)   -> n in

    let (params, (ret_id, args_ids)) = add_params subprogspec loc in
    Sym.push_saved_context gtbl ctx_param;
    Sym.push_saved_context gtbl ctx_dp;
    translate_declarative_part decl_part;

    let ftyp = search_spec name in
    let body_decl = translate_saved_context ctx_dp in
    let body = translate_block block in
    let body = (C.Block (body_decl@body, Some (Params.ret_lbl,[])), loc)::[] in
      Hashtbl.replace fun_decls (translate_name name)
                      (ret_id, args_ids, ftyp, body);
      remove_formals params;
      remove_declarative_part decl_part;
      ignore (Sym.exit_context gtbl);
      ignore (Sym.exit_context gtbl)

  in

  let rec translate_global_basic_declaration (basic, loc) =
    match basic with
      | ObjectDecl(ident, subtyp, const) ->
          let init = get_global_init ident in
          let read_only = const <> Variable in
          let tr_typ = T.translate subtyp in
          let init =
            match (init, extern ()) with
                    | (_, true) | (None, _) -> None
                    | (Some exp, false) ->
            let (e, _) = translate_exp exp in
              Some (e, tr_typ)
             in
            add_global loc subtyp tr_typ init read_only ident
      | UseDecl x -> Sym.add_use gtbl x
      | SpecDecl spec -> translate_spec spec loc false
      | NumberDecl(ident, v) -> add_number loc v true ident

  (* quand cette fonction est appelee, on est dans le corps d'un
     package *)
  and translate_global_decl_item (item,loc) =
    Npkcontext.set_loc loc;
    match item with
      | BasicDecl(basic) ->
          translate_global_basic_declaration (basic, loc)

      | BodyDecl(body) -> translate_body body false loc

  and translate_spec spec loc glob = match spec with

    | SubProgramSpec(subprog_spec) ->
        ignore (add_fundecl subprog_spec loc)

    | PackageSpec (nom, basic_decl_list, _ctx, init) ->
        if (not glob) then begin
    Npkcontext.report_error "Firstpass.translate_spec"
            "declaration de sous package non implemente"
        end;
        Sym.set_current gtbl nom;
        List.iter (fun (id, exp) -> add_global_init id exp) init;
        List.iter translate_global_basic_declaration basic_decl_list;
        Sym.reset_current gtbl;
        if extern () then Sym.add_with gtbl nom

  and translate_body body glob loc =
    Npkcontext.set_loc loc;
    match (body, glob) with
      | (SubProgramBody(subprog_decl,decl_part, ctx1, ctx2, block), _) ->
          add_funbody subprog_decl decl_part ctx1 ctx2 block loc
      | PackageBody(name, package_spec, ctx, decl_part), true ->
          Sym.set_current gtbl name;
          Sym.push_saved_context gtbl ctx;
          (match package_spec with
             | None -> ()
             | Some(_, basic_decls, ctx, init) ->
                 begin
                   Sym.push_saved_context gtbl ctx;
                   List.iter (fun (id, exp) -> add_global_init id exp) init;
                   List.iter translate_global_basic_declaration basic_decls;
                   ignore (Sym.exit_context gtbl)
                 end
          );
          List.iter translate_global_decl_item decl_part;
          ignore (Sym.exit_context gtbl)

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
      | With(nom, loc, spec) ->
          Npkcontext.set_loc loc;
          (match spec with
            | Some(spec, loc) ->
                translate_spec spec loc true;
                Sym.add_with gtbl nom
            | None -> Npkcontext.report_error
                "Firstpass.translate_context"
                  "internal error : no specification provided")
      | UseContext x -> Sym.add_use gtbl x;
    );
  in
    (* corps de la fonction translate *)

  let normalized_compil_unit =  Normalize.normalization compil_unit false in
  let (ctx, lib_item, loc) = normalized_compil_unit in
    Npkcontext.set_loc loc;
    do_as_extern translate_context ctx;
    translate_library_item lib_item loc;
    Npkcontext.forget_loc ();
    { C.globals = globals; C.init = !init; C.fundecs = fun_decls }

