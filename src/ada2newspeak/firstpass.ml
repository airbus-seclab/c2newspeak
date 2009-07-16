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
module K   = Npkil
module Nat = Newspeak.Nat
module Npk = Newspeak
module A   = Syntax_ada
module T   = Ada_types
module Sym = Symboltbl

open Ast

exception AmbiguousTypeException

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
  | VarSymb  of C.lv    * A.subtyp * bool * bool (** name, type, global?, ro? *)
  | EnumSymb of C.exp   * A.typ * bool (** TODO, typename, ro? *)
  | FunSymb  of C.funexp * Ast.sub_program_spec * bool * C.ftyp (** XXX *)
  | NumberSymb of T.data_t*bool (** XXX *)

type qualified_symbol = symb*C.typ*Npk.location


(** Promotes an identifier to a name *)
let ident_to_name ident = (None, ident)

(** Builds a string from a name *)
let string_of_name = Ada_utils.name_to_string

let base_typ = Ada_utils.base_typ

(**
 * A boolean flip flop.
 *)
let extern = object
  val mutable extflag:bool = false

  method is_it = extflag

  method do_it (f:unit->unit) =
    extflag <- true;
    f ();
    extflag <- false
end

(**
 * Extract a scalar type.
 * Basically, is [function (C.Scalar t) -> t].
 *)
let extract_scalar_typ (cir_typ:C.typ) :Npk.scalar_t = match cir_typ with
  | C.Scalar(t) -> t
  | _ -> Npkcontext.report_error "Firstpass.extract_scalar_typ"
                                 "type isn't a scalar type"

(**
 * Add a "belongs" operator to an expression, according to a constraint.
 *)
let make_check_constraint (contrainte:A.contrainte) (exp:C.exp) :C.exp =
  match contrainte with
    | A.IntegerRangeConstraint (v1,v2) ->
        C.Unop(K.Belongs_tmp(v1, K.Known (Nat.add v2 Nat.one)), exp)
    | A.FloatRangeConstraint(_, _) -> exp
    | A.RangeConstraint _ -> Npkcontext.report_error
                          "Firstpass.make_check_constraint"
                     "internal error : unexpected range constraint (non-static)"

(**
 * Add a "belongs" operator to an expression, according to a subtype.
 *)
let make_check_subtyp (subtyp:A.subtyp) (exp:C.exp) :C.exp =
  match subtyp with
    | A.Unconstrained _ -> exp
    | A.Constrained(_, contrainte, _, _) ->
        make_check_constraint contrainte exp
    | A.SubtypName _ ->
        Npkcontext.report_error
          "Firstpass.make_check_subtyp"
          "internal error : unexpected subtyp name"

let make_offset (styp:A.subtyp) (exp:C.exp) (size:C.exp) =
  match styp with
    | A.Constrained( _, A.IntegerRangeConstraint(nat1, _) , _ ,_) ->
                let borne_inf = C.Const(C.CInt(nat1)) in
                let decal =  C.Binop (Npk.MinusI, exp, borne_inf) in
                    C.Binop (Newspeak.MultI, decal,  size)
    | A.Constrained  _ -> Npkcontext.report_error "Firstpass.make_offset"
                 "contrainte (not IntegerRangeConstraint) not coded yet "
    | A.Unconstrained _ -> exp
    | A.SubtypName _ -> Npkcontext.report_error "Firstpass.make_offset"
                    "SubtypName not implemented yet (especially for Enum)"

(**
 * Translate a [Syntax_ada.typ].
 *)
let rec translate_typ (typ:A.typ) :C.typ = match typ with
| A.Float        -> C.Scalar(Npk.Float            (Ada_config.size_of_float))
| A.Integer      -> C.Scalar(Npk.Int(Npk.Signed,   Ada_config.size_of_int))
| A.IntegerConst -> C.Scalar(Npk.Int(Npk.Signed,   Ada_config.size_of_int))
| A.Boolean      -> C.Scalar(Npk.Int(Npk.Unsigned, Ada_config.size_of_boolean))
| A.Character    -> C.Scalar(Npk.Int(Npk.Unsigned, Ada_config.size_of_char))
| A.Declared(_,typ_decl,_,_) -> translate_declared typ_decl

(**
 * Translate a [Syntax_ada.typ_declaration].
 *)
and translate_declared (typ_decl:A.typ_declaration) :C.typ = match typ_decl with
| A.Enum(_, bits) -> C.Scalar(Npk.Int(bits))
| A.DerivedType(subtyp_ind) -> translate_typ
    (Ada_utils.extract_typ subtyp_ind)
| A.IntegerRange(_,Some(bits)) -> C.Scalar(Npk.Int(bits))
| A.IntegerRange(_,None) -> Npkcontext.report_error
    "Firstpass.translate_declared"
      "internal error : no bounds provided for IntegerRange"
| A.Array a -> C.Array(translate_typ (Ada_utils.extract_typ a.A.array_component)
                      ,a.A.array_size)
| A.Record r -> let (fields,sz) = translate_record r in C.Struct (fields,sz)

(**
 * Translate a record type.
 * Builds a CIR Struct with packed offsets.
 *)
and translate_record (r:(string*A.subtyp) list) :(C.field list*Npk.size_t) =
  List.fold_left (fun (cflds,start_off) (id,st) ->
    let ctyp = translate_subtyp st in
    (id, (start_off, ctyp))::cflds , (start_off + C.size_of_typ ctyp)
  ) ([],0) r

(**
 * Translate a [Syntax_ada.subtyp].
 *)
and translate_subtyp (styp:A.subtyp) :C.typ = translate_typ (base_typ styp)

(**
 * Main translating function.
 * Takes an Ada program as and returns a CIR tree.
 *)
let translate (compil_unit:A.compilation_unit) :Cir.t =

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


  and gtbl = Sym.create () in

  let find_symb (x:A.name) = Hashtbl.find symbtbl x
  and find_all_symb (x:A.name) = Hashtbl.find_all symbtbl x
  and mem_symb (x:A.name) :bool = Hashtbl.mem symbtbl x
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

  let find_all_use ident =
    List.flatten
      (List.map
         (fun pack -> find_all_symb (Some pack, ident))
         (Sym.s_get_use gtbl))


  in

  let find_name (name:A.name)
                f_ident
                f_with
                f_current
    =
    match name with
      | (None, ident) -> f_ident ident name
      | (Some pack, ident) when extern#is_it
                       ||  (Sym.is_with gtbl pack) ->
          f_with (Some pack,ident)
      | (pack, ident) when pack = Sym.current gtbl ->
          f_current (None,ident) name
      | (Some pack, _) -> Npkcontext.report_error
          "Firstpass.find_name"
            ("unknown package "
             ^pack)
  in
  let find_name_record  name f_ident f_with f_current =
    match name with
      | (None, ident) -> f_ident ident name
      | (Some pack, ident) when not ( (Sym.current gtbl = Some pack)
                                   || (Sym.is_with gtbl pack)
                                   || extern#is_it)
        -> f_with (Some pack,ident)
      | (Some pack, ident) when ( extern#is_it
                              ||  Sym.is_with gtbl  pack)
        -> f_with (Some pack,ident)
      | (pack, ident) when pack = Sym.current gtbl->
          f_current (None,ident) name
      | (Some pack, _) -> Npkcontext.report_error
          "Firstpass.find_name"
            ("unknown package "
             ^pack)
  in


  let translate_int (i:Nat.t) :C.exp = C.Const(C.CInt i)
  in

  (* fonctions de traduction des noms*)
  (* on ajoute le nom du package courant dans le cas ou on
     etudie les declarations internes *)
  (* fonction appelee dans add_fundecl, add_funbody, add_global *)
  let translate_name (pack,id:A.name) :string =
    let tr_name =
        if extern#is_it then pack,            id
                             else (Sym.current gtbl), id
    in
      string_of_name tr_name
  in

  (* gestion de la table de symboles *)

  (* declaration d'une variable locale *)
  let add_var (loc:Newspeak.location) (st:Syntax_ada.subtyp)
               (ident:string) ~(deref:bool) ~(ro:bool)
      :unit=
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
      let tr_typ = translate_typ (base_typ st) in
      let (lv, typ_cir) =
        if deref
        then (C.Deref(C.Lval(C.Local ident, tr_typ), tr_typ), C.Scalar Npk.Ptr)
        else (C.Local ident, tr_typ)
      in
        Hashtbl.add symbtbl x (VarSymb (lv, st, false, ro),
                               typ_cir, loc)

  (* declaration d'un nombre local *)
  and add_number loc value lvl ident =
    let x =  Normalize.normalize_ident
      ident (Sym.current gtbl) extern#is_it in
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
        | T.IntVal _   -> translate_typ A.IntegerConst
        | T.FloatVal _ -> translate_typ A.Float
        | T.BoolVal _ ->
            Npkcontext.report_error
              "Firstpass.add_number"
              "internal error : number cannot have Enum val"
      in
        Hashtbl.add symbtbl x
          (NumberSymb(value, lvl), typ_cir, loc)



  (* declaration d'un symbole d'enumeration *)
  and add_enum loc ident value typ global =
    let name = Normalize.normalize_ident
      ident (Sym.current gtbl) extern#is_it in
      (if mem_symb name
       then
         List.iter
           (fun x -> match x with
                (* erreur : il y a une declaration de variable
                   ou de nombre de meme niveau que la declaration
                   courante *)
              | ((VarSymb(_, _, global',_) | NumberSymb(_,global')),
                 _,_) when global=global' ->
                  Npkcontext.report_error "Firstpass.add_enum"
                    ("conflict : "^ident^" already declared")

              (* erreur : declaration d'un symbol d'enumeration
                 de meme type et de meme niveau *)
              | (EnumSymb(_, t, global'),_,_)
                  when t=typ && global=global' ->
                  Npkcontext.report_error "Firstpass.add_enum"
                    ("conflict : "^ident^" already declared")

              (* il existe une fonction interne sans argument
                 ayant le meme type de retour que le symbole
                 d'enumeration, qui est global *)
              | (FunSymb (_, Function(_, [], st), false, _), _, _)
                  when global && (base_typ st)=typ ->
                  Npkcontext.report_error "Firstpass.add_enum"
                    ("conflict : "^ident^" already declared")

              | ((EnumSymb(_)|VarSymb(_)
                 |FunSymb _ |NumberSymb(_)),_,_) -> ())
           (find_all_symb name));
      Hashtbl.add symbtbl
        name
        (EnumSymb(translate_int value, typ,
                  global), translate_typ typ, loc)

  and add_global (loc:Npk.location) (typ:A.subtyp)
                 (tr_typ:C.typ)     (tr_init:C.init_t option)
                 (ro:bool)          (x:string)
       :unit =
    let name = Normalize.normalize_ident
      x (Sym.current gtbl) extern#is_it in

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

      Hashtbl.add globals tr_name (tr_typ, loc, tr_init);
      Hashtbl.add symbtbl name
        (VarSymb (C.Global(tr_name), typ, true, ro),
         tr_typ, loc)

  and remove_symb (x:string) :unit =
        Hashtbl.remove symbtbl (ident_to_name x) in

  let remove_formals args =
    remove_symb Params.ret_ident;
    List.iter remove_symb args
  in

  (** Used to generate temporary variables. *)
  let temp =
    object (s)
        val mutable count :int = 0

        (**
         * Build a fresh identifier.
         * Several calls will yield "tmp0", "tmp1", and so on.
         *)
        method private new_id :string=
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
        method create (loc:Newspeak.location) (t:A.typ)
          :string * (C.stmtkind * Newspeak.location) * C.lv =
            let id = s#new_id in
              add_var loc (A.Unconstrained(t)) id false false;
              let decl = (C.Decl (translate_typ t, id), loc) in
                (id, decl, C.Local id)
    end
  in

  (* fonctions pour la gestion des types *)

  let            check_typ = Ada_utils.check_typ
  and known_compatible_typ = Ada_utils.known_compatible_typ in

  (* recherche d'un symbol de fonction : a revoir *)
  let find_fun_symb name =

    let rec mem_other_symb list_ident var_masque =
      match list_ident with
        | ((VarSymb(_)|NumberSymb(_)),_,_)::r when var_masque ->
            mem_other_symb r var_masque
        | ((VarSymb(_)|NumberSymb(_)),_,_)::_ ->
            Npkcontext.report_error
              "Firstpass.find_fun_symb"
              ((string_of_name name)
               ^" is not a function")
        | (EnumSymb(_),_,_)::r -> mem_other_symb r var_masque
        | (FunSymb _,_,_)::_ -> true
        | [] -> false
    in
    let sans_selecteur (ident:string) (name:A.name) :qualified_symbol =
      let list_symb = find_all_symb name in

      let rec find_use list_symb var_masque =
        match list_symb with
          | ((VarSymb(_)|NumberSymb(_)),_,_)::r when var_masque ->
              find_use r var_masque

          | ((VarSymb(_)|NumberSymb(_)),_,_)::_ -> (*WG TO DO *)
              Npkcontext.report_error
                "Firstpass.find_fun_symb"
                (ident ^" is not a funtion 1 ")

          | (EnumSymb(_),_,_)::r ->
              find_use r var_masque

          | (FunSymb (_(*fn*), _(*sp*), true, _), C.Fun, _)::r ->
              if (mem_other_symb r var_masque)
              then (Npkcontext.report_error
                      "Firstpass.find_fun_symb"
                      (ident^" is not visible : "
                       ^"multiple use clauses cause hiding"))
              else (*fn, sp, trt*)
    (*        print_endline ("found --------------"
    ^(match fn with C.Fname f -> f | _ -> "unkno"));
        *)
                List.hd list_symb


          | (FunSymb (_, _, false, _), C.Fun, _)::_ -> Npkcontext.report_error
              "Firstpass.find_fun_symb"
                ("internal error : imported function not "
                 ^"tagged as extern")

          | (FunSymb _, _, _)::_ -> Npkcontext.report_error
              "Firstpass.find_fun_symb"
                ("internal error : translate type isn't "
                 ^"a fun type")

          | [] -> Npkcontext.report_error
              "Firstpass.find_fun_symb"
                ("cannot find symbol "^ident)
      in
      let rec find_interne list_symb var_masque =
        match list_symb with
          | ((VarSymb(_,_,true,_)|NumberSymb(_,true)),
             _,_)::r when var_masque ->
              find_interne r var_masque

          | (VarSymb(_),_,_)::_ ->
              (*WG*)
              List.hd list_symb

          | (NumberSymb(_),_,_)::_ ->
              Npkcontext.report_error
                "Firstpass.find_fun_symb"
                ((string_of_name name)
                 ^" is not a funtion (NumberSymb)")

          | (EnumSymb(_),_,_)::r ->
              find_interne r true

          | (FunSymb _, C.Fun, _)::_ ->
              List.hd list_symb

          | (FunSymb _, _,_)::_ ->
              Npkcontext.report_error "Firstpass.find_fun_symb"
                ("internal error : translate type isn't a fun type")

          | [] -> find_use (find_all_use ident) var_masque

      in find_interne list_symb false

    and avec_selecteur (name:A.name) :qualified_symbol =
      let list_symb = find_all_symb name in
      let rec find_fun list_symb =
        match list_symb with
          | ((VarSymb(_)|NumberSymb(_)),_,_)::_ ->  (*WG TO DO *)
              Npkcontext.report_error
               "Firstpass.find_fun_symb"
                ((string_of_name name)
                 ^" is not a funtion")

          | (EnumSymb(_),_,_)::r ->
              find_fun r

          | (FunSymb (_, _, true, _), C.Fun, _)::_ ->
              List.hd list_symb

          | (FunSymb _, _, _)::_ -> Npkcontext.report_error
              "Firstpass.find_fun_symb"
                ("internal error : translate type isn't "
                 ^"a fun type")

          | [] -> Npkcontext.report_error
              "Firstpass.find_fun_symb"
                ("cannot find symbol "^(string_of_name name))
      in find_fun list_symb

    and avec_selecteur_courant (ident:A.name) (name:A.name) :qualified_symbol =
      let list_symb = find_all_symb ident in
        let rec find_global list_symb =
        match list_symb with
          | ((VarSymb(_,_,false,_)|NumberSymb(_,false)),_,_)::r ->
              find_global r

          | ((VarSymb(_,_,true,_)|NumberSymb( _,true)),_,_)::_ ->(*WG TO DO *)
              Npkcontext.report_error
               "Firstpass.find_fun_symb"
                ((string_of_name name)
                 ^" is not a funtion")

          | (EnumSymb(_),_,_)::r                    -> find_global r

          | (FunSymb (_,_, false, _), C.Fun, _)::_ -> List.hd list_symb
          | (FunSymb (_,_, true , _),     _, _)::_ -> (* fonction externe *)
                              Npkcontext.report_error
                                "Firstpass.find_fun_symb"
                                ("cannot find symbol "^(string_of_name name))

          | (FunSymb _, _, _)::_ ->
              Npkcontext.report_error "Firstpass.find_fun_symb"
                ("internal error : translate type isn't a fun type")

          | [] -> Npkcontext.report_error
              "Firstpass.find_fun_symb"
                ("cannot find symbol "^(string_of_name name))
      in find_global list_symb

    in find_name name sans_selecteur
         avec_selecteur avec_selecteur_courant

  in

  (* fonctions de traductions *)
  let translate_subtyp_option subtyp = match subtyp with
    | None -> C.Void
    | Some(subtyp) -> translate_typ (base_typ subtyp)

  in
  let translate_number v expected_typ = match v with
      | T.IntVal(i) ->
          let t = check_typ expected_typ A.IntegerConst
          in (translate_int i, t)
      | T.FloatVal f ->
          let t = check_typ expected_typ A.Float
          in (C.Const(C.CFloat(f,string_of_float f)), t)
      | T.BoolVal _ ->
          Npkcontext.report_error
            "Firstpass.translate_number"
            "internal error : number cannot have enum val"
  in

  let rec translate_lv (lv:Ast.lval) (write:bool) (trans_exp:Ast.expression
                                          ->A.typ option->C.exp*A.typ) =
    (*cas d'un symbol sans selecteur *)
    let fun_sans_sel ident name =
      if mem_symb name then find_symb name
      else begin
          match find_all_use ident with
            | [a] -> a
            | [] -> Npkcontext.report_error
                "Firstpass.translate_lv"
                  ("cannot find symbol "^ident)
            | _ -> Npkcontext.report_error
                "Firstpass.translate_lv"
                  ("multiple use clause :"
                   ^ident^"is not visible")
          end
    in

    (* cas d'un symbol avec selecteur connu *)
    let fun_sel_connu name =
        if mem_symb name then find_symb name
        else Npkcontext.report_error "Firstpass.translate_lv"
          ("cannot find symbol "^(string_of_name name))
    in

    (*cas d'un symbol avec selecteur qui est le package courant*)
    let fun_ident_name ident name =
      if mem_symb ident then
        try
          List.find
            (fun (symb, _, _) -> match symb with
               | VarSymb(_,_,true,false) -> true
               | _ -> false)
            (find_all_symb ident)
        with
          | Not_found -> Npkcontext.report_error "Firstpass.translate_lv"
              ("cannot find symbol "^(string_of_name name))
      else
        Npkcontext.report_error "Firstpass.translate_lv"
          ("cannot find symbol "^(string_of_name name))
    in

      match lv with
          Ast.Lval lv ->
            let (symb, _, _) = find_name_record lv
              fun_sans_sel   (*sans selecteur *)
              fun_sel_connu  (*avec selecteur connu *)
              fun_ident_name (*avec selecteur =  pkg courant*)

            in begin
              match symb with
                | VarSymb(_,_,_, true) when write ->
                    Npkcontext.report_error "Firstpass.translate_lv"
                      ("Invalid left value : "^(translate_name lv)
                       ^" is read only")

                | VarSymb(v, typ, _, _) -> (v, typ)
                | NumberSymb(_) ->
                    Npkcontext.report_error "Firstpass.translate_lv"
                      "Invalid left value: unexpected number symbol"
                | FunSymb _ -> Npkcontext.report_error "Firstpass.translate_lv"
                    "Invalid left value: unexpected function symbol"
                | EnumSymb(_) ->
                    Npkcontext.report_error "Firstpass.translate_lv"
                      "Invalid left value: unexpected enum"
              end

        (*Assignation dans un tableau*)
        | Ast.ArrayAccess (lval, expr) ->
            let (v, subtyp_lv) = translate_lv lval write trans_exp in
              match  subtyp_lv
              with
                  A.Unconstrained(A.Declared(_, A.Array a,_, _)) ->
                    let (stypindex, contraint,_,_ ) = a.A.array_index     in
                    let (stypelt,_,_,_)             = a.A.array_component in
                    let size_base =  C.exp_of_int (C.size_of_typ (
                                (translate_typ (base_typ stypelt)))) in
                    let (exp,_) = trans_exp expr (Some(base_typ stypindex)) in
                    let new_constr  =
                      match contraint with
                          None -> begin match stypindex with
                            | A.Constrained(_, contr, _, _) -> contr
                            | A.Unconstrained _
                            | A.SubtypName _ ->
                                Npkcontext.report_error
                                  "Firstpass Array Access"
                                  "Unconstrained or SubtypName"
                          end

                        | Some(A.RangeConstraint(A.CInt(a), A.CInt(b)))

                        | Some(A.IntegerRangeConstraint(a, b)) ->
                            if (Nat.compare a b)<=0
                            then
                              A.IntegerRangeConstraint(a, b)
                            else         Npkcontext.report_error
                              "Firstpass: in Array access"
                              "null range not accepted "

                        | Some(A.RangeConstraint _) ->
                            Npkcontext.report_error
                              "Firstpass: in Array access"
                              "constraint is RangeConstraint"

                        | _ ->  Npkcontext.report_error
                            "Firstpass: in Array access"
                              "constraint is not IntegerRange"
                    in
                      (*
                        let chk_exp = match new_constr with
                        Some ctr -> make_check_constraint ctr exp
                        | _ -> exp in
                      *)

                    let chk_exp = make_check_constraint new_constr exp in

                    let offset =  match new_constr
                    with A.IntegerRangeConstraint(nat1, _) ->
                      let borne_inf =   C.Const(C.CInt(nat1)) in
                      let decal =  C.Binop (Npk.MinusI,chk_exp, borne_inf) in
                        C.Binop (Newspeak.MultI, decal,  size_base)
                      |  _ -> Npkcontext.report_error "Firstpass.make_offset"
                           "constr (not IntegerRangeConstraint) not impl yet"
                    in
                      (C.Shift (v, offset), stypelt)

                  | _ ->        (*Constrained*)
                      Npkcontext.report_error "firstpass: ArrayAcces"
                        " subtyp_lv has not expected typ"
  in

  let rec translate_if_exp (e_cond:Ast.expression)
                           (e_then:Ast.expression)
                           (e_else:Ast.expression) =
    let loc = Npkcontext.get_loc () in
    let (tmp, decl, vid) = temp#create loc A.Boolean in
    let name = ident_to_name tmp in
    let instr_if = Ast.If (e_cond,
                       [(Ast.Assign(Lval name, e_then, false),loc)],
                       [(Ast.Assign(Lval name, e_else, false),loc)])
    in let tr_instr_if =
        translate_block [(instr_if,loc)]
    in
      remove_symb tmp;
      (C.BlkExp (decl::tr_instr_if,
                 C.Lval (vid, translate_typ A.Boolean), false),
       A.Boolean)

  and translate_and (e1:Ast.expression) (e2:Ast.expression) =
    let loc = Npkcontext.get_loc () in
    let (tr_e2,_ ) = translate_exp e2 (Some A.Boolean) in
    let (tmp, decl, vid) = temp#create loc A.Boolean in
    let assign = C.Set (vid, translate_typ A.Boolean, tr_e2) in
    let tr_ifexp = fst (translate_if_exp e1
                                         e2
                                         (Ast.CBool false,T.boolean)) in
      remove_symb tmp;
      C.BlkExp (decl::(assign,loc)::[C.Exp tr_ifexp,loc]
      , C.Lval (vid, translate_typ A.Boolean), false), A.Boolean

  and translate_or (e1:Ast.expression) (e2:Ast.expression) =
    let loc = Npkcontext.get_loc () in
    let (tr_e2,_ ) = translate_exp e2 (Some A.Boolean) in
    let (tmp, decl, vid) = temp#create loc A.Boolean in
    let assign = C.Set (vid, translate_typ A.Boolean, tr_e2) in
    let tr_ifexp = fst (translate_if_exp e1
                                         (Ast.CBool true,T.boolean)
                                         e2) in
      remove_symb tmp;
      C.BlkExp (decl::(assign,loc)::[C.Exp tr_ifexp,loc]
      , C.Lval (vid, translate_typ A.Boolean), false), A.Boolean

  and translate_binop op (e1:Ast.expression) (e2:Ast.expression) expected_typ =
    let expected_typ1 = Ada_utils.typ_operand op expected_typ in
    let (tr_e1, tr_e2, typ) =
      try
        let (tr_e1, typ1) = translate_exp e1 expected_typ1 in
        let (tr_e2, typ2) = translate_exp e2 (Some(typ1))  in
        (tr_e1, tr_e2, typ2)
      with AmbiguousTypeException ->
        try
          let (tr_e2, typ2) = translate_exp e2 expected_typ1 in
          let (tr_e1, typ1) = translate_exp e1 (Some(typ2))  in
          (tr_e1, tr_e2, typ1)
        with AmbiguousTypeException ->
              Npkcontext.report_error "Firstpass.translate_binop"
                "ambiguous operands"
    in
      Ada_utils.check_operand_typ op typ;
      match (op,translate_typ typ) with
      (* Numeric operations *)
      | Plus, C.Scalar(Npk.Int   _)->C.Binop(Npk.PlusI   , tr_e1, tr_e2),typ
      | Plus, C.Scalar(Npk.Float n)->C.Binop(Npk.PlusF  n, tr_e1, tr_e2),typ
      | Minus,C.Scalar(Npk.Int   _)->C.Binop(Npk.MinusI  , tr_e1, tr_e2),typ
      | Minus,C.Scalar(Npk.Float n)->C.Binop(Npk.MinusF n, tr_e1, tr_e2),typ
      | Mult, C.Scalar(Npk.Int   _)->C.Binop(Npk.MultI   , tr_e1, tr_e2),typ
      | Mult, C.Scalar(Npk.Float n)->C.Binop(Npk.MultF  n, tr_e1, tr_e2),typ
      | Div,  C.Scalar(Npk.Int   _)->C.Binop(Npk.DivI    , tr_e1, tr_e2),typ
      | Div,  C.Scalar(Npk.Float n)->C.Binop(Npk.DivF   n, tr_e1, tr_e2),typ
      | Rem,  C.Scalar(Npk.Int   _)->C.Binop(Npk.Mod     , tr_e1, tr_e2),typ

      (* Comparisons *)
      | Eq, C.Scalar t -> C.Binop (Npk.Eq t, tr_e1, tr_e2), A.Boolean
      | Gt, C.Scalar t -> C.Binop (Npk.Gt t, tr_e1, tr_e2), A.Boolean

      | And, C.Scalar _ -> translate_and e1 e2
      | Or , C.Scalar _ -> translate_or  e1 e2

      | (Power | Mod ) ,_ ->
          Npkcontext.report_error "Firstpass.translate_binop"
            "run-time operator not implemented"

      | _ -> Npkcontext.report_error "Firstpass.translate_binop"
            "invalid operator and argument"

  and translate_not (exp:Ast.expression) expected_typ =
    match expected_typ with
      | None
      | Some(A.Boolean) ->
          let (exp, _) = translate_exp exp (Some(A.Boolean))
          in (C.Unop (K.Not, exp), A.Boolean)
      | _ -> Npkcontext.report_error "translate_not" "Unexpected type for not"

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
  and make_arg_list (args:argument list) (spec:Ast.param list)
      :Ast.expression list =
    let argtbl:(string,Ast.expression) Hashtbl.t = Hashtbl.create 5 in

    (**
     * Step 1 : extract positional parameters. Named parameters go into the
     * argtbl hashtable.
     *
     * Non-leading positional parameters, if any, remain in the "positional"
     * list and will lead to errors.
     *
     * /!\ Side-effects : this function references the argtbl variable.
     *)
    let rec extract_positional_parameters (ar :Ast.argument list)
        :Ast.expression list =
        (match ar with
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
        )

    (**
     * Step 2 : merge this list with the function specification, in order to
     * name the (formerly) positional parameters.
     * For the remaining parameters :
     *   - try to fetch them from the argtbl hashtable
     *   - try to assign their default value
     *
     * /!\ Side-effects : this function references the argtbl variable.
     *)
    and merge_with_specification (pos_list : Ast.expression list)
                                 (spec     : Ast.param      list)
        :(string*Ast.expression) list =
            match pos_list, spec with
              |  [],_  -> (* end of positional parameters *)
                          List.map (function x -> (x.formal_name,(
                                   try Hashtbl.find argtbl x.formal_name
                                   with Not_found -> begin
                                       match x.default_value with
                                         | Some value -> value
                                         | None ->
                                        Npkcontext.report_error
                                        "firstpass.fcall"
                                        ("No value provided for "
                                        ^"parameter "^x.formal_name
                                        ^", which has no default one.")
                                   end
                               )))
                               spec
              | e::pt,s::st -> (s.formal_name, e)::
                               (merge_with_specification pt st)
              | _::_,[]     -> Npkcontext.report_error "Firstpass.function_call"
                              "Too many actual arguments in function call"

    in
        (* Step 1... *)
        let pos      = extract_positional_parameters args in
        (* Step 2... *)
        let eff_args = merge_with_specification pos spec in
        (* We don't need the ids anymore *)
        List.map snd eff_args

  (** Translates a function call.  *)
  and translate_function_call (fname:C.funexp) (tr_typ:C.ftyp)
                              (spec:sub_program_spec) (arg_list:argument list)
                              (expected_typ:A.typ option) :C.exp*A.typ =
      let (params, ret_t) =
          match spec with
            | Function(_,params,subtyp) ->
                (params, check_typ expected_typ (base_typ subtyp))

            | Procedure(name, _) -> Npkcontext.report_error
                "Firstpass.translate_exp"
                  ((string_of_name name)
                   ^" is a procedure, function expected")
    in
    let arg_list = make_arg_list arg_list params in
    let translate_parameter (param:Ast.param) (exp:Ast.expression) :C.exp =
        let (tr_exp, _) = translate_exp exp (Some(base_typ param.param_type)) in
            make_check_subtyp param.param_type tr_exp in
    let (tr_params:C.exp list) = List.map2 translate_parameter params arg_list
    in
        try (C.Call(tr_typ, fname, tr_params), ret_t)
        with
          | Invalid_argument _ -> Npkcontext.report_error
                                "Firstpass.translate_function_call"
                                "wrong number of arguments"

  and translate_var name expected_typ =
    let trans_fun_typ tr_typ fname spec =
      translate_function_call fname tr_typ spec [] expected_typ
    in

    (* recherche d'autres symboles respectant eventuellement
       une condition (filter).
       use : indique si on peut regarder les imports
        None : pas d'import regarder
        Some(ident) : on peut rechercher
                     l'identifiant ident
       var_masque : indique si les variables sont masques par un
                    autre symbole
       Si les variables sont visibles, alors on declenche
       une erreur.
    *)
    let rec mem_other_symb list_symb filter use
        var_masque =
      match list_symb with
        | [] -> (match use with
                   | None -> false
                   | Some(ident) ->
                       (* on regarde les imports *)
                       mem_other_symb
                         (find_all_use ident) filter
                         None var_masque)

        (* les variables sont masquees *)
        | ((VarSymb(_)|NumberSymb(_)),_,_)::r when var_masque ->
            mem_other_symb r filter use var_masque

        | ((VarSymb(_)|NumberSymb(_)),_,_)::_ ->
            Npkcontext.report_error
              "Firstpass.translate_var"
              ((string_of_name name)^" is not visible : "
               ^"multiple use clauses cause hiding")

        (* un autre symbole existe ayant le bon type *)
        | (EnumSymb(_,typ,_),_,_)::_ when (filter typ) -> true
        | (FunSymb (_, (Function(_,[],typ)), _, _),_,_)::_
            when (filter (base_typ typ)) -> true

        (* symbole d'enumeration ou de fonctions n'ayant
           pas le bon type *)
        | (EnumSymb(_),_,_)::r | (FunSymb _,_,_)::r ->
            mem_other_symb r filter use var_masque

    in
    let sans_selecteur (ident:string) (name:A.name) :C.exp*A.typ =

      let rec find_use list_symb var_masque var_possible =
        match list_symb with
          | [] when var_masque ->
              (* variable masque : au moins
                 un symbol mais mauvais type *)
              Npkcontext.report_error
                "Firstpass.translate_var.find_enum_fun"
                "uncompatible types"

          | [] -> Npkcontext.report_error
              "Firstpass.translate_var"
                ("cannot find symbol "^ident)

          (* cas des variables *)
          | ((VarSymb(_)|NumberSymb(_)),_,_)::r when var_masque ->
              find_use r var_masque var_possible

          (* var visible, symbol unique*)
          | [(VarSymb(v,typ,_,_),tr_typ,_)] when var_possible ->
              let t = check_typ expected_typ (base_typ typ) in
                (C.Lval (v, tr_typ), t)

          (* nombre visible, symbole unique*)
          | [(NumberSymb(v,_),_,_)] when var_possible ->
              translate_number v expected_typ

          (* d'autres symboles existent, conflit ! *)
          | ((VarSymb(_)|NumberSymb(_)),_,_)::_ ->
              Npkcontext.report_error
                "Firstpass.translate_var"
                (ident^" is not visible : "
                 ^"multiple use clauses cause hiding")

          (* cas ou le type attendu n'est pas precise *)
          | (EnumSymb(exp,typ,_),_,_)::r
              when expected_typ = None ->
              if (mem_other_symb r (fun _ -> true)
                    None var_masque)
              then raise AmbiguousTypeException
              else (exp, typ)

          | (FunSymb (fname, (Function(_,[],_) as spec), _, tr_typ),
             _, _)::r when expected_typ=None ->
              if (mem_other_symb r (fun _ -> true)
                    None var_masque)
              then raise AmbiguousTypeException
              else trans_fun_typ tr_typ fname spec

          (* cas ou le type attendu est connu et correct *)
          | (EnumSymb(exp,typ,_),_,_)::r when
              known_compatible_typ expected_typ typ ->
              if (mem_other_symb r
                    (known_compatible_typ expected_typ)
                    None var_masque)
              then (Npkcontext.report_error
                      "Firstpass.translate_var"
                      (ident^" is not visible : "
                       ^"multiple use clauses cause hiding"))
              else (exp, typ)

          | (FunSymb (fname, (Function(_,[],subtyp) as spec), _, tr_typ),
             _, _)::r
              when (known_compatible_typ expected_typ
                      (base_typ subtyp)) ->
              if (mem_other_symb r
                    (known_compatible_typ expected_typ)
                    None var_masque)
              then (Npkcontext.report_error
                      "Firstpass.translate_var"
                      (ident^" is not visible : "
                       ^"multiple use clauses cause hiding"))
              else
                trans_fun_typ tr_typ fname spec

          (* autres cas : une variable n'est plus valide *)
          | (EnumSymb(_),_,_)::r | (FunSymb _,_,_)::r ->
              find_use r var_masque false


      in

      (* lorsqu'on a appele cette fonction, on a au moins
         un enum/fun en debut de la liste.
         Dans tous les cas, les variables sont masquees.
      *)
      let rec find_enum_fun list_symb = match list_symb with

        (* variable masque par le 1er symbole enum/fun *)
        | ((VarSymb(_)|NumberSymb(_)), _, _)::r -> find_enum_fun r

        (* cas type attendu connu et compatible :
           symbol unique a ce niveau*)
        | (EnumSymb(exp,typ,_),_,_)::_
            when known_compatible_typ expected_typ typ ->
            (exp, typ)

        | (FunSymb (fname, (Function(_,[],subtyp) as spec), _, tr_typ),
            _, _)::_
            when (known_compatible_typ expected_typ
                    (base_typ subtyp)) ->
            trans_fun_typ tr_typ fname spec

        (* cas type attendu non connu :
           recherche d'autres symboles *)
        | (EnumSymb(exp,typ,_),_,_)::r
            when expected_typ = None ->
            if (mem_other_symb r (fun _ -> true)
                  (Some(ident)) true)
            then raise AmbiguousTypeException
            else (exp, typ)

        | (FunSymb (fname, (Function(_,[],_) as spec), _, tr_typ),
           _, _)::r when expected_typ=None ->
            if (mem_other_symb r (fun _ -> true)
                  (Some(ident)) true)
            then raise AmbiguousTypeException
            else trans_fun_typ tr_typ fname spec

        (* autres cas : type incompatible, fonction
           a plusieurs arguments, procedure *)
        | (EnumSymb(_), _,_)::r | (FunSymb _, _, _)::r ->
            find_enum_fun r

        | [] -> find_use (find_all_use ident) true false


      in
      let list_symb = find_all_symb name in

        (*fonction principale : recherche d'un symbole
          lorsque le package n'est pas precise.*)
        match list_symb with
          | (VarSymb(v,subtyp,_,_),tr_typ,_)::_ ->
              let t = check_typ expected_typ (base_typ subtyp) in
                (C.Lval (v, tr_typ), t)
          | (NumberSymb(v,_),_,_)::_ ->
              translate_number v expected_typ

          | [] -> find_use (find_all_use ident) false true
          | _ -> find_enum_fun list_symb

    (* recherche d'un symbole externe, avec selecteur *)
    and avec_selecteur (name:A.name) :C.exp*A.typ =
      let rec find_enum_fun list_symb = match list_symb with
        | ((VarSymb(_)|NumberSymb(_)), _, _)::r ->
            find_enum_fun r

        (* type attendu connu et compatible *)
        | (EnumSymb(exp,typ,_),_,_)::_
            when known_compatible_typ expected_typ typ ->
            (exp, typ)

        | (FunSymb (fname, (Function(_,[],subtyp) as spec), _, tr_typ),
           _, _)::_
            when (known_compatible_typ expected_typ
                    (base_typ subtyp)) ->
            trans_fun_typ tr_typ fname spec

        (* type attendu inconnu *)
        | (EnumSymb(exp,typ,_),_,_)::r
            when expected_typ = None ->
            if mem_other_symb r (fun _ -> true) None true
            then raise AmbiguousTypeException
            else (exp, typ)

        | (FunSymb (fname, (Function(_,[],_) as spec), _, tr_typ),
           _, _)::r when expected_typ=None ->
            if mem_other_symb r (fun _ -> true) None true
            then raise AmbiguousTypeException
            else trans_fun_typ tr_typ fname spec

        | (EnumSymb(_), _,_)::r | (FunSymb _, _, _)::r ->
            find_enum_fun r

        | [] -> Npkcontext.report_error
            "Firstpass.translate_var"
              "uncompatible types" in

      let list_symb = find_all_symb name in
        match list_symb with
          | (VarSymb(v,subtyp,_,_),tr_typ,_)::_ ->
              let t = check_typ expected_typ (base_typ subtyp) in
                (C.Lval (v, tr_typ), t)
          | (NumberSymb(v,_),_,_)::_ ->
              translate_number v expected_typ
          | [] -> Npkcontext.report_error
              "Firstpass.translate_var"
                ("cannot find symbol "^(string_of_name name))
          | _ -> find_enum_fun list_symb

    (* recherche interne uniquement *)
    and avec_selecteur_courant (ident:A.name) (name:A.name) :C.exp*A.typ =
      let list_symb = find_all_symb ident in
      let rec find_global list_symb =
        match list_symb with
          | [] -> Npkcontext.report_error
              "Firstpass.translate_var"
                ("cannot find symbol "^(string_of_name name))

          (* variable globale *)
          | (VarSymb(v,subtyp,true,_),tr_typ,_)::_ ->
              let t = check_typ expected_typ (base_typ subtyp) in
                (C.Lval (v, tr_typ), t)

          (* nombre global *)
          | (NumberSymb(v,true),_,_)::_ ->
              translate_number v expected_typ

          (* type connu et compatible *)
          (*symbole d'enumeration global *)
          | (EnumSymb(exp,typ,true),_,_)::_
              when known_compatible_typ expected_typ typ ->
              (exp, typ)

          (* fonction interne *)
          | (FunSymb (fname,
                     (Function(_,[],subtyp) as spec), false, tr_typ),
             _, _)::_
              when (known_compatible_typ expected_typ
                      (base_typ subtyp)) ->
              trans_fun_typ tr_typ fname spec

          (*type inconnu*)
          (*symbole d'enumeration global *)
          | (EnumSymb(exp,typ,true),_,_)::r
              when expected_typ = None ->
              if mem_other_symb r (fun _ -> true) None false
              then raise AmbiguousTypeException
              else (exp, typ)

          (* fonction interne *)
          | (FunSymb (fname, (Function(_,[],_) as spec), false, tr_typ),
              _, _)::r when expected_typ=None ->
             if mem_other_symb r (fun _ -> true) None false
             then raise AmbiguousTypeException
             else trans_fun_typ tr_typ fname spec

          | _::r -> find_global r
      in find_global list_symb
    in
      find_name name sans_selecteur avec_selecteur
        avec_selecteur_courant

  and translate_exp (exp,_:Ast.expression) expected_typ = match exp with
    | CFloat f -> C.Const(C.CFloat(f,string_of_float f)),
                  check_typ expected_typ A.Float
    | CInt   i -> translate_int i,
                  check_typ expected_typ A.IntegerConst
    | CChar  c -> translate_int (Nat.of_int c),
                  check_typ expected_typ A.Character
    | CBool  b -> translate_int (Ada_utils.nat_of_bool b),
                  check_typ expected_typ A.Boolean
    | Var     name            -> translate_var  name expected_typ
    | Not   (exp)             -> translate_not  exp  expected_typ
    | Binary(binop,exp1,exp2) -> translate_binop binop exp1 exp2 expected_typ
    | CondExp(e1,e2,e3)       -> translate_if_exp e1 e2 e3
    | Qualified(subtyp, exp) ->
        let qtyp = check_typ expected_typ (base_typ subtyp) in
        let (tr_exp, typ) = translate_exp exp (Some(qtyp)) in
          (make_check_subtyp subtyp tr_exp, typ)

    | FunctionCall(name, arg_list) ->
        (*fonction ou lecture d'un element de tableau/matrice*)
        (* let (fname, spec, tr_typ) = find_fun_symb name in *)
        let array_or_fun = find_fun_symb name in
          begin
            match array_or_fun with
                (FunSymb (fname, spec, _, tr_typ), C.Fun,  _) ->
                  translate_function_call
                    fname tr_typ spec arg_list expected_typ
              | (VarSymb(lv, subtyp, _, _),_,_) ->  begin

                  (* array : strip ids *)
                  let arg_list = List.map snd arg_list in

                  let rec destroy subt = (*du plus gros vers plus petit*)
                    let styp_fom_ind (a,_,_,_) = a in
                    match subt with
                    | A.Unconstrained(A.Declared (_,A.Array a,_,_)) ->
                        let sbtypidx = styp_fom_ind a.A.array_index     in
                        let sbtypelt = styp_fom_ind a.A.array_component in
                        let deb = (sbtypidx, sbtypelt) in
                        let fin = destroy sbtypelt in
                          deb::fin
                    | _ -> []
                  in

                  (*TO DO base_typ already exist use it if possible*)
                  let subtyp_to_typ sub = match sub with
                      A.Constrained (z, _, _, _) ->  z
                    | _ -> Npkcontext.report_error "firstpass.ml:Function Call"
                                          " for array range TO DO "
                  in

                  (* TODO WG ! ajouter le belongs ! *)
                  let rec rebuild lv subt arg_list  =
                    let  last_exp = List.hd arg_list in
                    let (subt_range, tpelt) = List_utils.last subt in
                      (* ! ajouter le belongs ! *)

                    let chk_exp = make_check_subtyp subt_range last_exp
                    in
                    let sz =  C.exp_of_int (
                      C.size_of_typ ((translate_typ (base_typ tpelt)))) in

                    let offset = make_offset subt_range chk_exp sz in
                      match arg_list with
                        | [_] -> let adatyp = subtyp_to_typ tpelt in
                                   ( C.Lval (C.Shift (lv, offset),
                                             (  translate_typ adatyp )
                                            ),
                                     adatyp
                                   )
                       | _::tl -> rebuild (C.Shift (lv, chk_exp)) subt tl
                       | [] -> invalid_arg "offset"
                  in

                  let bk_typ = destroy subtyp in
                  let dim = List.length arg_list in
                    if (List.length bk_typ < dim)
                    then Npkcontext.report_error "firstpass.ml:Function Call"
                      "more elts than dimensions";

                    if (dim = 0) then
                      Npkcontext.report_error "firstpass.ml:Function Call"
                      "no element for shifting";
                    let types = List.map (
                      fun x -> Some (subtyp_to_typ (fst x))
                    ) bk_typ in

                    let dim_types = Array.to_list (
                      Array.sub  (Array.of_list types) 0 dim) in
                    let tr_arg_list =
                      List.map2 (fun x y ->
                        fst (translate_exp x y)) arg_list dim_types
                    in
                      match (rebuild lv bk_typ tr_arg_list) with
                          (C.Lval (a, tpelt), adatyp) ->
                            (C.Lval (a, tpelt),  adatyp)
                        | _ ->  Npkcontext.report_error "firstpass:FCall"
                            ("unexepted form in translate_exp")
                end
              | _ -> Npkcontext.report_error "Firstpass.translate_exp"
                  "FunctionCall case but unexpected result"
          end

  (**
   * Make a C assignment.
   *)
  and make_affect (id:C.lv)            (exp:C.exp)
                  (subtyp_lv:A.subtyp) (loc:Npk.location)
     :C.stmt =
    let typ_lv = base_typ subtyp_lv in
    let typ = translate_typ typ_lv in
    let checked_exp = make_check_subtyp subtyp_lv exp
    in (C.Set(id,typ,checked_exp),loc)

  (**
   * Translate a [Syntax_ada.Assign].
   *)
  and translate_affect lv exp loc unchecked =
    let (tr_lv,subtyp_lv) = translate_lv lv (not unchecked) translate_exp
      (*WG*) in
    let (tr_exp,_) = translate_exp exp (Some(base_typ subtyp_lv))
    in make_affect tr_lv tr_exp subtyp_lv loc

  (**
   * Translate a [Syntax_ada.block].
   *)
  and translate_block (block:Ast.block) :C.blk = match block with
    | [] -> []
    | (instr,loc)::r ->
        begin
        Npkcontext.set_loc loc;
         match instr with
           | Ast.Return(exp) ->
               translate_block (* WG Lval for Array diff*)
                 ((Ast.Assign( Lval (ident_to_name Params.ret_ident)
                             , exp
                             , false),loc)
                  ::(Ast.ReturnSimple,loc)::r)
           | Ast.ReturnSimple ->
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
           | Ast.Exit -> (C.Goto Params.brk_lbl, loc)::(translate_block r)
           | Ast.Assign(lv,exp,unchecked) ->
               (translate_affect lv exp loc unchecked)::(translate_block r)
           | Ast.If(condition,instr_then,instr_else) ->
               let (tr_exp, typ) = translate_exp condition (Some A.Boolean) in
                 if typ <> A.Boolean then begin
                   Npkcontext.report_error "Firstpass.translate_block"
                                         "expected a boolean type for condition"
                 end;
                 let tr_then = translate_block instr_then in
                 let tr_else = translate_block instr_else in
                   (C.build_if loc (tr_exp, tr_then, tr_else))
                   @(translate_block r)
           | Ast.Loop(NoScheme, body) ->
               let tr_body = translate_block body in
                 (C.Block([C.Loop(tr_body), loc], Some (Params.brk_lbl,[])),loc)
                 ::(translate_block r)
           | Ast.Loop(While(cond), body) ->
               translate_block
                 ((Loop(NoScheme,(If(cond,[],[Exit,loc]),loc)::body),
                   loc)::r)
           | Ast.ProcedureCall (name, args) -> begin
               let array_or_fun  = find_fun_symb name in
                 match array_or_fun with
                     (FunSymb (fname, spec, _, tr_typ), C.Fun,  _) ->
                       let params =
                         match spec with
                           | Function(_) -> Npkcontext.report_error
                               "Firstpass.translate_instr"
                                 ((string_of_name name)
                                  ^" is a function, procedure expected")
                           | Procedure(_, params) -> params
                       in
                       let tr_param (param:Ast.param) ((exp,tp):Ast.expression)=
                         match param.mode with
                           | A.In -> fst (translate_exp (exp,tp)
                                          (Some(base_typ param.param_type)))
                           | A.Out | A.InOut ->
                               match exp with
                                 | Var(v) ->
                                     let vid, typ = translate_lv (Lval v)
                                                                 true
                                                                 translate_exp
                                        in
                                     let t = check_typ
                                       (Some(base_typ param.param_type))
                                       (base_typ typ) in
                                       C.AddrOf(vid, translate_typ t)
                                 | _ ->  Npkcontext.report_error
                                     "Firstpass.translate_instr"
                                       ("Actual parameter with \"out\" or \"in "
                                        ^"out\" mode must be a variable")
                       in
                       let arg_list = make_arg_list args params in
                       let tr_params = List.map2 tr_param params arg_list
                       in
                         (C.Exp(C.Call(tr_typ, fname, tr_params)), loc)
                          ::(translate_block r)
                   | _ ->
                       Npkcontext.report_error "Firstpass.translate_instr"
                 "find_fun_symb did not expect this (maybe array) as an instr! "
             end

           | Ast.Case(e, choices, default)->(C.Switch(fst(translate_exp e None),
                            (* Choices *) (List.map (function exp,block ->
                                              let (value,typ) =
                                                  translate_exp exp None
                                              in (value, C.scalar_of_typ
                                                        (translate_typ typ)),
                                                  translate_block block)
                                              choices
                                              ),
                     (* "default" block *)  translate_block (
                                                Ada_utils.with_default
                                                                    default
                                                                    [])
                                            )
                                            ,loc)::(translate_block r)
            | Ast.Block (dp, blk) ->
                          (* xlt and remove_dp has side effects :
                             they must be done in this order *)
                         let t_dp = translate_declarative_part dp in
                         let res = (C.Block ((t_dp@(translate_block blk)),
                                        None),loc)
                         in remove_declarative_part dp; res::(translate_block r)
            end

  and translate_param param =
    let typ_cir = match param.mode with
      | A.In    -> translate_typ (base_typ param.param_type)
      |   A.Out
      | A.InOut -> C.Scalar(Npk.Ptr)
    in
        (fun _ -> typ_cir) param.formal_name

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
    in let params_typ : C.typ list = translate_param_list param_list in
      (name, (params_typ, translate_subtyp_option return_type))

  and add_fundecl subprogspec loc =
    let check_ident name =
     if Hashtbl.mem symbtbl name
     then
       let list_ident = Hashtbl.find_all symbtbl name in
         List.iter
           (fun symb -> match symb with
              | (FunSymb (_,_,extern', _),_,_)
                  when extern'= extern#is_it ->
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
                          when etyp = (base_typ rtyp) ->
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
                  extern#is_it, ftyp), C.Fun, loc);
      ftyp

  and translate_enum_declaration idtyp typ_decl list_val_id loc global =
    List.iter
      (fun (x,id) -> add_enum loc x id
         (A.Declared(idtyp, typ_decl, T.unknown, loc)) global)
      list_val_id

  and translate_derived_typ_decl subtyp_ind loc global =
    match Ada_utils.extract_typ subtyp_ind with
      | A.Declared(idtyp, (A.Enum(list_val_id, _) as typ_decl),_,_) ->
          translate_enum_declaration idtyp typ_decl list_val_id loc global
      | _ -> ()

  and translate_typ_declaration idtyp typ_decl loc global =
    match typ_decl with
      | A.Enum (list_val_id, _) ->
          translate_enum_declaration idtyp typ_decl list_val_id loc global
      | A.DerivedType ref_subtyp_ind ->
          translate_derived_typ_decl ref_subtyp_ind loc global
      | A.IntegerRange _
      | A.Array        _
      | A.Record       _ -> ()

  and translate_basic_declaration basic loc = match basic with
    | ObjectDecl(ident, subtyp_ind, const) ->
        let subtyp =  Ada_utils.extract_subtyp subtyp_ind in
        let read_only = const <> Variable in
        add_var loc subtyp ident false read_only;
        Some (C.Decl(translate_subtyp subtyp,
          ident),loc)

    | TypeDecl (idtyp,typ_decl) ->
        translate_typ_declaration idtyp typ_decl loc false;
        None

    | SubtypDecl _ -> None

    | SpecDecl(_) -> Npkcontext.report_error
        "Firstpass.translate_basic_declaration"
          ("declaration de sous-fonction, sous-procedure ou "
           ^"sous package non implemente")

    | UseDecl(use_clause) -> Sym.s_add_use gtbl use_clause;
                             None
    | NumberDecl(ident, v) ->
        add_number loc v false ident;
        None

  and translate_declarative_item (item,loc) =
    Npkcontext.set_loc loc;
    match item with
      | BasicDecl(basic) -> translate_basic_declaration basic loc
      | BodyDecl _ -> Npkcontext.report_error "Firstpass.translate_block"
            "sous-fonction, sous-procedure ou sous package non implemente"

  and translate_declarative_part decl_part =
    Sym.enter_context gtbl;
    List_utils.filter_map translate_declarative_item decl_part

  and remove_basic_declaration basic = match basic with
    | ObjectDecl(ident, _, _) -> remove_symb ident
    | TypeDecl(_,A.Enum(idents,_))  -> List.iter (fun (x,_) -> remove_symb x)
                                               idents
    | SpecDecl(_) -> Npkcontext.report_error
        "Firstpass.remove_basic_declaration"
          ("declaration de sous-fonction, sous-procedure ou "
           ^"sous package non implemente")
    | UseDecl _ -> ()
    | NumberDecl(ident, _) -> remove_symb ident
    | TypeDecl _
    | SubtypDecl _ -> ()

  and remove_declarative_item (item,_) = match item with
    | BasicDecl(basic) -> remove_basic_declaration basic
    | BodyDecl(_) -> ()

  and remove_declarative_part decl_part =
    Sym.exit_context gtbl;
    List.iter remove_declarative_item decl_part

  and add_funbody subprogspec decl_part block loc =
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

    let ftyp = search_spec name
    and (params, (ret_id, args_ids)) = add_params subprogspec loc
    and body_decl = translate_declarative_part decl_part
    and body = translate_block block in
    let body = (C.Block (body_decl@body, Some (Params.ret_lbl,[])), loc)::[] in
      remove_formals params;
      remove_declarative_part decl_part;

      Hashtbl.replace fun_decls (translate_name name)
        (ret_id, args_ids, ftyp, body)

  in

  let rec translate_global_basic_declaration (basic, loc) =
    match basic with
      | ObjectDecl(ident, subtyp_ind, const) ->
        let init = get_global_init ident in
        let subtyp = Ada_utils.extract_subtyp subtyp_ind in

          let read_only = const <> Variable in
          let tr_typ = translate_subtyp subtyp in
          let tr_init : C.init_t option =
            match (init, extern#is_it) with
              | (_,true)
              | (None,_) -> Some None
              | (Some(exp),false) ->
                  let (tr_exp,_) = translate_exp exp (Some(base_typ subtyp))
                  in Some((Some [(0, extract_scalar_typ tr_typ, tr_exp)]))
          in
          add_global loc subtyp tr_typ tr_init read_only ident
      | TypeDecl(idtyp,typ_decl) ->
          translate_typ_declaration idtyp typ_decl loc true
      | SubtypDecl _ -> ()
      | UseDecl x -> Sym.s_add_use gtbl x
      | SpecDecl(spec) -> translate_spec spec loc false
      | NumberDecl(ident, v) ->
           add_number loc v true ident

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

    | PackageSpec(nom, basic_decl_list,init) ->
        if (not glob) then begin Npkcontext.report_error
            "Firstpass.translate_spec"
                "declaration de sous package non implemente"
        end;
        Sym.set_current gtbl nom;
        List.iter (fun (id, exp) -> add_global_init id exp) init;
        List.iter translate_global_basic_declaration basic_decl_list;
        Sym.reset_current gtbl;
        if extern#is_it then Sym.add_with gtbl nom

  and translate_body (body:Ast.body) glob loc :unit =

    Npkcontext.set_loc loc;
    match (body, glob) with
      | (Ast.SubProgramBody(subprog_decl,decl_part, block), _) ->
          add_funbody subprog_decl decl_part block loc
      | Ast.PackageBody(name, package_spec, decl_part), true ->
          Sym.set_current gtbl name;
          (match package_spec with
             | None -> ()
             | Some(_, basic_decls, init) ->
                 List.iter (fun (id, exp) -> add_global_init id exp) init;
                 List.iter translate_global_basic_declaration basic_decls
          );
          List.iter translate_global_decl_item decl_part

      | Ast.PackageBody _, false -> Npkcontext.report_error
          "Firstpass.translate_body"
            "declaration de sous package non implemente"

  in

  let translate_library_item lib_item loc =
    Npkcontext.set_loc loc;
    match lib_item with
      | Ast.Body body -> translate_body body true loc
      | Ast.Spec _    -> Npkcontext.report_error
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
      | UseContext x -> Sym.s_add_use gtbl x;
    );
  in
    (* corps de la fonction translate *)

  let normalized_compil_unit =  Normalize.normalization compil_unit false
  in
  let (ctx,lib_item,loc) = normalized_compil_unit
  in
    try
      Npkcontext.set_loc loc;
      extern#do_it (fun _ -> translate_context ctx);
      translate_library_item lib_item loc;
      Npkcontext.forget_loc ();
      { C.globals = globals; C.fundecs = fun_decls; C.specs = [] }
    with AmbiguousTypeException ->
      Npkcontext.report_error "Firstpass.translate"
                "uncaught ambiguous type exception"

