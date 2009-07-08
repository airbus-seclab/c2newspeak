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
open Eval

module Nat = Newspeak.Nat
module  T  = Ada_types
module TC  = Typecheck
module Sym = Symboltbl

let (%+) = Nat.add
let (%-) = Nat.sub

let gtbl : Sym.t = Sym.create ()

let string_of_name = Ada_utils.name_to_string

let typ_to_adatyp : Syntax_ada.typ -> Ada_types.t = function
  | Integer            -> T.integer
  | IntegerConst       -> T.universal_integer
  | Float              -> T.std_float
  | Boolean            -> T.boolean
  | Character          -> T.character
  | Declared (_,_,t,_) -> t

let subtyp_to_adatyp st =
  match st with
  | Unconstrained typ      -> T.new_unconstr (typ_to_adatyp typ)
  | Constrained (_,_,_,t) -> t
  | SubtypName  n          -> try
                                Sym.s_find_type gtbl n
                              with Not_found ->
                                begin
                                  Npkcontext.report_warning "ST2AT"
                                    ("Cannot find type '"
                                    ^name_to_string n
                                    ^"'");
                                  T.unknown;
                                end

(**
 * Try to find a body for a given specification.
 *
 * @param ~specification is the [basic_declaration] associated to the
 *                       specification, as found in a [package_spec].
 * @param ~bodylist is a list of possible bodies, as found
 *                  in a [declarative_part].
 * @return a boolean describing whether a body could be found.
 *)
let find_body_for_spec ~(specification:Ast.basic_declaration)
                       ~(bodylist:Ast.declarative_item list) :bool =
  (* Try to match a spec and a body *)
  let match_ok s b = match (s,b) with
    | Ast.SpecDecl(Ast.SubProgramSpec sps),
        Ast.BodyDecl(Ast.SubProgramBody (spsb,_,_)) -> sps = spsb
    | Ast.ObjectDecl _ as x,Ast.BasicDecl (Ast.ObjectDecl _ as y) -> x = y
    | _ -> false
  in
  List.exists (function bd -> match_ok specification bd) bodylist

(** Return the name for a specification. *)
let name_of_spec (spec:Ast.basic_declaration) :string = match spec with
  | Ast.ObjectDecl (idl,_,_) -> ident_list_to_string idl
  | Ast.TypeDecl   (i,_)
  | Ast.NumberDecl (i,_)
  | Ast.SubtypDecl (i,_) -> i
  | Ast.SpecDecl (Ast.SubProgramSpec (Ast.Function  (n,_,_)))
  | Ast.SpecDecl (Ast.SubProgramSpec (Ast.Procedure (n,_))) -> name_to_string n
  | Ast.SpecDecl (Ast.PackageSpec (n,_,_)) -> n
  | Ast.UseDecl _ -> "<no name>"

let check_package_body_against_spec ~(body:Ast.declarative_part)
                                    ~(spec:Ast.package_spec) =
  let (pkgname,spec_and_loc,_) = spec in
  let (        body_and_loc  ) = body in
  let speclist = List.map fst spec_and_loc in
  let bodylist = List.map fst body_and_loc in
  (* Filter on specifications : only sp such as
   * filterspec sp = true will be checked.      *)
  let filterspec = function
    | Ast.NumberDecl _ | Ast.SpecDecl   _ -> true
    | Ast.ObjectDecl _ | Ast.TypeDecl _ | Ast.SubtypDecl _
    | Ast.UseDecl _ -> false
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

let normalize_ident ident package extern =
  if extern then (package, ident)
            else (None   , ident)

let build_init_stmt (x,exp,loc) =
  Ast.Assign( Ast.Lval (None,x), exp, true), loc

let extract_subprog_spec (ast:compilation_unit):compilation_unit =
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
let parse_specification (name:string) :compilation_unit =
  (* tricherie : probleme avec sous-package *) (* FIXME *)
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
let parse_package_specification (name:string):package_spec =
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

(*
 * renvoie la specification normalisee du package correspondant
 * a name, les noms etant traites comme extern a la normalisation
 *)
let rec parse_extern_specification (name:string) :Ast.spec*location =
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
and normalization (compil_unit:compilation_unit) (extern:bool)
    :Ast.compilation_unit =
  let csttbl = Hashtbl.create 100 in

  (**
   * This object encapsulates the table of types. It is basically a Hashtbl.t
   * mapping a [name] to a triplet of :
   *   - a [subtyp]
   *   - a [location]
   *
   * /!\ Side-effects : all methods call the underlying ones in Hashtbl.t,
   *                    and thus can modify the state of the object.
   *)
  let types =
    object (s)
      val tbl = Hashtbl.create 0

      (** Add a new subtype, or raise an error in case of conflict. *)
      method add (n:Syntax_ada.name) (subtyp:subtyp)
      (location:location)   (global:bool)
      :unit =
        if s#mem n then begin
          match Hashtbl.find tbl n with
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
             snd n = "integer"
          || snd n = "float"
          || snd n = "boolean"
          || snd n = "character"
          || Hashtbl.mem tbl n

      (** Find the type definition. *)
      method find (n:name) :(subtyp*location) =
        match String.lowercase (snd n) with
          | "integer"   ->  Syntax_ada.Constrained(Syntax_ada.Integer
                                                  ,Ada_config.integer_constraint
                                                  ,true
                                                  ,T.integer
                                                  )
                           ,Newspeak.unknown_loc
          | "float"     ->  Syntax_ada.Unconstrained Syntax_ada.Float
                           ,Newspeak.unknown_loc
          | "boolean"   ->  Syntax_ada.Unconstrained Syntax_ada.Boolean
                           ,Newspeak.unknown_loc
          | "character" ->  Syntax_ada.Unconstrained Syntax_ada.Character
                           ,Newspeak.unknown_loc
          | _ -> let (x,y,_) = Hashtbl.find tbl n in (x,y)

      (** Find all the types matching. *)
      method find_all (n:name)
      :(subtyp*location*bool) list =
        Hashtbl.find_all tbl n

      (** Remove a subtype definition. *)
      method remove (x:name) :unit =
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

  let normalize_ident_cur_ext ident is_ext =
    normalize_ident ident (Sym.current gtbl) is_ext
  in

  let normalize_ident_cur ident =
    normalize_ident_cur_ext ident extern

  in

  let find_all_use (ident:string)
        :(subtyp*location*bool) list =
    List.flatten
      (List.map
         (fun pack -> types#find_all (Some pack, ident))
         (Sym.s_get_use gtbl))
  in

  let add_enum_litt symbs typ global extern =
    List.iter (fun (ident,v) -> let (p,i) = normalize_ident_cur_ext ident extern
                                in
                                 add_enum (p,i)
                                         typ
                                         global
                                         v)
              symbs
  in
  let add_typ nom typdecl location ~global ~extern =
    let subtyp = match typdecl with
      | Array  _
      | Record _ -> Unconstrained(Declared(snd nom
                                          ,typdecl
                                          ,T.unknown
                                          ,location))
      | Enum(symbs,_) ->
          let min = snd (List.hd symbs)
          and max = snd (List_utils.last symbs) in
          let contrainte = IntegerRangeConstraint(min, max) in
          let t = T.new_enumerated (fst (List.split symbs)) in
          let typ = Declared(snd nom,typdecl, T.unknown,location) in
            add_enum_litt symbs typ global extern;
            Constrained(typ, contrainte, true, t)

      | IntegerRange(contrainte, _) ->
          Constrained(Declared(snd nom
                              ,typdecl
                              ,T.unknown
                              ,location)
                     ,contrainte
                     ,true
                     ,T.unknown (* FIXME *)
                     )
      | DerivedType(subtyp_ind) ->
          let subtyp = extract_subtyp subtyp_ind in
          let typ = base_typ subtyp in
            begin
              match typ with
                | Declared(_,Enum(symbs,_),_,_) ->
                    add_enum_litt symbs typ global extern
                | _ -> ()
            end;
            subtyp
    in
      types#add nom subtyp location global;

  and find_subtyp x =

    let sans_selecteur ident =
      if types#mem x then types#find x
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
      try types#find x
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
          | (None, ident) -> sans_selecteur ident
          | (Some pack, _) when extern ||
                (Sym.is_with gtbl pack) -> avec_selecteur x
          | (pack, ident) when pack = (Sym.current gtbl) ->
              selecteur_courant (None,ident)
          | (Some pack, _) -> Npkcontext.report_error "Ada_normalize.find_typ"
                ("unknown package " ^pack)
  in

  let normalize_name (parents, ident) =
    Sym.normalize_name gtbl (parents,ident) extern
  in

let rec normalize_subtyp_indication (subtyp_ref, contrainte, subtyp, adatype) =
  (* on etablit le sous-type tel qu'il sera utilise dans
     le reste du code, a partir du type de base du sous-type
     de reference, de la contrainte normalisee, et d'un
     booleen qui indique si le sous-type de reference est
     statique *)
    if subtyp <> None then Npkcontext.report_error
              "Ada_normalize.normalize_subtyp_indication"
              "internal error : subtyp already provided";
    let norm_subtyp_ref = normalize_subtyp subtyp_ref in
    let (norm_subtyp, norm_contrainte) =
      match (contrainte, norm_subtyp_ref) with
        | (None, Unconstrained(typ)) ->
            (Unconstrained(typ), None)
        | (None, Constrained(typ, const, static, t)) ->
            (Constrained(typ, const, static, t), None)
        | (Some(const), Unconstrained(typ)) ->
            let norm_contrainte =
              normalize_contrainte const typ
            in
              (Constrained(typ,norm_contrainte,true,typ_to_adatyp typ),
               Some(norm_contrainte))
        | (Some(const), Constrained(typ,const_ref,stat_ref,t)) ->
            let norm_contrainte = normalize_contrainte const typ in
              constraint_check_compatibility const_ref norm_contrainte;
              (Constrained(typ,norm_contrainte,stat_ref,t),
               Some(norm_contrainte))
        | (_, SubtypName _ ) ->
            Npkcontext.report_error
              "Ada_normalize.normalize_subtyp_indication"
              "internal error : unexpected subtyp name"
    in
      (norm_subtyp_ref, norm_contrainte, Some(norm_subtyp), adatype)

and normalize_subtyp subtyp =
  let norm_typ typp =
    match typp with
        Declared (id,Array a,t,x) ->
          let n_st1 =  normalize_subtyp_indication a.array_index     in
          let n_st2 =  normalize_subtyp_indication a.array_component in

        let subtyp = extract_subtyp n_st1 in

        let contrainte = match subtyp with
          | Constrained(_,contrainte,_,_) -> contrainte
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
          Declared (id
                   ,Array({array_index     = n_st1;
                           array_component = n_st2;
                           array_size      = taille;
                         })
                   ,t
                   ,x)
      |  _ -> typp
  in
    match subtyp with (* For array norm_typ is used here*)
      | Unconstrained(typ) -> Unconstrained(norm_typ typ)
      | Constrained(typ,const,static,t) ->
          Constrained(norm_typ typ,const,static,t)
      | SubtypName(name) ->
          fst (find_subtyp (normalize_name name))

(**
 * Normalize an actual argument.
 * The identifier does not have to be normalized (it is just a plain string),
 * but normalize the expression.
 *)
and normalize_arg (id,e:argument) :Ast.argument = id,normalize_exp e

and normalize_binop (bop:binary_op) (e1:expression) (e2:expression)
  :Ast.expression =
  let direct_op_trans = function
  | Plus  -> Ast.Plus  | Minus -> Ast.Minus | Div   -> Ast.Div
  | Mult  -> Ast.Mult  | Or    -> Ast.Or    | And   -> Ast.And
  | Gt    -> Ast.Gt    | Eq    -> Ast.Eq    | Rem   -> Ast.Rem
  | Mod   -> Ast.Mod   | Power -> Ast.Power
  |_ -> invalid_arg "direct_op_trans"
  in
  (* Is the operator overloaded ? *)
  let ovl_opname = (None,make_operator_name bop) in
  if (Hashtbl.mem csttbl ovl_opname) then
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
          | _      , Qualified (st,_) -> Some (subtyp_to_adatyp st)
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

and normalize_uop (uop:unary_op) (exp:expression) :Ast.expression =
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
  let (_,top) = Sym.s_find_subprogram gtbl n in
  let t = match top with
  | None -> Npkcontext.report_error "normalize_exp"
            "Expected function, got procedure"
  | Some top -> top
  in
  Ast.FunctionCall(n, List.map normalize_arg params),t

(**
 * Normalize an expression.
 *)
and normalize_exp ?(expected_type:Ada_types.t option) (exp:expression)
:Ast.expression = match exp with
  | CInt   x -> Ast.CInt   x,T.universal_integer
  | CFloat x -> Ast.CFloat x,T.universal_real
  | CBool  x -> Ast.CBool  x,T.boolean
  | CChar  x -> Ast.CChar  x,T.character
  | Var    n -> begin (* n may denote the name of a parameterless function *)
                  let n' = normalize_name n in
                    try let t = Sym.s_find_variable ?expected_type gtbl n in
                            Ast.Var(n') ,t
                            with Symboltbl.ParameterlessFunction (rt) ->
                              Ast.Var(n') ,rt
                end
  | Unary (uop, exp)    -> normalize_uop uop exp
  | Binary(bop, e1, e2) -> normalize_binop bop e1 e2
  | Qualified(subtyp, exp) -> let t = subtyp_to_adatyp subtyp in
                              Ast.Qualified(normalize_subtyp subtyp,
                                  normalize_exp ~expected_type:t exp)
                              ,t
  | FunctionCall(n, params) -> normalize_fcall (n, params)
  | Attribute (st, attr)->
      begin
        let t = subtyp_to_adatyp st in
        match T.attr_get t attr with
        | (t,T.IntVal   x) -> Ast.CInt   x, t
        | (t,T.BoolVal  x) -> Ast.CBool  x, t
        | (t,T.FloatVal x) -> Ast.CFloat x, t
      end

(**
 * Normalize a constraint.
 *)
and normalize_contrainte (contrainte:contrainte) (typ:typ) :contrainte =
  let eval_range exp1 exp2 =
    let norm_exp1 = normalize_exp exp1
    and norm_exp2 = normalize_exp exp2 in
      (* on essaye d'evaluer les bornes *)
      (try
         let val1 = eval_static
           norm_exp1 (Some(typ)) csttbl
           gtbl extern
         and val2 = eval_static
           norm_exp2 (Some(typ)) csttbl
           gtbl extern in
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

in
  let add_extern_typdecl id typ_decl loc =
    add_typ (normalize_ident_cur_ext id true)
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
                 let exp' = normalize_exp exp in
                 let v = eval_static_integer_exp exp' csttbl gtbl false
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
        end
      else
        (symbs, size)
    in Enum(symbs, size)

  in

  let normalize_integer_range taille contrainte =
    match (taille, contrainte) with
      | (None, RangeConstraint(_)) ->
          begin
            try
              let norm_contrainte =
                normalize_contrainte contrainte IntegerConst
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

  let normalize_typ_decl ident typ_decl loc global represtbl =
   match typ_decl with
    | Enum(symbs, size) ->
        let typ_decl = enumeration_representation ident symbs
                                                  size represtbl loc in
        let id = normalize_ident_cur ident in
        let ids = fst (List.split symbs) in
        let t = T.new_enumerated ids in
        Sym.s_add_type gtbl ident t;
        List.iter (fun i -> Sym.s_add_variable gtbl i t
        ) ids;
        add_typ id typ_decl loc global extern;
        typ_decl
    | DerivedType(subtyp_ind) ->
        let t =
          let (tp,_,_,st) = subtyp_ind in
          if st = T.unknown then subtyp_to_adatyp tp
          else st
        in
        let new_t = T.new_derived t in
          Sym.s_add_type gtbl ident new_t;
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
          | Character -> DerivedType(norm_subtyp_ind)
          | IntegerConst ->
              Npkcontext.report_error
                "Ada_normalize.normalize_typ_decl"
                "internal error : incorrect type"
          (* declared types : simplification *)
          | Declared(parent,Enum(symbs, size),_,_) ->
              List.iter (fun (i,_) -> Sym.s_add_variable gtbl i new_t) symbs;
              check_represent_clause_order parent represtbl loc;
              enumeration_representation ident symbs size represtbl loc
          | Declared(_, (( IntegerRange(_,_)
                        | Array  _
                        | Record _
                        | DerivedType _
                        ) as def),_,_) -> def
        in

        (*constitution of the subtype representing the current declaration *)
        let norm_subtyp =
            match (extract_subtyp norm_subtyp_ind ) with
            | Unconstrained _ -> Unconstrained(Declared(ident
                                                       ,typ_decl
                                                       ,T.unknown
                                                       ,loc
                                                       ))
              | Constrained(_, contrainte, static, t) ->
              (* for enumeration types, we update the value of the constraint *)
                  let contrainte = match (typ_decl, parent_type) with
                    | (Enum(new_assoc,_), Declared(_,Enum(symbs, _),_,_)) ->
                        update_contrainte contrainte symbs new_assoc
                    | _ -> contrainte
                  in Constrained(Declared(ident
                                         ,typ_decl
                                         ,T.unknown
                                         ,loc)
                                ,contrainte
                                ,static
                                ,t
                                )
              | SubtypName _ ->
                  Npkcontext.report_error
                    "Ada_normalize.normalize_typ_decl"
                    "internal error : unexpected subtyp name" in

        let new_subtyp_ind =
          let (subtyp, contrainte, _, adatype) = norm_subtyp_ind in
          (subtyp, contrainte, Some(norm_subtyp),adatype)
        in
        let norm_typ_decl = DerivedType(new_subtyp_ind) in
          add_typ (normalize_ident_cur ident) norm_typ_decl loc global extern;
          norm_typ_decl
    | IntegerRange(contrainte,taille) ->
        let decl = normalize_integer_range taille contrainte in
        let id = normalize_ident_cur ident in
        Sym.s_add_type gtbl ident (T.new_range T.null_range);
        add_typ id decl loc global extern;
        decl
    | Array a when a.array_size = None ->
        let norm_inter =  normalize_subtyp_indication a.array_index
        and norm_subtyp_ind = normalize_subtyp_indication a.array_component in
        let subtyp = extract_subtyp norm_inter in
        let (tpc,_,_,ttc) = a.array_component in
        let (tpi,_,_,tti) = a.array_index     in
        let tc = if ttc = T.unknown then subtyp_to_adatyp tpc else ttc in
        let ti = if tti = T.unknown then subtyp_to_adatyp tpi else tti in
        let t = T.new_array tc [ti] in
        Sym.s_add_type gtbl ident t;
        let contrainte = match subtyp with
          | Constrained(_,contrainte,_,_) -> contrainte
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
        let norm_typ = Array {array_index     = norm_inter;
                              array_component = norm_subtyp_ind;
                              array_size      = taille}
        in
          add_typ (normalize_ident_cur ident) norm_typ loc global extern;
          norm_typ
    | Array _ -> Npkcontext.report_error "Ada_normalize.normalize_typ_decl"
                          "internal error : size of array already provided"
    | Record r -> begin
                    Sym.s_add_type gtbl ident (T.unknown);
                    let norm_fields =
                      List.map (fun (id, st) -> (id, normalize_subtyp st)) r
                    in
                    let norm_typ = Record norm_fields in
                    add_typ (normalize_ident_cur ident) norm_typ
                            loc global extern;
                    norm_typ
                  end
  in

  let remove_typ_decl nom typ_decl = match typ_decl with
    | Enum(symbs, _) -> types#remove (normalize_ident_cur nom);
        List.iter (fun (symb, _) -> remove_cst (normalize_ident_cur symb))
                  symbs
    | DerivedType  _
    | IntegerRange _
    | Array        _
    | Record       _ -> types#remove (normalize_ident_cur nom)

  in

  let remove_decl_part decl_part =
    (* incomplet *)
    List.iter
      (function (item,_) -> match item with
      | BasicDecl(TypeDecl(id,tdecl,_)) -> remove_typ_decl id tdecl
      | BasicDecl(SubtypDecl(ident,_))-> types#remove(normalize_ident_cur ident)
      | BasicDecl(ObjectDecl(ident_list,_, _, _)) ->
          List.iter
            (fun ident -> remove_cst (normalize_ident_cur ident))
            ident_list
      | BasicDecl(NumberDecl(ident,_))->remove_cst (normalize_ident_cur ident)
      | BasicDecl(RepresentClause _) -> ()
      | BasicDecl(SpecDecl _)        -> ()
      | BasicDecl(UseDecl(_))        -> ()
      | BodyDecl _                   -> ()
      | BasicDecl(RenamingDecl _)    -> ()
      )
    decl_part

  in

  let remove_params subprogram_decl =
    let params = match subprogram_decl with
      | Function (_,param_list,_) -> param_list
      | Procedure(_,param_list)   -> param_list
    in
      Sym.exit_context gtbl;
      List.iter
        (fun param -> remove_cst (normalize_ident_cur param.formal_name))
        params
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
              Sym.s_add_variable gtbl param.formal_name
                                          (subtyp_to_adatyp param.param_type)
              ;
              add_cst (normalize_ident_cur param.formal_name)
                                    (VarSymb false)
                                    false;
           end;
          { Ast.param_type    = normalize_subtyp param.param_type
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
            let norm_subtyp = normalize_subtyp return_type in
              Sym.s_add_subprogram gtbl name
                                       (List.map mk_param param_list)
                                       (Some (subtyp_to_adatyp return_type))
                                       ;
              add_function norm_name (if param_list = [] then
                                        (Some(base_typ norm_subtyp))
                                      else None)
                                     false;
              Ast.Function(norm_name,
                       normalize_params param_list true,
                       norm_subtyp)
        | Procedure(name,param_list) ->
            let norm_name = normalize_ident_cur name in
              Sym.s_add_subprogram gtbl name
                              (List.map mk_param param_list) None;
              add_function norm_name None false;
              Ast.Procedure(norm_name,
                        normalize_params param_list false)
  in

  let rec normalize_basic_decl item loc global reptbl handle_init =
    match item with
    | UseDecl(use_clause) -> Sym.s_add_use gtbl use_clause;
        Some (Ast.UseDecl use_clause)
    | ObjectDecl(ident_list,subtyp_ind,def, Variable) ->
        let t =
          let (tp,_,_,st) = subtyp_ind in
          if st = T.unknown then subtyp_to_adatyp tp
          else st
        in
        let norm_subtyp_ind = normalize_subtyp_indication subtyp_ind in
        begin match def with
        | None -> ()
        | Some exp -> List.iter (fun x -> handle_init x (normalize_exp
                                          ~expected_type:t exp) loc)
                        ident_list
        end;
          (List.iter
             (fun x ->
               Sym.s_add_variable gtbl x t;
               add_cst (normalize_ident_cur x) (VarSymb(global)) global)
             ident_list);
          Some (Ast.ObjectDecl(ident_list, norm_subtyp_ind, Ast.Variable))
    | ObjectDecl(ident_list,subtyp_ind, Some(exp), Constant) ->
        let t =
          let (tp,_,_,st) = subtyp_ind in
          if st = T.unknown then subtyp_to_adatyp tp
          else st
        in
        let normexp = normalize_exp ~expected_type:t exp in
        (* constantes *)
        let norm_subtyp_ind =
          normalize_subtyp_indication subtyp_ind in

        let subtyp = extract_subtyp norm_subtyp_ind in
        let typ = base_typ subtyp in
        let add_ident v x = add_cst (normalize_ident_cur x)
          (StaticConst(v, typ, global)) global in
        let status =
          try
            let value = eval_static normexp (Some typ) csttbl gtbl extern in
              (* on verifie que la valeur obtenue est conforme
                 au sous-type *)
              check_static_subtyp subtyp value;
              List.iter (fun x ->
                Sym.s_add_variable gtbl x t ~value;
                           add_ident value x) ident_list;
              Ast.StaticVal value
          with
            | AmbiguousTypeException -> Npkcontext.report_error
                                        "Ada_normalize.normalize_basic_decl"
                                        "uncaught ambiguous type exception"
            | NonStaticExpression -> List.iter
                                      (fun x ->
                                         let n = (normalize_ident_cur x) in
                                           Sym.s_add_variable gtbl
                                                          x
                                                          t;
                                                 add_cst n
                                                        (VarSymb global)
                                                        global
                                      ) ident_list;
                                      Ast.Constant

        in
          List.iter (fun x -> handle_init x normexp loc) ident_list;
          Some (Ast.ObjectDecl(ident_list, norm_subtyp_ind,status))
    | ObjectDecl(_) ->
        Npkcontext.report_error
          "Ada_normalize.normalize_basic_decl"
          ("internal error : constant without default value"
           ^"or already evaluated")
    | TypeDecl(id,typ_decl,_) ->
        let norm_typ_decl = normalize_typ_decl id typ_decl loc global reptbl
        in Some (Ast.TypeDecl(id,norm_typ_decl))
    | SpecDecl(spec) -> Some (Ast.SpecDecl(normalize_spec spec))
    | NumberDecl(ident, exp) ->
        let norm_exp = normalize_exp exp in
        let v = eval_static_number norm_exp csttbl gtbl extern in
            let t = match v with
              | T.BoolVal  _ | T.IntVal _ -> T.universal_integer
              | T.FloatVal _              -> T.universal_real
            in
            Sym.s_add_variable gtbl ident t;
            add_cst (normalize_ident_cur ident)
                    (Number(v, global))
                    global;
          Some (Ast.NumberDecl(ident, v))
    | SubtypDecl(ident, subtyp_ind) ->
        let norm_subtyp_ind = normalize_subtyp_indication subtyp_ind  in
        let t =
          let (tp,_,_,st) = subtyp_ind in
          if st = T.unknown then subtyp_to_adatyp tp
          else st
        in
          Sym.s_add_type gtbl ident t;
          types#add (normalize_ident_cur ident)
                    (extract_subtyp norm_subtyp_ind)
                    loc
                    global;
          Some (Ast.SubtypDecl(ident, norm_subtyp_ind))
    | RenamingDecl (n, o) ->
        begin
          Sym.add_renaming_decl gtbl n (normalize_name o);
          None;
        end
    | RepresentClause _ -> failwith "NOTREACHED"

  and normalize_package_spec (name, list_decl) :Ast.package_spec =
    Sym.set_current gtbl name;
    Sym.enter_context ~name ~desc:"Package spec" ~weakly:true gtbl;
    let represtbl = Hashtbl.create 50 in
    let list_decl = List.filter (function
                                  | RepresentClause(rep), loc ->
                                      Hashtbl.add represtbl
                                        (extract_representation_clause_name rep)
                                        (rep, loc); false
                                  | _ -> true)
      list_decl in
    let init = ref [] in
    let add_init x exp _loc =
      init := (x,exp)::!init
    in
    let rec normalize_decls decls =
      List_utils.filter_map (fun (decl, loc) ->
                  Npkcontext.set_loc loc;
                  match normalize_basic_decl decl loc true
                        represtbl add_init with
                    | None -> None
                    | Some decl -> Some (decl,loc)
               ) decls in
    let norm_spec = normalize_decls list_decl in
      Sym.reset_current gtbl;
      Sym.exit_context gtbl;
      (name, norm_spec, !init)

  and normalize_spec spec = match spec with
    | SubProgramSpec(subprogr_spec) -> Ast.SubProgramSpec(
          normalize_sub_program_spec subprogr_spec ~addparam:false)
    | PackageSpec(package_spec) ->
        Ast.PackageSpec(normalize_package_spec package_spec)

  in

  let rec normalize_lval = function
    | Lval n -> (Ast.Lval n, Some (Sym.s_find_variable gtbl n))
    | ArrayAccess (lv,e) -> Ast.ArrayAccess(fst (normalize_lval lv),
                                            normalize_exp  e),None
  in

  let rec normalize_instr (instr,loc) =
    Npkcontext.set_loc loc;
    match instr with
    | NullInstr    -> None
    | ReturnSimple -> Some (Ast.ReturnSimple, loc)
    | Assign(lv, exp) -> begin
                           let (lv', t) = normalize_lval lv in
                           Some (Ast.Assign( lv'
                                           , normalize_exp ?expected_type:t exp
                                           , false
                                           ), loc)
                         end
    | Return(exp) -> Some (Ast.Return(normalize_exp exp), loc)
    | If(exp, instr_then, instr_else) ->
        Some
        (Ast.If(normalize_exp exp, normalize_block instr_then,
            normalize_block instr_else), loc)
    | Loop(NoScheme,instrs) -> Some (Ast.Loop(Ast.NoScheme,
                                              normalize_block instrs),loc)
    | Loop(While(exp), instrs) -> Some (Ast.Loop(Ast.While(normalize_exp exp),
                     normalize_block instrs), loc)
    | Loop(For(iter, exp1, exp2, is_rev), block) ->
        let dp = [BasicDecl (ObjectDecl ( [iter]
                             , ( SubtypName (None,"integer")
                               , None
                               , None
                               , T.integer
                               )
                             , Some (if is_rev then exp2 else exp1)
                             , Constant
                             )
                      )
              , loc]
        in
      Sym.enter_context gtbl;
      let (ndp,init) = normalize_decl_part dp ~global:false in
      let nblock = (List.map build_init_stmt init)@normalize_block block in
      let res =
      Some
      (Ast.Block
        ( ndp
        , [Ast.Loop
             ( Ast.While
                ( normalize_exp (if is_rev then Binary(Ge,Var(None,iter),exp1)
                                           else Binary(Le,Var(None,iter),exp2)))
                , nblock@[Ast.Assign ( Ast.Lval (None, iter)
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
        ) , loc) in
      Sym.exit_context gtbl;
      res
    | Exit -> Some (Ast.Exit, loc)
    | ProcedureCall(nom, params) -> Some (Ast.ProcedureCall(normalize_name nom
                                         ,List.map normalize_arg params), loc)
    | Case (e, choices, default) ->
              Some (Ast.Case (normalize_exp e,
                    List.map (function e,block->
                            normalize_exp e,
                            normalize_block block)
                        choices,
                    Ada_utils.may normalize_block default
                    ),loc)
    | Block (dp,blk) -> Sym.enter_context ~desc:"Declare block" gtbl;
                        let (ndp,init) = normalize_decl_part dp ~global:false in
                        let norm_block = normalize_block blk in
                        let init_stmts = List.map build_init_stmt init in
                        remove_decl_part dp;
                        Sym.exit_context gtbl;
                        Some (Ast.Block (ndp, init_stmts@norm_block), loc)

  and normalize_block block =
    List_utils.filter_map normalize_instr block

  and normalize_decl_part decl_part ~global =
    let represtbl = Hashtbl.create 50 in
    let decl_part :(declarative_item*location) list = List.filter (function
        | BasicDecl(RepresentClause(rep)), loc ->
          Hashtbl.add represtbl
            (extract_representation_clause_name rep)
            (rep, loc); false
        | _ -> true
      ) decl_part in
    let initializers = ref [] in
    let add_init x exp loc =
      initializers := (x,exp,loc) :: !initializers
    in
    let normalize_decl_items items =
      List_utils.filter_map (function
        | BasicDecl(basic),loc ->
            begin
              Npkcontext.set_loc loc;
              match normalize_basic_decl basic loc global
                                       represtbl add_init with
                | None -> None
                | Some decl -> Some( Ast.BasicDecl decl,loc)
            end
        | BodyDecl(body),loc ->
            Npkcontext.set_loc loc;
            Some (Ast.BodyDecl(normalize_body body), loc)
      ) items in
    let ndp = normalize_decl_items decl_part in
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
        let (norm_decl_part,init) = normalize_decl_part decl_part
                                                        ~global:false in
        let norm_block = normalize_block block in
        let init_stmts = List.map build_init_stmt init in
          remove_decl_part decl_part;
          remove_params subprog_decl;
          Sym.exit_context gtbl;
          Ast.SubProgramBody( norm_subprog_decl
                            , norm_decl_part
                            , init_stmts@norm_block)
    | PackageBody(name, package_spec, decl_part) ->
        let (nname,nspec,_) = normalize_package_spec
                                (with_default package_spec
                                    (parse_package_specification name)
                              )
        in
          Sym.set_current gtbl name;
          Sym.enter_context ~name ~desc:"Package body" gtbl;
          let (ndp,init) = normalize_decl_part decl_part ~global:true in

          let norm_spec = (nname,nspec,List.map (fun (x,y,_) -> (x,y)) init)
          in

          remove_decl_part decl_part;
          check_package_body_against_spec ~body:ndp ~spec:norm_spec;
          Sym.reset_current gtbl;
          Sym.exit_context gtbl;
          Ast.PackageBody(name, Some norm_spec, ndp)

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
  let add_extern_spec (spec:Ast.spec) =
    let add_extern_basic_decl (basic_decl, loc) =
      Npkcontext.set_loc loc;
      match basic_decl with
        | Ast.TypeDecl(id,typ_decl) ->
            add_extern_typdecl id typ_decl loc
        | Ast.ObjectDecl(ident_list, (tp,_,_,t),
                     (Ast.Variable | Ast.Constant)) ->
            (List.iter
            (fun x -> let n=normalize_ident_cur_ext x true in
                        Sym.s_add_variable gtbl x (if T.is_unknown t then
                          subtyp_to_adatyp tp else t);
                         add_cst n
                                 (VarSymb(true))
                                 true;
               )
               ident_list
            )

        | Ast.ObjectDecl(ident_list,subtyp_ind, Ast.StaticVal v) ->
            (* constante statique *)

            let subtyp = extract_subtyp subtyp_ind in
            let typ = base_typ subtyp in
            let (_,_,_,t) = subtyp_ind in
              List.iter
                (fun x -> let n = normalize_ident_cur_ext x true in
                  Sym.s_add_variable gtbl x t;
                  add_cst n
                   (StaticConst(v, typ, true)) true)
                ident_list

        | Ast.NumberDecl(ident, v) ->
            Sym.s_add_variable gtbl ident T.universal_integer;
            add_cst (normalize_ident_cur_ext ident true)
                    (Number(v, true))
                    true
        | Ast.SpecDecl(Ast.SubProgramSpec
                     (Ast.Function(name, [], return_typ))) ->
            add_function name (Some(base_typ return_typ)) true
        | Ast.SpecDecl(Ast.SubProgramSpec(Ast.Function(name, _, _) |
                                      Ast.Procedure(name, _))) ->
            add_function name None true
        | Ast.SubtypDecl(ident, subtyp_ind) ->
            let subtyp = extract_subtyp subtyp_ind in
              types#add (normalize_ident_cur_ext ident true)
                (*extract_subtyp subtyp_ind*) subtyp
                loc true
        | Ast.SpecDecl _ | Ast.UseDecl  _ -> ()

    in match spec with
      | Ast.SubProgramSpec(Ast.Function(name, [], return_typ)) ->
          add_function name (Some(base_typ return_typ)) true
      | Ast.SubProgramSpec(Ast.Function(name, _, _)|Ast.Procedure(name, _)) ->
          add_function name None true
      | Ast.PackageSpec(name, basic_decls,_) ->
          Sym.set_current gtbl name;
          Sym.enter_context ~name ~desc:"Package spec (extern)"
                            ~weakly:true gtbl;
          List.iter add_extern_basic_decl basic_decls;
          Sym.reset_current gtbl;
          Sym.exit_context gtbl;
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
      | UseContext(n)::r -> Sym.s_add_use gtbl n;
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
