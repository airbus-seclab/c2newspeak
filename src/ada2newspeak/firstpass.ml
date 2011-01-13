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
module A   = AdaSyntax
module T   = AdaTypes
module Sym = Symboltbl

open Ast

let added_spec = ref[]
let add_spec name =  added_spec:=name::!added_spec
(*let f_is_with name = List.mem name !added_spec*)

(** Builds a string from a name *)
let string_of_name = Ada_utils.name_to_string

let concat_resolved_name sc n = match sc with
  | Sym.Lexical      ->        n
  | Sym.In_package p -> (p ^ "." ^ n)

let translate_resolved_name sc n = match sc with
  | Sym.Lexical      -> C.Local         n
  | Sym.In_package p -> C.Global (p ^ "." ^ n)

let translate_nat i =
  C.Const(C.CInt i)

let translate_int x =
  translate_nat (N.Nat.of_int x)

(**
 * Main translating function.
 * Takes an Ada program as and returns a CIR tree.
 *)
let translate compil_unit =
  let extern = ref false in

  let fun_decls = Hashtbl.create 10 in
  let globals   = Hashtbl.create 10 in
  let init      = ref []            in
  let curpkg    = ref None          in

  let translate_name (pack,id) =
    let tr_name = match (pack, !curpkg) with
    | Some p, _      -> [p;id]
    |  _    , Some p -> [p;id]
    | None  , None   ->   [id]
    in
      string_of_name tr_name
  in

  (** Used to generate temporary variables. *)
  let temp =
    object (s)
        val mutable count = 0

        (**
         * Build a fresh identifier.
         *)
        method private new_id =
            let res = count in
            count <- count + 1;
            Temps.to_string res (Temps.Misc "ada_firstpass")

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

  let rec translate_if_exp e_cond e_then e_else =
    let loc = Npkcontext.get_loc () in
    let (tmp, decl, vid) = temp#create loc T.boolean in
    let instr_if = If (e_cond,
           [Assign(Var (Sym.Lexical, tmp, snd e_then), e_then),loc],
           [Assign(Var (Sym.Lexical, tmp, snd e_else), e_else),loc])
    in let tr_instr_if =
        translate_block [(instr_if,loc)]
    in
      C.BlkExp (decl::tr_instr_if,
                 C.Lval (vid, T.translate T.boolean), false)

  and translate_and e1 e2 =
    let loc = Npkcontext.get_loc () in
    let tr_e2 = translate_exp e2 in
    let (_, decl, vid) = temp#create loc T.boolean in
    let assign = C.Set (vid, T.translate T.boolean, tr_e2) in
    let tr_ifexp = translate_if_exp e1
                                    e2
                                    (CBool false,T.boolean) in
      C.BlkExp (decl::(assign,loc)::[C.Exp tr_ifexp,loc]
      , C.Lval (vid, T.translate T.boolean), false)

  and translate_or e1 e2 =
    let loc = Npkcontext.get_loc () in
    let tr_e2 = translate_exp e2 in
    let (_, decl, vid) = temp#create loc T.boolean in
    let assign = C.Set (vid, T.translate T.boolean, tr_e2) in
    let tr_ifexp = translate_if_exp e1
                                    (CBool true,T.boolean)
                                    e2 in
      C.BlkExp (decl::(assign,loc)::[C.Exp tr_ifexp,loc]
      , C.Lval (vid, T.translate T.boolean), false)

  and translate_binop typ op e1 e2 =
      let c1 = translate_exp e1 in
      let c2 = translate_exp e2 in
      match (op,T.translate typ) with
      (* Numeric operations *)
      | Plus ,C.Scalar(N.Int   _) -> C.Binop(N.PlusI   , c1, c2)
      | Plus ,C.Scalar(N.Float n) -> C.Binop(N.PlusF  n, c1, c2)
      | Minus,C.Scalar(N.Int   _) -> C.Binop(N.MinusI  , c1, c2)
      | Minus,C.Scalar(N.Float n) -> C.Binop(N.MinusF n, c1, c2)
      | Mult ,C.Scalar(N.Int   _) -> C.Binop(N.MultI   , c1, c2)
      | Mult ,C.Scalar(N.Float n) -> C.Binop(N.MultF  n, c1, c2)
      | Div  ,C.Scalar(N.Int   _) -> C.Binop(N.DivI    , c1, c2)
      | Div  ,C.Scalar(N.Float n) -> C.Binop(N.DivF   n, c1, c2)
      | Rem  ,C.Scalar(N.Int   _) -> C.Binop(N.Mod     , c1, c2)

      (* Comparisons *)
      | Eq, C.Scalar t -> C.Binop (N.Eq t, c1, c2)
      | Gt, C.Scalar t -> C.Binop (N.Gt t, c1, c2)
      | And, C.Scalar _ -> translate_and e1 e2
      | Or , C.Scalar _ -> translate_or  e1 e2
      | Power , C.Scalar(N.Int _) -> begin
	  match c2 with
	      C.Const (C.CInt n) when
		(compare (Nat.to_int n) 2 = 0) -> 
		  C.Binop(N.MultI, c1, c1)
	    | _ -> Npkcontext.report_error 
		"Firstpass.translate_binop"
		  "run-time operator not implemented  (Power 1)"
	end

      | Power , C.Scalar(N.Float sz) -> begin
	  match c2 with
	      C.Const (C.CInt n) when
		(compare (Nat.to_int n) 2 = 0) -> 
		  C.Binop(N.MultF sz, c1, c1)
	    | _ -> Npkcontext.report_error 
		"Firstpass.translate_binop"
		  "run-time operator not implemented (Power 2)"
	end

     | Mod , _ -> begin
	 Npkcontext.report_warning "Firstpass.translate_binop"
           "run-time operator Mod, no check yet";
	 C.Binop (N.Mod, c1, c2)
       end
         

      (* | Power TO DO test 2nd postif when fst integer cf p.93*\) *)
      | Power  ,_ ->
          Npkcontext.report_error "Firstpass.translate_binop"
           "run-time operator not implemented (Power 3)"

 

      | _ -> Npkcontext.report_error "Firstpass.translate_binop"
            "invalid operator and argument"

  (** Returns ftyp & list of params *)
  and translate_subprogram_parameters (params:Ast.argument list) =
    let translate_parameter (t,arg) =
      let x_t = T.translate t in
      let x_arg = match arg with
      | In  e    -> C.In (T.check_exp t (translate_exp e))
      | Out lv   -> 
	  let (lv, t) = translate_lv lv in
	    C.Out (lv, T.translate t)
      | InOut lv -> 
	  let (lv, t) = translate_lv lv in
(* TODO: shouldn't it be InOut here? Do a test*)
	    C.Out (lv, T.translate t)
      in
      (x_t, x_arg)
    in
    List.split (List.map translate_parameter params)

  (** Translates a function call. *)
  and translate_function_call fname (arg_list:Ast.argument list) rt =
    let (ftyp0, tr_params) =
      translate_subprogram_parameters arg_list
    in
    C.Call((ftyp0, T.translate rt), fname, tr_params)

  and translate_lv lv =
      match lv with
        | Var (sc,lv,t) ->
            let clv = translate_resolved_name sc lv in
            (clv, t)
        | ArrayAccess (lv, exps) ->
            let (x_lv,t_lv ) = translate_lv lv in
            let x_exps = List.map translate_exp exps in
            let (tc, ti) = T.extract_array_types t_lv in
            let size_c = C.size_of_typ (T.translate tc) in

            let tyexl = List.map2 (fun x y -> x,y) ti x_exps in

            let exp_offset = List.fold_left (
	      fun old_off (ty, exp) ->
		(* FIXME rebase and check exp *)
		let base = T.extract_base ty in
		let exp' = T.check_exp ty exp in
		let rebased_exp = if (base = Nat.zero) then exp'
		else C.Binop ( N.MinusI
				 , exp'
				   , (translate_nat base)
			     )
		in
		  if old_off = translate_nat (Nat.zero) then rebased_exp
		  else
		    C.Binop ( N.PlusI
				, rebased_exp
				  , C.Binop( N.MultI
					       , translate_nat (T.length_of ty)
						 , old_off
					   )
			    )
	    ) 
	      (translate_nat Nat.zero) tyexl 
	    in
            let offset = C.Binop( N.MultI
                                    , exp_offset
                                      , translate_int size_c
                                ) in
              C.Shift (x_lv, offset), tc
        | RecordAccess (lv, off_pos, tf) ->
            let (record, _) = translate_lv lv in
            let offset = translate_int off_pos in
            C.Shift (record, offset), tf
        | PtrDeref (lv, t) ->
            let (xlv, _) = translate_lv lv in
            let xt = T.translate t in
            C.Deref (C.Lval (xlv, xt), xt), t
	      
	| BlkLval ( block, lv ) -> 
	    let trans_blk   = translate_block block in
	    let trans_lv, t = translate_lv lv in
	      C.BlkLv (trans_blk, trans_lv, false), t


  and translate_exp (exp,typ) :C.exp =
    match exp with
    | CFloat f -> C.Const(C.CFloat(f,string_of_float f))
    | CInt   i -> translate_nat i
    | CChar  c -> translate_nat (Nat.of_int c)
    | CBool  b -> translate_nat (Ada_utils.nat_of_bool b)
    | Lval   l -> let (tlv,tp) = translate_lv l in
                  C.Lval (tlv, T.translate tp)
    | Not    exp              -> C.Unop (Npkil.Not, translate_exp exp)
    | Binary(binop,exp1,exp2) ->
        let bop = translate_binop (snd exp1) binop exp1 exp2 in
        (T.check_exp typ bop)
    | CondExp(e1,e2,e3)       -> translate_if_exp e1 e2 e3
    | FunctionCall(sc, name, arg_list, rt) ->
        let fname = C.Fname (concat_resolved_name sc name) in
        translate_function_call fname arg_list rt
    | AddressOf (lv, _) ->
        let (lv', tlv) = translate_lv lv in
        C.AddrOf(lv', T.translate tlv)
    | Cast (o, n, e) -> begin
	let sc_o = C.scalar_of_typ  (T.translate o) in
	let sc_n = C.scalar_of_typ  (T.translate  n) in
	 (* translate -> cir.typ *)
	 (*  Unop (Npkil.Coerce (Newspeak.domain_of_typ k), e) *)
	  match sc_o, sc_n with 
	       (Newspeak.Int _, Newspeak.Int _) -> 
		 (*	C.Unop (Npkil.Coerce (Newspeak.domain_of_typ k),  *)
		   translate_exp (e,n)
		 (* ) *)
	    | _ -> C.Unop ( Npkil.Cast (sc_o, sc_n),  
			    translate_exp (e,n)
			  ) 
      end
    | BlkExp (block , exp) -> (*with exp Ast.Lval (Ast.RecordAccess_)*)
	let trans_blk = translate_block block in
	let trans_exp = translate_exp exp in
	  C.BlkExp (trans_blk, trans_exp, false)

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
    let tr_exp = translate_exp exp in
      match tr_exp, tr_lv  with
	  C.BlkExp ( [(C.Block (dcle::(affe::[]), None), _)], tt_exp, false) ,
	  C.BlkLv  ( [(C.Block (dcll::(affl::[]), None), _)], tt_lv , false)  ->
	    C.Block ( dcll::(dcle::(affl::(affe::
		         [make_affect tt_lv tt_exp subtyp_lv loc])))
		    , None
		    ), loc
	      
	| C.BlkExp ( [(C.Block (dcle::(affe::[]), None), _)], tt_exp, false), _ ->
	    C.Block ( dcle::(affe::[make_affect tr_lv tt_exp subtyp_lv loc])
		    , None
		    ), loc
	      
	|  _ , C.BlkLv ( [(C.Block (dcll::(affl::[]), None), _)], tt_lv, false) ->
	    C.Block ( dcll::(affl::[make_affect tt_lv tr_exp subtyp_lv loc])
		    , None
		    ), loc

	| _ -> make_affect tr_lv tr_exp subtyp_lv loc


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
               let tr_exp = translate_exp condition in
                 let tr_then = translate_block instr_then in
                 let tr_else = translate_block instr_else in
                     (C.build_if loc (tr_exp, tr_then, tr_else))
                   @ (translate_block r)
           | Loop(NoScheme, body) ->
               let tr_body = translate_block body in
                 (C.Block([C.Loop(tr_body), loc], Some Params.brk_lbl),loc)
                 ::(translate_block r)
           | Loop(While(cond), body) ->
               translate_block
                 ((Loop(NoScheme,(If(cond,[],[Exit,loc]),loc)::body),
                   loc)::r)
           | ProcedureCall (sc, name, args) -> begin
               let fname = C.Fname (concat_resolved_name sc name) in
                 let (ftyp0, tr_args) =
                   translate_subprogram_parameters args in
                 let ftyp = ftyp0, C.Void in
                 (C.Exp(C.Call(ftyp, fname, tr_args)), loc)
                 ::(translate_block r)
             end

           | Case (e, choices, default) ->
                   (C.Switch(translate_exp e,
                         (List.map (function exp,block ->
                             let value = translate_exp exp in
                             ( value
                             , C.scalar_of_typ (T.translate (snd exp))
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

  and translate_param_list param_list =
    (List.map (fun p -> T.translate p.param_type) param_list)

  and add_params spec =
    let params = List.map (fun x -> x.formal_name) spec.arguments in
      (Params.ret_ident, params)

  and translate_sub_program_spec spec =
    let params_typ = translate_param_list spec.arguments in
    let return_typ =
      match spec.return_type with
	| None    -> C.Void
	| Some st -> T.translate st
    in
      (params_typ, return_typ)

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
		      let (ndecls,niblk) = translate_declarative_item dp in
			(decls @ ndecls),
		      (iblk  @ niblk )
		   ) ([],[]) decl_part
      
  and add_funbody subprogspec block loc =
    let (_, args_ids) = add_params subprogspec in
    let ftyp = translate_sub_program_spec subprogspec in
    let body = translate_block block in
    let mangle_sname = function
      | []       -> Npkcontext.report_error "fstpass:mangle_sname" "unreachable"
      | x::[]    -> None  , x
      | x::y::[] -> Some x, y
      | _        -> Npkcontext.report_error "mangle_sname"
                      "chain of selected names is too deep"
    in
    let body = (C.Block (body, Some Params.ret_lbl), loc)::[] in
    let declaration = 
      {
	C.arg_identifiers = args_ids;
	C.function_type = ftyp;
	C.body = body;
	(* TODO: put the position of the function start *)
	C.position = N.unknown_loc
      }
    in
      Hashtbl.replace fun_decls (translate_name (mangle_sname subprogspec.name))
        declaration
  in

  let add_global loc tr_typ i x =
    let tr_name = translate_name (!curpkg,x) in
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
      | ObjectDecl(ident, subtyp, _, init_o) -> begin
	  let tr_typ = T.translate subtyp in
          let init =
            if !extern then None else init_o
          in
	    add_global loc tr_typ init ident
	    
	end
      | SpecDecl spec -> translate_spec spec
      | NumberDecl _ -> ()

  and translate_global_decl_item (item,loc) =
    Npkcontext.set_loc loc;
    match item with
      | BasicDecl(basic) ->
          translate_global_basic_declaration (basic, loc)
      | BodyDecl(body) -> translate_body body loc
	  
  and translate_spec spec = 
    match spec with
      | SubProgramSpec(subprog_spec) -> 
          ignore (translate_sub_program_spec subprog_spec )
      | PackageSpec (nom, basic_decl_list) ->
	  curpkg := Some nom;
	  List.iter translate_global_basic_declaration basic_decl_list;
	  add_spec nom;
          curpkg := None

  and translate_body body loc =
    Npkcontext.set_loc loc;
    match body with
      | SubProgramBody(subprog_decl, block) ->
          add_funbody subprog_decl block loc
      | PackageBody(name, package_spec, decl_part) ->
	  curpkg := Some name;
          (match package_spec with
             | None -> ()
             | Some(_, basic_decls) ->	  
	         List.iter 
		   translate_global_basic_declaration 
		   basic_decls;
	  );
          List.iter translate_global_decl_item decl_part;
  in

  let translate_library_item lib_item loc =
    Npkcontext.set_loc loc;
    match lib_item with
      | Body body -> translate_body body loc
      | Spec _    -> Npkcontext.report_error
            "Firstpass.translate_library_item"
            "Rien a faire pour les specifications"
  in

  let translate_context (content, location) =
    Npkcontext.set_loc location;
    translate_spec content
  in

  let (ctx, lib_item, loc) = compil_unit in
    Npkcontext.set_loc loc;
    extern := true;
    List.iter translate_context ctx;
    extern := false;
    translate_library_item lib_item loc;
    Npkcontext.forget_loc ();
    { C.globals = globals; C.init = !init; C.fundecs = fun_decls }

