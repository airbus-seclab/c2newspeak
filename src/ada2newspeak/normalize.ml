(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal 
  language  well-suited for static analysis.
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

open AdaSyntax
open Ada_utils

module Nat = Newspeak.Nat
module  T  = AdaTypes
module TC  = Typecheck
module Sym = Symboltbl

let (%+) = Nat.add
let (%-) = Nat.sub

let set_package = ref None

let gtbl = Sym.create ()

let spec_tbl = Hashtbl.create 5

(*Normalization returns a list of files which body
  has to be compiled and normalized
*)
let body_tbl = ref []

let init_bodies bds = body_tbl := bds

let bodies_to_add () = !body_tbl

(* Name-related functions *)

let name_of_sp_spec spec =
  match spec with
    | Subprogram (x,_,_) -> x

let name_of_synt_spec spec = 
  match spec with 
    | SubprogramSpec sps -> name_of_sp_spec sps
    | PackageSpec (x,_) -> x

let compilation_unit_name (_, library_item, _) =
  match library_item with
    | Spec s -> name_of_synt_spec s
    | Body (SubprogramBody (s, _, _)) -> name_of_sp_spec s
    | Body (PackageBody (n, _, _)) -> n

let mangle_sname names =
  match names with 
  | []       -> Npkcontext.report_error "mangle_sname" 
                                        "unreachable"
  | x::[]    -> None  , x
  | x::y::[] -> Some x, y
  | _        -> Npkcontext.report_error "mangle_sname"
                  "chain of selected names is too deep"

let add_p (pk, o) = 
  match pk with  
    | None -> Sym.current gtbl, o
    | Some _ -> pk,o
  
let subtyp_to_adatyp gtbl n = 
  let n' = mangle_sname n in
  try
    snd (Symboltbl.find_type gtbl n')
  with Not_found -> 
    begin
      Npkcontext.report_warning "ST2AT"
        ( "Cannot find type '"
        ^ name_to_string n
        ^ "'");
      T.new_unknown "Cannot find type (subtyp_to_adatyp)";
    end

let merge_types gtbl (tp, cstr) =
  match tp with 
      [] -> (*Hack only for the for x in  1..z case, no integer*)
	T.universal_integer 
    | _ ->
	let t = subtyp_to_adatyp gtbl tp in
	  if (T.is_unknown t) then
	    Npkcontext.report_warning "merge_types"
	      ("merged subtype indication into unknown type (" ^ T.get_reason t ^ ")");  
	  let t = 
	    match T.extract_symbols t with 
		Some enums ->  
		  if (T.is_boolean t) then t else T.new_enum t enums 
	      | _ ->  t
	  in      
	    match cstr with
	      | None -> t
	      | Some c -> T.new_constr t c
		  
let subtyp_to_adatyp st = subtyp_to_adatyp gtbl st
let merge_types sti = merge_types gtbl sti

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
	let (sc,(act_id, t, ro)) = 
	 Sym.find_variable_with_error_report ?expected_type gtbl (pkg,id) 
	in
	  SelectedVar(sc,act_id,t,ro)
      with
      | Sym.Parameterless_function (sc, n, rt) -> 
	  (* Il se peut cependant qu'il y ait renaming cf t470*)
	    let scp = match sc with Sym.In_package p -> Some p | _ -> None in
	    let nsc = scp, n in

	  let (sc_ren,(act_n, _,_)) = 
	    Sym.find_subprogram 
	      ~silent:true gtbl (add_p nsc)  [] (Some rt)
	      (*expected_type*)
	      (fun x -> Symboltbl.find_type gtbl x) 
	  in
	    SelectedFCall( sc_ren , act_n , rt)
	    
      | Sym.Variable_no_storage (t,v) -> SelectedConst (t, v)
    end
  in
   match n with
     | SName (Var pfx, fld) -> begin
         try
           let (_, (act_id, t, _)) = Sym.find_variable gtbl (None, pfx) in
           let (off, tf) = T.record_field t fld in
           let lv = Ast.Var (Sym.Lexical, act_id, t) in
             SelectedRecord (lv , off, tf)
     	 with Not_found -> resolve_variable (Some pfx) fld
       end
     | Var id ->  resolve_variable None id
     | SName (SName (Var _, _) as pf, z) -> begin
	 match (resolve_selected pf) with
           | SelectedFCall _ 
           | SelectedVar   _ 
           | SelectedConst _ -> Npkcontext.report_error "resolve_selected"
               "bad type for selected value field"
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
  let match_ok s b = 
    match (s,b) with
      | Ast.SpecDecl(Ast.SubProgramSpec sps),
	Ast.BodyDecl(Ast.SubProgramBody (spsb, _)) -> sps = spsb
      | Ast.ObjectDecl _ as x, Ast.BasicDecl (Ast.ObjectDecl _ as y) -> x = y
      | _ -> false
  in
    List.exists (function bd -> match_ok specification bd) bodylist
	
let check_package_body_against_spec ~body ~spec =
  let (pkgname,spec_and_loc) = spec in
  let (        body_and_loc) = body in
  let speclist = List.map fst spec_and_loc in
  let bodylist = List.map fst body_and_loc in
  (* Filter on specifications : only sp such as
   * filterspec sp = true will be checked.      *)
  let filterspec basicdecl =
    match basicdecl with 
    | Ast.NumberDecl _ | Ast.SpecDecl _ -> true
    | Ast.ObjectDecl _ -> false
  in
  List.iter (function sp ->
    if (filterspec sp) then
      begin
        if not (find_body_for_spec ~specification:sp ~bodylist)
        then Npkcontext.report_error "Ada_utils.check_package_body_against_spec"
          ( "Body for package " ^ pkgname
          ^ " does not match its specification : cannot find a body for \""
          ^ Ast.name_of_spec sp ^ "\"")
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

let extract_subprog_spec ast =
    match ast with
      | (context, Body(SubprogramBody(spec,_,_)), loc) ->
        (context, Spec(SubprogramSpec(spec)),     loc)
      | (_, Spec _, _) -> Npkcontext.report_error
          "Ada_normalize.extract_subprog_spec"
        "body expected, specification found"
      | (_, Body(PackageBody _), _) -> Npkcontext.report_error
          "Ada_normalize.extract_subprog_spec"
        "subprogram body expected, package body found"

let parse_specification name =
  let spec_name = name ^ ".ads" in
  let body_name = name ^ ".adb" in
  let spec_ast =
    if Sys.file_exists spec_name
    then begin
      if ((Sys.file_exists body_name) && 
	    (not ( List.mem body_name !body_tbl))) 
      then body_tbl := body_name::!body_tbl
      ;

      let res = File_parse.parse spec_name in
	if (!Npkcontext.verb_ast) then begin
          print_endline "Abstract Syntax Tree (extern)";
          print_endline "-----------------------------";
          Print_syntax_ada.print_ast [res];
          print_newline ();
 	end;
	res
    end
    else
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
    | (_, Spec(SubprogramSpec _),_) ->
                Npkcontext.report_error
                   "Ada_normalize.parse_package_specification"
                  ( "package specification expected, "
                  ^ "subprogram specification found")
    | (_, Body _, _) -> Npkcontext.report_error
           "normalize.parse_package_specification"
          "internal error : specification expected, body found"

let rec normalize_exp ?expected_type exp =
  match exp with
    | CInt   x -> Ast.CInt   x, (match expected_type with
                                | Some t -> t
                                | None -> T.universal_integer
                                )
    | CFloat x -> Ast.CFloat x,T.universal_real
    | CChar  x -> Ast.CChar  x,T.character

    | Lval(ParExp(n, params)) -> 
	  normalize_fcall (n, params) expected_type
	
    | Lval(PtrDeref _ as lv) -> 
	let nlv, tlv = normalize_lval lv in
          Ast.Lval (nlv), tlv
  
    | Lval (SName (ParExp(Var n, params), fld)) -> 
	begin
	  (*The None here might be error in tbl_find_subprogram*)
	  let fcall = normalize_fcall(Var n, params) None in
	    match fcall with
		(Ast.Lval lval, t) ->
		  let (off, tf) = T.record_field t fld in
		    Ast.Lval(Ast.RecordAccess (lval , off, tf)), tf
		      
	      |  Ast.FunctionCall (_sc, fid, _args, _rett), t -> 	
		   let (off, tf) = T.record_field t fld in
		   let x = Temps.to_string 0 (Temps.Value_of fid) in
		   let loc = Npkcontext.get_loc () in 
		     Sym.add_variable gtbl x loc t;
		     let bd     = Ast.ObjectDecl (x, t, Ast.Variable, None) in 
		     let lv_aff = Ast.Var ( Sym.Lexical, x, t) in
		     let exp_aff = fst fcall, t in 
		     let affect = [Ast.Assign ( lv_aff, exp_aff ), loc] in 
		     let instr = Ast.Block ([Ast.BasicDecl bd, loc], affect) in
		     let lv = Ast.Var (Sym.Lexical, x, t) in
		     let expr   = Ast.Lval (Ast.RecordAccess (lv , off, tf)) in
		       Ast.BlkExp ( [instr, loc], (expr, tf)), tf
			 
	      | _ ->  Npkcontext.report_error " Normalize_exp"
		  " Result of fcall not handled"
	end
	  
    | Lval (SName ( SName ( ParExp(Var n, params), fld ), fld2 )) -> 
	begin
	  let n_exp = normalize_exp(Lval(SName(ParExp(Var n,params),fld))) in
	    match n_exp  with  
		(Ast.Lval nlv,  t) ->  
		  let (off, tf) = T.record_field t fld2 in
		    Ast.Lval(Ast.RecordAccess (nlv , off, tf)), tf
		      
	      | Ast.BlkExp ( [instr, loc], ( Ast.Lval lvalue, t)), _t ->
		  let (off, tf) = T.record_field t fld2 in
		  let expr = Ast.Lval (Ast.RecordAccess (lvalue, off, tf)) in
		    Ast.BlkExp ( [instr, loc], (expr, tf)), tf
		      
	      | Ast.BlkExp _,_ ->  Npkcontext.report_error
		  "normalize_exp" 
		    "not a Record: unexpected case"
		    
	      | _ -> Npkcontext.report_error "normalize_exp" "not a Lval"
	end
	  
    | Lval (SName  (SName ( SName ( x, fld ), fld2 ), fld3)) ->
	begin 
	  let n_exp = normalize_exp( Lval( SName ( SName ( x, fld ), fld2 ))) 
	  in   
	    match n_exp  with  
		(Ast.Lval nlv, t) ->  
		  let (off, tf) = T.record_field t fld3 in
		    Ast.Lval(Ast.RecordAccess (nlv , off, tf)), tf
	      
	      | Ast.BlkExp ( [instr, loc], ( Ast.Lval lvalue, t)), _t ->
		  let (off, tf) = T.record_field t fld3 in
		  let expr = Ast.Lval (Ast.RecordAccess (lvalue, off, tf)) in
		    Ast.BlkExp ( [instr, loc], (expr, tf)), tf
	  
	      | _ -> Npkcontext.report_error "normalize_exp" "unexpected call in case with fld3 "
	end


    | Lval lv ->  
        begin
          match resolve_selected ?expected_type lv with
          | SelectedVar (sc, id,  t, _) ->  
	       Ast.Lval(Ast.Var(sc,id,t)) ,t 
          | SelectedFCall (sc, id, rt) -> Ast.FunctionCall (sc, id, [], rt), rt
          | SelectedConst (t,v) -> insert_constant ~t v
          | SelectedRecord (lv, off, tf) ->
              Ast.Lval(Ast.RecordAccess(lv, off, tf)), tf
        end
    | Unary (uop, exp)    -> normalize_uop uop exp
    | Binary(bop, e1, e2) -> normalize_binop bop e1 e2  expected_type
    | Qualified(lv, exp) -> 
	let stn = Symboltbl.make_name_of_lval lv in
        let t = subtyp_to_adatyp stn in
	let (e, _) = normalize_exp ~expected_type:t exp in
          (e, t)
    | Attribute (lv , "address", None) -> begin
	try  
	  let (nlv, tlv) = normalize_lval lv in
	    (Ast.AddressOf (nlv, tlv), T.system_address)
	with _ -> 
	  Npkcontext.report_warning 
	    "UNSOUND .... normalize_attribute 'address'" 
	    "UNSOUND .....normalize_attribute 'address' ";
	  (Ast.CInt (Newspeak.Nat.zero) , T.system_address)
      end
        
    | Attribute (lv, attr, Some exp) -> begin
        let st = Symboltbl.make_name_of_lval lv in
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
                          ("No such function-attribute : '" ^ attr ^ "'")
        end
    | Attribute (lv, attr, None) -> begin 
          let st = Symboltbl.make_name_of_lval lv in
	  let typ = 
	    try 
	      let n = mangle_sname st in
	      let (_,(_,t,_)) = Sym.find_variable gtbl n in
		t
	    with Not_found -> 
	      subtyp_to_adatyp st 
	  in
	  let (exp,t') = T.attr_get typ attr in
          let (exp',_) = normalize_exp exp in
	    (exp',t')
	   
      end
    | Aggregate _ ->
        Npkcontext.report_error "normalize_exp"
          "Array aggregate found without direct lvalue"
 

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
and make_arg_list args spec =
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
      | []   -> []
      | (Some  _, _)::_  ->
	  let process_argument (x, e) =
	    match x with
              | None -> 
		  Npkcontext.report_error "normalize.fcall"
                    "Named parameters shall follow positional ones"
              | Some id when Hashtbl.mem argtbl id ->
                  Npkcontext.report_error "normalize.fcall"
                    ("Parameter " ^ id ^ " appears twice")
	      | Some id -> Hashtbl.add argtbl id e
	  in
            (* don't stop at first named argument : populate argtbl *)
            List.iter process_argument ar;
            []
      | (None, e)::tl -> e::(extract_positional_parameters tl)
  in

  let make_arg arg exp =
    let mode = match (arg.mode, fst exp) with
    | In    , _          -> Ast.In   exp
    | Out   , Ast.Lval l -> Ast.Out   l
    | InOut , Ast.Lval l -> Ast.InOut l
    | _ -> Npkcontext.report_error "make_arg_copy"
                  ( "Actual parameter with \"out\" or \"in out\" mode "
                  ^ "must be a left-value")
    in
      (subtyp_to_adatyp arg.param_type, mode)
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
                                   (spec     :     param      list)
      :Ast.argument list =
          match pos_list, spec with
            |  [],_  -> (* end of positional parameters *)
	         List.map (function x ->
                   let value =
                     ( try Hashtbl.find argtbl x.formal_name
                       with Not_found ->
                         match x.default_value with
                           | Some value -> normalize_exp value
                           | None -> Npkcontext.report_error
                               "normalize.fcall"
                                 ( "No value provided for "
                                   ^ "parameter " ^ x.formal_name
                                   ^ ", which has no default one.")
                     ) in
                     make_arg x value) spec
            | (ev,_)::pt,s::st ->
		let t = subtyp_to_adatyp s.param_type in
                  (make_arg s (ev,t))::(merge_with_specification pt st)
            | _::_,[]     -> Npkcontext.report_error "normalize.function_call"
                            "Too many actual arguments in function call"
  in
    (* Step 1... *)
  let pos      = extract_positional_parameters args in
    (* Step 2... *)
  let eff_args = merge_with_specification pos spec in
    eff_args

(**
 * Normalize an actual argument.
 * The identifier does not have to be normalized (it is just a plain string),
 * but normalize the expression.
 *)
and normalize_arg (id,e) = id,normalize_exp e

and normalize_binop bop e1 e2 xpec =
  let direct_op_trans =
    function
      | Plus  -> Ast.Plus  | Minus -> Ast.Minus | Div   -> Ast.Div
      | Mult  -> Ast.Mult  | Or    -> Ast.Or    | And   -> Ast.And
      | Gt    -> Ast.Gt    | Eq    -> Ast.Eq    | Rem   -> Ast.Rem
      | Mod   -> Ast.Mod   | Power -> Ast.Power
      |_ -> invalid_arg "direct_op_trans"
  in
    match bop with
	(* Operators that does not exist in AST *)
      | Lt     -> normalize_exp (          Binary(Gt, e2, e1) )
      | Le     -> normalize_exp (Unary(Not,Binary(Gt, e1, e2)))
      | Ge     -> normalize_exp (Unary(Not,Binary(Gt, e2, e1)))
      | Neq    -> normalize_exp (Unary(Not,Binary(Eq, e1, e2)))
      | Xor    -> 
	  let (e1',t1) = normalize_exp e1 in
          let (e2',t2) = normalize_exp e2 in
            Ast.CondExp ((e1',t1)
                           ,(Ast.Not(e2',t2),t2)
                             ,(e2',t2)
                        )
              ,TC.type_of_xor t1 t2
      | OrElse -> 
	  let (e1',t1) = normalize_exp e1 in
          let (e2',t2) = normalize_exp e2 in
            Ast.CondExp ((e1',t1)
                           ,(Ast.CBool true,T.boolean)
                             ,(e2',t2)
                        )
              ,TC.type_of_binop Ast.Or t1 t2
      | AndThen -> 
	  let (e1',t1) = normalize_exp e1 in
          let (e2',t2) = normalize_exp e2 in
            Ast.CondExp ((e1',t1)
                           ,(e2',t2)
                             ,(Ast.CBool false,T.boolean)
                        )
              ,TC.type_of_binop Ast.And t1 t2
		
      (* Otherwise : direct translation *)		
      | _ ->  
	
	  let bop' = direct_op_trans bop in
	  let n = Ada_utils.make_operator_name bop in
	  let expected_type =
	    match (e1, e2) with
	      | Lval l1, Lval l2 -> 
		  Sym.get_possible_common_type gtbl l1 l2
	      | _ , Qualified (lvn,_) -> 
		  let n = Symboltbl.make_name_of_lval lvn in
		    Some (subtyp_to_adatyp n)
	      | _ -> None
	  in
	  let (e1',t1) = normalize_exp ?expected_type e1 in
	  let (e2',t2) = normalize_exp ?expected_type e2 in
	    if ( Sym.is_operator_overloaded gtbl n) then
	      begin 
		(* No Expected type : might raise error in typecheck *)
		let norm_args = [(None, (e1',t1));(None, (e2',t2))] in 
		let norm_typs = [(None, t1);(None, t2)] in
		  try
		    let packg = match (Sym.current gtbl) with 
			Some p -> Some p
		      | None -> !set_package
		    in
		    let (sc,( act_name, spec, top)) =
		      Sym.find_subprogram ~silent:true gtbl (packg, n) norm_typs xpec
	      		(fun x -> Symboltbl.find_type gtbl x)
	      	    in 

		    let t = match top with	
		      | None -> Npkcontext.report_error 
			  "normalize_binop"
	      		  "Expected function, got procedure"
	      		    
		      | Some top -> top
	      	    in
		    let effective_args = make_arg_list norm_args spec 
		    in
		      Ast.FunctionCall(sc, act_name, effective_args, t), t
			
		  with Not_found ->
		  (*Overloaded but no renaming matches expected spec*)
		    let tc = TC.type_of_binop bop' t1 t2  in
		    Ast.Binary (bop', (e1',t1), (e2',t2)), tc
	      end
	    else  begin
	      let tc = TC.type_of_binop bop' t1 t2  in
		Ast.Binary (bop', (e1',t1), (e2',t2)), tc
	    end
		      
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

and normalize_fcall (n, params) expectedtype =
  (* Maybe this indexed expression is an array-value *)
  let n = Symboltbl.make_name_of_lval n in
  let n = mangle_sname n in
    try
      let norm_args = List.map normalize_arg params in
      let norm_typs = List.map (fun (s,(_,t)) -> (s,t)) norm_args in
	
      let (sc,(act_name,spec,top)) = 
	Sym.find_subprogram 
	  ~silent:true gtbl (add_p n) norm_typs 
	  expectedtype (fun x -> Symboltbl.find_type gtbl x) 
	in
      let t = match top with 
	| None -> Npkcontext.report_error "normalize_fcall"
            "Expected function, got procedure"
	| Some top -> top
      in
		    
      let effective_args = make_arg_list norm_args spec in
	
	Ast.FunctionCall(sc, act_name, effective_args, t),t

    with Not_found ->
      try 
	(*To do double checked because conversion de tablo
	  contraint/non contraint cf 8.2 *)
	begin
	  let (sc,(act_id, t,_)) = 
	    Sym.find_variable_with_error_report gtbl n 
	  in
	  let tc = fst (T.extract_array_types t) in
	  let params' = List.map snd params in
	  let lv = Ast.Var (sc, act_id, t) in
	    Ast.Lval (Ast.ArrayAccess(lv, List.map normalize_exp params'))
	      ,tc
	end
      with Invalid_argument _ | Not_found ->
	try (*could be 'lv.field(1)', field a record field 
	      (with array type) see t424
	    *)
	  match n with 
	    | Some f, fld -> 
		let (sc,(act_name, t,_)) = 
		  Sym.find_variable_with_error_report gtbl (None,f) 
		in
		  if (T.is_record t) then 
		    let (off, tf) = T.record_field t fld in	
		      if (T.is_array tf) then 
			let tc = fst (T.extract_array_types tf) in
			let params' = List.map snd params in
			  if (compare (List.length params') 1 = 0) then
			    Ast.Lval (
			      Ast.ArrayAccess(  
				Ast.RecordAccess
				  ( Ast.Var(sc, act_name, t)
				      , off
					, tf
				  )
				  , List.map normalize_exp params'
			      )
			    ), tc
			  else 
			    Npkcontext.report_error "normalize_fcall"
			      " f.ret(1) case, only handled for a 1-D array"
		      else raise Not_found
		  else raise Not_found
		    
	    | _ -> raise Not_found
		
	with _ ->   (*Variable not found in gtbl could be a 'cast'*)
	  try
      	    let   (_, cast_t) = Sym.find_type gtbl n in
      	      if (compare (List.length params) 1 <> 0) then    
      		Npkcontext.report_error "normalize_fcall"
      		  "Cast only handled for single variable"
	      ;
      	      let param =  List.hd params in
		(*Il y a cast, la valeur castée ne doit 
		  pas etre checke cf t457*)
      	      let  (norm_exp, arg_t)= normalize_exp (snd param) in
      		
		if (not (T.is_compatible cast_t arg_t))
		then 
		  (*WG  TO DO print_warning *)
		  print_endline  ( "\nL = "
      				   ^ T.print cast_t
      				   ^ "\nR = "
      				   ^ T.print arg_t
      				 )
		;
		(*norm_exp, cast_t*)
		Ast.Cast (arg_t, cast_t, norm_exp), cast_t
		 
      	  with _ ->  
	    (*See t437: "=" undefined operator case*)
	    if ( (compare (make_operator_name Eq)  (snd n) = 0) &&
		 (compare (List.length params)  2 = 0 )
	       ) then
	      begin 
		Npkcontext.report_warning 
		  "normalize_fcall" "Be carefull binary operator not explicitely declared"; 
		let n_args = List.map normalize_arg params in
		let el1 = snd (List.nth n_args 0) in 
		let el2 = snd (List.nth n_args 1) in 
		  Ast.Binary (Ast.Eq,  el1,  el2 ) , T.boolean
	      end
	    else 
		Npkcontext.report_error "normalize_fcall" "Function not found"
	    
	      
and eval_range (exp1, exp2) =
  let norm_exp1 = normalize_exp exp1
  and norm_exp2 = normalize_exp exp2 in
    (* on essaye d'evaluer les bornes *)
    (try
       let val1 = Eval.eval_static norm_exp1 gtbl in
       let val2 = Eval.eval_static norm_exp2 gtbl in
       let contrainte =  match (val1, val2) with
	 | (T.FloatVal f1, T.FloatVal f2) when f1 <= f2 ->
	     FloatRangeConstraint (f1, f2)

	 | (T.FloatVal _, T.FloatVal _) ->          
             Npkcontext.report_error
               "Ada_normalize.normalize_contrainte"
               "null range not accepted"	     
	       
	 | (T.IntVal i1, T.IntVal i2) when  (Nat.compare i1 i2) <= 0 ->
             IntegerRangeConstraint(i1, i2)
   
	 | (T.IntVal _, T.IntVal _) -> 
	     Npkcontext.report_error
               "Ada_normalize.normalize_contrainte"
               "null range not accepted"
	      
         | (T.BoolVal b1, T.BoolVal b2) when  b1 <= b2 ->
             let i1 = nat_of_bool b1
             and i2 = nat_of_bool b2 
	     in
               IntegerRangeConstraint(i1, i2)
         | (T.BoolVal _, T.BoolVal _) ->
             Npkcontext.report_error
               "Ada_normalize.normalize_contrainte"
               "null range not accepted"

         | _ ->
             (* ce cas n'est pas cense se produire :
                on a verifie que les deux bornes sont de meme
                type.*)
             Npkcontext.report_error
               "Ada_normalize.normalize_contrainte"
               ( "internal error : range error : expected static "
               ^ "float or integer constant")
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
                   ~value:(T.IntVal v) ~no_storage:true
		) symbs;
      ()
  | DerivedType subtyp_ind ->
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
  | Digits d ->
      let t = T.new_float d in
      Sym.add_type gtbl ident loc t

and add_representation_clause id aggr loc =
  Npkcontext.set_loc loc;
  let t = subtyp_to_adatyp [id] in
  let assoc_list : (Newspeak.Nat.t * Newspeak.Nat.t) list =
      List.map (fun (i, exp) ->
        let orgn_val =
          try
            let (_,(_,_,x,_)) = 
	      Sym.find_variable_value ~expected_type:t gtbl (None,i)
            in 
	      x
          with Sym.Variable_no_storage (_,x) -> Some x
        in
        let original = match orgn_val with
          | None -> Npkcontext.report_error "normalize:repclause"
                      ("No value found for key '" ^ i ^ "'")
          | Some x -> x
        in
        let exp' = normalize_exp exp in
        let value = Eval.eval_static exp' gtbl in
        match (original, value) with
        | T.IntVal a, T.IntVal b -> (a,b)
        | _ -> Npkcontext.report_error "representation_clause"
                 "Expected an integer value for representation clause"
    ) aggr in
  let repr_newtyp = T.handle_enum_repr_clause t assoc_list in

    (*Replacing the type in the global value*)
    Symboltbl.replace_type gtbl id repr_newtyp;

    List.iter (fun x -> 
		 Symboltbl.replace_typ_enum 
		   gtbl x t repr_newtyp
	      )
      ( List.map2 (fun (x,_) (_,u) -> (x,T.IntVal u)) aggr assoc_list
      ) 
  
and parse_extern_specification name =
  Npkcontext.print_debug "Parsing extern specification file";
  let spec_ast = parse_specification name in
    let norm_spec = normalization spec_ast in
      Npkcontext.print_debug "Done parsing extern specification file";
      match norm_spec with
	  (spec_l, Ast.Spec spec, loc) -> 
	    let basicdecls = List.fold_left (
	      fun ctx item -> 
		match item with
		    Ast.PackageSpec (m, decls), loc -> 
		      (Ast.PackageSpec (m, decls), loc)::ctx 
		  | _ -> ctx 
	    ) [] spec_l
	    in
	   
	      (basicdecls, spec, loc)
		
	| (_, Ast.Body(_), _) -> Npkcontext.report_error
            "normalize.parse_extern_specification"
              "internal error : specification expected, body found"
	      
and normalize_ident_cur ident = 
  match (Sym.current gtbl) with
  | Some x -> [x;ident]
  | None   -> [ident]

and normalize_params_cur param =
  let is_basic_or_pref_typ strs =
    let is_basic str =
      match str with
	  "integer" | "float" | "boolean" -> true
	| _ -> false
    in
      match strs with
	| typ::[] -> is_basic typ  
	| l when (compare (List.length l) 2 = 0) -> true 
	    (*add non regression*)
	| _ ->  Npkcontext.report_error "normalize_params_cur"
                  "chain of selected names is too deep"
	    
  in
  (*This function adds packing name info in parameter type label of specifications, 
    requires: type is in standard, and type is not already precised by a package *)
  let add_pack pack param =  
    let p_typ = param.param_type in
       if (is_basic_or_pref_typ p_typ) then
	 p_typ
       else
	 pack::p_typ 	  
  in
  let add_pack_default p def =
    match def with 
	Some (Lval (Var x)) -> Some (Lval ( SName (Var p, x)))
      | _ -> def
  in
      
    match (Sym.current gtbl) with
      | Some p ->
	  { 
	  formal_name  = param.formal_name
	  ; mode  = param.mode
	    (*Add package name only if not a Standard type*)
	  ; param_type = add_pack p param 
	  ; default_value = add_pack_default 
	                     p 
	                     param.default_value
	}
      | None ->
	  param
	  
	  
and normalize_sub_program_spec subprog_spec ~addparam =
  let normalize_params param_list func =
    if addparam then
      Sym.enter_context ~desc:"SP body (parameters)" gtbl;
    List.map
      ( fun param ->
          if func && (param.mode <> In)
          then Npkcontext.report_error
            "Normalize.normalize_params"
            ( "invalid parameter mode : functions can only have"
              ^ " \"in\" parameters");
          if (param.default_value <> None && param.mode <> In) then
            Npkcontext.report_error "Normalize.normalize_params"
              "default values are only allowed for \"in\" parameters";
          if addparam then begin
            Sym.add_variable gtbl param.formal_name (Newspeak.unknown_loc)
              (subtyp_to_adatyp param.param_type)
              ~ro:(param.mode = In)
            ;
          end;
          { Ast.formal_name = param.formal_name
          ; Ast.param_type  = subtyp_to_adatyp param.param_type
          }
      )
      param_list
  in
    match subprog_spec with
	(* TODO: remove this unique case *)
      | Subprogram(name,param_list,return_type) -> 
	  let norm_name = normalize_ident_cur name in
	  let norm_param_list = 
	    List.map normalize_params_cur param_list in
	    (* Param type must be preceded by 
	       the package name see test t405*)
          let t = Ada_utils.may subtyp_to_adatyp return_type in
            Sym.add_subprogram gtbl name norm_param_list t;
	    let arguments = 
	      normalize_params norm_param_list (return_type <> None) 
	    in
	      {
		Ast.name = norm_name;
		Ast.arguments = arguments;
		Ast.return_type = t;
	      }
	      
and normalize_basic_decl item loc =
  match item with
    | UseDecl use_clause -> 
	Sym.add_use (Hashtbl.mem spec_tbl use_clause) gtbl use_clause; 
	[]
    | ObjectDecl(ident_list, subtyp_ind, def, Variable) -> 
	let norm_subtyp_ind = normalize_subtyp_ind subtyp_ind in
	let t = merge_types norm_subtyp_ind in
	  List.iter (fun x -> Sym.add_variable gtbl x loc t) ident_list;
	  List.map (fun ident ->
		      Ast.ObjectDecl ( ident
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
	      (*TODO:  add a test for this*)
            | Aggregate _ -> 
		List.iter (fun x -> 
		  Sym.add_variable gtbl x loc t) ident_list;
		Ast.Constant
            | _ ->  
		try
		  let normexp = normalize_exp ~expected_type:t exp in
		  let value = Eval.eval_static normexp gtbl in	  
		  
		    List.iter ( fun x ->
			Sym.add_variable gtbl x loc t ~value
		  )
                      ident_list;
		      Ast.StaticVal value
		with
		  | Eval.NonStaticExpression -> List.iter
                      (fun x -> Sym.add_variable gtbl x loc t
                      ) ident_list;
                      Ast.Constant
		  | _ ->  
		      Npkcontext.report_error "Exit"
		      "Normalize: ObjectDecl Constant"
	end
	in
	  List.map ( fun ident -> 
		     Ast.ObjectDecl( ident, t, status
				    , build_init_stmt (ident, Some exp, loc)
				   )
		 ) ident_list
  | ObjectDecl _ -> Npkcontext.report_error
                   "Ada_normalize.normalize_basic_decl"
                   ( "internal error : constant without default value"
                   ^ "or already evaluated")
  | TypeDecl(id,typ_decl) ->
      normalize_typ_decl id typ_decl loc;
      []
  | SpecDecl spec ->  [Ast.SpecDecl(normalize_spec spec)]
  | NumberDecl(ident, exp) ->
      begin
        try
          let value = Eval.eval_static (normalize_exp exp) gtbl in
          add_numberdecl ident value loc;
          [Ast.NumberDecl(ident, value)]
        with
          | Eval.NonStaticExpression -> Npkcontext.report_error
             "eval_static.integer_exp"
             "expected static expression"
      end
  | SubtypDecl(ident, subtyp_ind) -> 
      let norm_subtyp_ind = normalize_subtyp_ind subtyp_ind in
      Sym.add_type gtbl ident loc (merge_types norm_subtyp_ind);
      []
  | RenamingDecl (n, arguments, ret_tp, o) ->
      let updt_arguments = Ada_utils.may 
	(List.map normalize_params_cur) arguments in
      let r_t = Ada_utils.may subtyp_to_adatyp ret_tp in
      let (pk, o') = mangle_sname o in
      let old = add_p (pk, o')  in
	(*only dedicated to find_symbols in Symbols*)
      let program_args = match updt_arguments with
	  None -> []  
	| Some args -> args 
      in

	(* Necessary for possible_type in symbol: only though to avoid 
	   Ambiguous raised for binop (is_overloaded)*)
	if not ( Sym.is_already_defined gtbl n ) then 
	  Sym.add_subprogram gtbl n program_args r_t
	;	
	Sym.add_renaming_decl gtbl
	  (Sym.current gtbl, n) updt_arguments old;
        []

  | RepresentClause (id, EnumRepClause aggr) ->
	    add_representation_clause id aggr loc;[]
  | RepresentClause (id, _) -> 
      Npkcontext.report_warning "normalize"
        ( "Ignoring representation clause "
          ^ "for '" ^ id ^ "'");
      []
  | GenericInstanciation  (ident , names, actuals) when
      ((compare (List.length names) 1  = 0) &&
       (compare(List.hd names) "unchecked_conversion" = 0)) -> 
      begin
	match actuals with 
	    (Some "source", Lval src)::(Some "target", Lval tgt)::[]
	  | (None, Lval src)::(None, Lval tgt)::[] -> 
	      begin
		let src_t = Symboltbl.make_name_of_lval src in	  
		let tgt_t = Symboltbl.make_name_of_lval tgt in
		let r_t = Ada_utils.may subtyp_to_adatyp(Some tgt_t) in
		let source = {
		  formal_name = "SOURCE";
		  mode  = In;
		  param_type = src_t;
		  default_value = None
		} 
		in		  
		let param_list =  source::[] in 
		  (*add target *)
		  Sym.add_subprogram gtbl ident param_list r_t;
		  []
	      end
	  | _ -> 
	      Npkcontext.report_warning "normalize"
              ("ignoring generic instanciation of ") ; 
	      []
      end
	
  | GenericInstanciation _ -> 
      Npkcontext.report_warning "normalize"
	("ignoring generic instanciation of ") ; 
      []
	


and normalize_package_spec (name, list_decl) =
  Sym.set_current gtbl name;
  Sym.enter_context ~name ~desc:"Package spec" gtbl;
  let rec normalize_decls decls =
     List.flatten (List.map (fun (decl, loc) ->
			      Npkcontext.set_loc loc;
			      List.map (fun x -> (x,loc))
				(normalize_basic_decl decl loc)
			   ) decls) in
  let norm_spec = 
    if  (Hashtbl.mem spec_tbl name) 
    then begin  
      match (Hashtbl.find spec_tbl name) with
	Ast.PackageSpec (_, n_spec),_ ->  n_spec
      | _ ->   Npkcontext.report_error "normalize_package_spec "
             ("Package form in spec_tbl of" ^ name ^ "not as expected");
    end
    else begin    
      let n_spec = normalize_decls list_decl in
      let spec =  Ast.PackageSpec (name, n_spec) in 
	(*Sym.add_with gtbl name;*)
	Hashtbl.add spec_tbl name (spec, Newspeak.unknown_loc);
	n_spec
    end
  in
    Sym.reset_current gtbl;
    Sym.exit_context gtbl;
    (name, norm_spec)
      
and normalize_spec spec = match spec with
  | SubprogramSpec subprogr_spec -> Ast.SubProgramSpec(
      normalize_sub_program_spec subprogr_spec ~addparam:false)
  | PackageSpec package_spec ->
      Ast.PackageSpec(normalize_package_spec package_spec)

and normalize_lval ?(force = false) ?expected_type synt_lv = 
  match synt_lv with
      (*t414*)
    | (SName (ParExp(Var _, _), _)) 
    | (SName ( SName ( ParExp(Var _, _), _ ), _ )) as arrec -> 	
	begin
	let exp =  normalize_exp (Lval arrec) in
	  match exp with
	      Ast.Lval(Ast.RecordAccess _ as record_access), tf -> 
		 record_access, tf

	    | Ast.BlkExp ( [instr, loc], ( Ast.Lval lvalue, _)), t ->
		Ast.BlkLval ([instr, loc], lvalue ), t
		
	    | _ -> Npkcontext.report_error 
		"normalize_lval" "Unexpected case"
      end

    | (Var _ | SName _) as lv ->
     (* Only in write contexts *)
      begin match resolve_selected ?expected_type lv with
      | SelectedVar    (sc, id,  t, ro) ->
          if (ro && not force) then
          Npkcontext.report_error "normalize_val"
             ("Invalid left value : '" ^ id ^ "' is read-only");
          Ast.Var  (sc, id, t), t
      | SelectedRecord (lv, off, tf) ->
          Ast.RecordAccess (lv, off, tf), tf
      | SelectedFCall _
      | SelectedConst _ -> Npkcontext.report_error "normalize_lval"
                             ("Invalid left-value")
      end
  | ParExp (lv, e) ->
      let (lv',tlv) = normalize_lval lv in
      let (tc,_ti) = T.extract_array_types tlv in
      Ast.ArrayAccess(lv' , List.map (fun x -> normalize_exp (snd x)) e), tc
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
      begin
	let (lv', t_lv) = normalize_lval ~force:true (Var x) in
	let ne_pas_dupliquer a b =
	  let a = Nat.to_big_int a
	  and b = Nat.to_big_int b in
	    if EBigInt.sign_big_int b < 0
	    then begin
	      Npkcontext.report_error "Eval"
		"Integer exponents should be strictly positive."
	    end 
	    else Nat.of_big_int (
	      EBigInt.power_big_int_positive_big_int a b)
	in
	let power powep = 
	  let (exp, typ) = 
	    normalize_exp ~expected_type:t_lv powep in 
	    match exp with  
 		Ast.Binary( Ast.Power, (Ast.CInt x,_), (Ast.CInt y,_))-> 
		  (Ast.CInt (ne_pas_dupliquer x y), typ)
	      | Ast.Binary( Ast.Power, (Ast.CFloat x,_), (Ast.CInt y,_))->
		  (Ast.CFloat (x ** (float_of_int(Nat.to_int y))), typ)
	      | _ -> Npkcontext.report_error "build_init_stmt" 
		                             "Impossible case"
	in
	  match def with 
	      Binary (Power, _, _) ->
		let (e', t_exp) = power def in
		  if (not (T.is_compatible t_lv t_exp)) then
		    begin
		      Npkcontext.report_error "normalize_instr"
			"Incompatible types in build_init_stmt";
		    end;
		  Some [Ast.Assign( lv'
				      , (e',t_exp)
				  ), loc]
		    
	    | Unary (UMinus,  Binary (Power, a, b)) ->
		let (power_def, t_exp)  =  power (Binary (Power, a, b) ) in 
		let zero =
		  if (T.is_integer t_exp) then
		    Ast.CInt (Nat.zero)
		  else if (T.is_float t_exp) then
		    Ast.CFloat (0.0)
		  else Npkcontext.report_error "build_init_stmt"
		    "Unary minus defined for integer and floating-point types"
		in
		let e' = Ast.Binary(Ast.Minus, (zero,t_exp), 
				    (power_def,t_exp)) in
		  if (not (T.is_compatible t_lv t_exp)) then
		    begin
		      Npkcontext.report_error "normalize_instr"
			"Incompatible types in build_init_stmt";
		    end;
		  Some [Ast.Assign( lv'
				      , (e',t_exp)
				  ), loc]
		    
	    | _ -> 
		Some (normalize_block ~force_lval:true [Assign(Var x, def), loc])
      end
	
(**
 *The optional parameter return_type helps disambiguate return_statements :
 * while translating a block in a function, it is set to this function's
 * return type.
 * When translating other blocks, it shall be set to None.
 *)
and normalize_instr ?return_type ?(force_lval = false) (instr,loc) =
  Npkcontext.set_loc loc;
  match instr with
  | NullInstr    -> []
  | ReturnSimple -> [Ast.ReturnSimple, loc]
  | Assign(lv, Aggregate (NamedAggr bare_assoc_list)) ->
      let (nlv, t_lv) = normalize_lval lv in
      normalize_assign_agr nlv t_lv bare_assoc_list loc
  | Assign(lv, Aggregate (PositionalAggr exp_list)) -> 
      let (nlv, t_lv) = normalize_lval lv in

	if (T.is_array t_lv) then 
	  begin
	    let tc, ti = 
	      match T.extract_array_types t_lv with
		| (c_t, [i]) -> c_t, i
		| _ -> Npkcontext.report_error 
		    "normalize_instr" 
		      "unexpected matrix type"
	    in
	    let all_values  = T.all_values ti in
	     List.flatten ( 
	      List.map2 (
		fun type_val exp ->
		  let k = insert_constant (T.IntVal type_val) in
		    match exp with 
			Aggregate(NamedAggr assoc_list) when (
			  T.is_record tc) ->   
			    let alv = Ast.ArrayAccess (nlv, [k]) in
			      normalize_assign_agr 
				alv 
				tc 
				assoc_list 
				loc
			     
			| Aggregate (PositionalAggr exps) when (
			    T.is_record tc) ->
			    (*CHECK tc is a record type t410*)
			    (*TO DO: for matrix initialization: 
			      build AggrExp for the selector*)
			    let alv = Ast.ArrayAccess (nlv, [k]) in
			    let fields = T.all_record_fields tc in
			    let assoc_list = List.map2 (fun x y ->
							  (AggrField x,y)
						       ) fields exps 
			    in 
  			      normalize_assign_agr 
				alv 
				tc 
				assoc_list 
				loc
			   
		   | Aggregate (PositionalAggr _) 
		   | Aggregate (NamedAggr _)  -> 
		       Npkcontext.report_error "normalize_instr, Assign" 
			 "array as compound not implemented yet"
		   | _ -> 
		       let v = normalize_exp exp in 
			 [Ast.Assign (Ast.ArrayAccess
			(nlv, [k]), v), loc]
	      ) all_values exp_list
	     )
	  end
	else if (T.is_record t_lv) then 
	  begin
	    let fields = List.map (fun x -> AggrField x)
	      (T.all_record_fields t_lv)
	    in
	      try
		let assoc_l = List.map2 (fun x y -> (x,y))
		  fields exp_list
		in 
		  normalize_assign_agr nlv t_lv assoc_l loc
	      with _ -> Npkcontext.report_error "normalize_instr"
	                                        "List.assoc 2 failed"
	  end
	else 
	  Npkcontext.report_error  "normalize_instr"
	    "Posit. case UNexpected type "
	  
		  
  | LvalInstr(ParExp(lv, params)) ->
      let n = Symboltbl.make_name_of_lval lv in
      let n = mangle_sname n in
	(*use to be after find_sub*)
      let norm_args = List.map normalize_arg params in
      let norm_typs = List.map (fun (s,(_,t)) -> (s,t)) norm_args in
      let (sc,(act_name,spec,_)) = 
     (* Sym.find_subprogram gtbl n in*)
	Sym.find_subprogram gtbl (add_p n) norm_typs None
	     ( fun x -> Symboltbl.find_type gtbl x ) 
      in	 
      let effective_args = make_arg_list norm_args spec in
	[Ast.ProcedureCall( sc, act_name, effective_args), loc]
  | LvalInstr((Var _|SName (Var _,_)) as lv) ->
      normalize_instr (LvalInstr (ParExp(lv, [])),loc)
  | LvalInstr _ -> Npkcontext.report_error "normalize_instr"
                     "Statement looks like a procedure call but is not one"
  | Assign(lv, exp) -> 
        let (lv', t_lv) = normalize_lval ~force:force_lval lv in
        let (e', t_exp) = normalize_exp ~expected_type:t_lv exp in
	  if (not (T.is_compatible t_lv t_exp)) then
	    begin
            Npkcontext.print_debug ("LV = " ^ T.print t_lv);
            Npkcontext.print_debug ("EX = " ^ T.print t_exp);
            Npkcontext.report_error "normalize_instr"
              "Incompatible types in assignment";
          end;
	     [Ast.Assign( lv'
			    , (e',t_exp)
			 ), loc]
	
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

  | Loop(For(iter, range, is_rev), block) -> begin
      let (exp1, exp2) = match range with
	| DirectRange (min, max) -> (min, max)
	| ArrayRange n -> begin
	    let n = Symboltbl.make_name_of_lval n in
	    let n = mangle_sname n in
              try 
		let (_,(_,t,_)) = Sym.find_variable gtbl n in
		  ( fst (T.attr_get t "first")
                      , fst (T.attr_get t "last"))
	      with Not_found -> 
		(*ArrayRange range can in fact be a type range! *)
		let t = snd (Sym.find_type gtbl n) in 
		    ( fst (T.attr_get t "first")
                      , fst (T.attr_get t "last"))
	  end
	| SubtypeRange lv -> begin
            let st = Symboltbl.make_name_of_lval lv in
            let t = subtyp_to_adatyp st in
              ( fst (T.attr_get t "first")
                  , fst (T.attr_get t "last"))
          end
      in
    
      let toto = 
	match range with 
	    DirectRange (CInt _, CInt _) ->       
	      merge_types (normalize_subtyp_ind (["standard";"integer"], None))
	  | DirectRange ( l ,CInt _) 
	  | DirectRange (CInt _,  l) 
	  | DirectRange (l, _) -> snd (normalize_exp l)
	  | ArrayRange n  -> begin
	      let nn = Symboltbl.make_name_of_lval n in
	      let mn = mangle_sname nn in
		try 
		  let (_,(_,t,_)) = Sym.find_variable gtbl mn in  
		    T.extract_array_range t 
		with Not_found -> 
		  
		  merge_types (normalize_subtyp_ind (nn, None))
	    end
	  | SubtypeRange lv ->  
	      merge_types (normalize_subtyp_ind (
			     Symboltbl.make_name_of_lval lv, None))
      in

      let basic_loop () = 
	(*TODO For while loop a counter incremention is not enough *)
	Sym.enter_context gtbl;
	let ndp = 
	  let init = if is_rev then exp2 else exp1 in 
	  let status = 
	    match init with
		Aggregate _ ->  
		  Npkcontext.report_error "Loop declarative part" 
		    "Aggregate not handled"
	      | _ ->
		  try	
		    let normexp = normalize_exp ~expected_type:toto init in
		    let value = Eval.eval_static normexp gtbl in
		      Npkcontext.set_loc loc;
		      Sym.add_variable gtbl iter loc toto ~value;
		      Ast.StaticVal value
		  with
		      Eval.NonStaticExpression ->
			Npkcontext.report_warning "NonStaticExpression"
		     	  "Normalize: ObjectDecl Constant";
			Sym.add_variable gtbl iter loc toto;
			Ast.Variable
		    | _ -> Npkcontext.report_error "Exit"
			"Normalize: ObjectDecl Constant"
	  in	    
	    [Ast.BasicDecl ( Ast.ObjectDecl ( iter, toto, status
			     , build_init_stmt ( iter, Some init, loc)) 
			   ) , loc
	    ]
	in	  
	
	let nblock = normalize_block ?return_type block in
	let loop = [Ast.Loop ( Ast.While
		( normalize_exp (
		    if is_rev then 
		      Binary(Ge,Lval(Var iter),exp1)
		    else 
		      Binary(Le,Lval(Var iter),exp2))
		)
		, nblock @ [Ast.Assign ( Ast.Var (Sym.Lexical,iter,T.integer)
		, normalize_exp( Binary( (if is_rev then Minus else Plus)
					   , Lval(Var iter)
					     , CInt (Nat.one)))
				       ), loc] 
			      )
		       , loc
		     ]
	in  
	  Sym.exit_context gtbl;
	  [Ast.Block (ndp, loop), loc]
      in
	
	match range with 
	      (*when subtype is enumeration, iter++ will 
		fail because iter has type enumeration(t417):
		solution: create an integer, and an array
		mapping it to representing clause
	      *)
	      SubtypeRange lv  ->  
		begin
		  let names = Symboltbl.make_name_of_lval lv in
		  let ada =  subtyp_to_adatyp names in 
		    match (T.extract_symbols ada, T.compute_int_constr ada) with 

		    | Some _, None -> 
			Npkcontext.report_error 
			  "normalize in: Loop(For(_, range,_)"
			  ("iteration sur type enum contraint"^
			     " not implemented(cf t417)")

		    |  Some enums, Some (min, max) ->
			 let sub_enums = List.filter
			   ( fun (_, v) ->
			       (compare  (Newspeak.Nat.to_int min) v <= 0) &&
			        (compare v (Newspeak.Nat.to_int max) <= 0)
			   ) enums
			 in
			   (*no 'classical' incremention possible, 
			     bornes are known: unrolling*)
			 let (init, tail) = 
			   if is_rev then begin
			     let rev = List.rev sub_enums in
			       AdaSyntax.CInt (Newspeak.Nat.of_int (snd (List.hd rev)))
				 , List.tl rev
			   end else begin
			     AdaSyntax.CInt (
			       Newspeak.Nat.of_int(snd (List.hd sub_enums)))
			       ,  List.tl sub_enums
			   end
			 in
			   Sym.enter_context gtbl; 
			   Sym.add_variable gtbl iter loc toto;
			   let ndp = [(Ast.BasicDecl 
					 (Ast.ObjectDecl (iter, toto, Ast.Variable,
					   build_init_stmt (iter, Some init, loc)) 
					 ),  loc
				      )
				     ] 
			   in 
			   let nblock = normalize_block ?return_type block in
			   let unrolled_loops = ref nblock in
			     List.iter
				 ( fun (x_str, _) ->
				     unrolled_loops:=List.append !unrolled_loops
				       ([Ast.Assign (
					   Ast.Var (Sym.Lexical, iter, ada)
					     , normalize_exp(  Lval(Var x_str))
					 )
					   , loc
					]@nblock
				       )
				 ) tail;
			     Sym.exit_context gtbl;
			     [Ast.Block (ndp, !unrolled_loops), loc]
				 
		    | _ ->  basic_loop ()
		end
	    | _ -> 	basic_loop ()
    end
      
  | Exit -> [Ast.Exit, loc]
  | Case (e, choices, default) ->
      let case_exp = normalize_exp e in
      let (_, case_t) = case_exp in 
        [Ast.Case (case_exp ,
                   List.map (fun (e,block) ->
                               normalize_exp e ~expected_type:case_t,
                               normalize_block ?return_type block)
                     choices,
                   Ada_utils.may (fun x -> normalize_block ?return_type x)
                     default
                  ),loc
	]
  | Block (dp,blk) -> 
      Sym.enter_context ~desc:"Declare block" gtbl;
      let ndp = normalize_decl_part dp in
      let norm_block = normalize_block ?return_type blk in
        Sym.exit_context gtbl;
        [Ast.Block (ndp, norm_block), loc]



and normalize_assign_agr nlv t_lv bare_assoc_list loc =
  let  handle_others r_lv aff prefs_idx lists_idx =
    let res = ref [] in 
    let rec handle_others_aux ps_idx  ls_idx  = 
      match ls_idx with 		      
	  [] -> res:= (Ast.Assign (Ast.ArrayAccess
				     (r_lv, ps_idx), aff),
		       loc
		      ) ::!res  
	| hd::tl  -> List.iter (fun x -> 
				  handle_others_aux (x::ps_idx) tl
			       ) hd
    in
      handle_others_aux prefs_idx  lists_idx;
      !res
  in
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
                ( "Within an aggregate, selectors"
                ^ "should evaluate as integers")
      in
      List.fold_left (fun (kvl, others_exp) (selector, value) ->
        let rec handle aggregate_sel = 
	  match aggregate_sel with
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
		    (interval e1' e2') @ kvl, None
		end
            | AggrOthers ->
		begin
		  if others_exp <> None then
                    Npkcontext.report_error "normalize"
                      "In an aggregate, there shall be only one \"others\" clause";
		  (kvl, Some value)
		end
            | AggrField f -> handle (AggrExp (Lval(Var f)))
        in 
	  handle selector
	) ([],None) bare_assoc_list
    in
    (*
     * Now, using lv's type, we can :
     *   - compute missing elements
     *   - replace 'others' with them
     *)
      match T.extract_array_types t_lv with
	| (tc, [ti]) -> begin (*code deplace cf plus bas*)
	    
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
	    let rec are_all_flds ll = 
	      match ll with
		  [] -> true
		| hd::tl -> match hd with
		      (AggrField _, _) ->  are_all_flds tl 
		    | _ -> false
	    in
	      List.rev (
		  List.flatten (
		    List.map (

		      fun (aggr_k, aggr_v) ->(* id[aggr_k] <- aggr_v *)
			
			let key  = normalize_exp aggr_k in
			  
			match aggr_v with
			    Aggregate (NamedAggr ll) when (are_all_flds ll) ->
 			      let array_lv = Ast.ArrayAccess (nlv, [key]) in
				normalize_assign_agr array_lv tc ll loc

			  | Aggregate (NamedAggr ((AggrExp _, _)::[])) ->
			      Npkcontext.report_error "normalize_assign_agr"
				"Array with aggregate expression not done yet"
				
	      		  | Aggregate(NamedAggr((AggrOthers, only_oth)::[])) -> 
			      let rec has_only_other exp =
				match exp with
				    CInt _ -> Some exp
				  | Aggregate(NamedAggr((AggrOthers, z)::[])) -> 
				      has_only_other z
				  | _ -> Npkcontext.report_error 
				      "normalize_assign_agr"
					"only int handled in array with others"
			      in
			      let othval = 
				match (has_only_other only_oth) with
				    Some i -> normalize_exp i 
				  | _ -> Npkcontext.report_error 
				      "normalize_assign_agr"
					"Other case only handled when only used with others"
			      in
			      let rec depiler_type tc = 
				if T.is_array tc then
				  match T.extract_array_types tc with
				      (tc', [ti']) ->
				     	let idxes = T.all_values ti' in
					  idxes::(depiler_type tc')
				    | _  ->  Npkcontext.report_error 
					"normalize_assign_agr"
					  "Case not handled"
				else []
			      in

			      let rec bimapping f l m  = 
				match l with 
				    [] ->   Npkcontext.report_error 
				      "normalize_assign_agr" 
				      "unexpected empty list case"
				  | hd::[] -> List.map (f hd) m
				  | hd::tl ->
				      let fst_map = List.map (f hd) m in
					List.append fst_map (bimapping f tl m )
			      in
				
			      let rec build_assign idxs top_lvs vl =
				match idxs with 
				    [] -> 
				      List.map (fun x->Ast.Assign(x,vl), loc) top_lvs
				  | hd::tl -> 
				      let new_top_lvs = 
					let buildaccess x y = 
					  let key = insert_constant (T.IntVal y )  in
					    Ast.ArrayAccess (x, [key])
					in
					  bimapping  buildaccess top_lvs hd
				      in 
					build_assign tl new_top_lvs vl
			      in 
				build_assign 
				  (depiler_type tc)
				  [Ast.ArrayAccess (nlv, [key])] 
				  othval
				

				(*-----*)
			    | _ -> 
				let value =  normalize_exp aggr_v in
				  [Ast.Assign (Ast.ArrayAccess (nlv, [key]), value), loc]
				    
		    ) (other_list @ assoc_list') 
		  )
		)
		
	  end


	(* MATRIX 2*2: we only handle case with "others" and nothing else*)
	  | (c, twotwolist) when (compare (List.length twotwolist) 2 = 0) ->
	     let only_has_others assoc others =
	      match (assoc, others) with
	  	  ([], Some hd) -> begin
	  	    match hd with
	  		Aggregate(NamedAggr((AggrOthers, xx)::[]))-> Some xx
	  	      | _ -> None
	  	  end
	  	| _ -> None
	    in
	      begin
	  	match (only_has_others assoc_list others_opt ) with
	  	    Some xx ->
	  	      let affected = match xx with
	  		  CInt _
	  		| CFloat _
	  		| Lval (Var _) ->
	  		    normalize_exp ~expected_type:c xx
	  		| _ -> Npkcontext.report_error "normalize:assign aggregate"
	  		    "Expected an Integer or a Float"
	  	      in
	  	      let all_values = List.map (
	  		fun x ->
	  		  let vals = T.all_values x in
	  		    List.map (fun y -> (Ast.CInt y,x)) vals
	  	      )  twotwolist
	  	      in
	  		handle_others nlv affected [] (List.rev all_values)
			  
	  	  | _ ->  Npkcontext.report_error "aggregate"
	  	      "matrix type case not handled"
	      end
	| _ ->
	    Npkcontext.report_error "aggregate"
              "Unexpected type"
	    
    (* end of array_case *)
  in




  let record_case _ =
    let module FieldSet = Set.Make (String) in
    (* (selector*exp) list --> (string*exp) list*exp option *)
    let filter_aggregate sel_vals =
      List.fold_left
	(fun (fvl,others_opt) (selector, value) ->
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
	) ([],None) sel_vals
    in

    
    let (assoc_list,other) = filter_aggregate bare_assoc_list in
      
    let getfields_other tlv assoc_flds value_opt =
      match value_opt with
	| None           -> []
	| Some other_exp ->
            begin
              let flds = T.all_record_fields tlv in
              let mk_set l =
		List.fold_left (fun x y -> FieldSet.add y x)
                  FieldSet.empty l
              in
              let all_fields = mk_set flds in
              let defined    = mk_set assoc_flds in
              let missing_others =
		FieldSet.elements (FieldSet.diff all_fields defined) in
		List.map (fun f -> (f, other_exp)) missing_others
            end
    in      
    let other_list =  getfields_other t_lv  (List.map fst assoc_list) other in

    let rec  handle_record lv tlv  aggrs = 
      List.flatten 
   	(List.rev_map (
	   fun (aggr_fld, aggr_val) ->
	     (* id.aggr_fld <- aggr_v *)
	     let (off, tf) = T.record_field tlv aggr_fld in	
	       match aggr_val with
		   Aggregate (NamedAggr ((AggrOthers,   
					  Aggregate (NamedAggr ((AggrOthers, va)::[])))::[])) 
		 |  Aggregate (NamedAggr ((AggrOthers, va)::[])) when (T.is_array tf) ->
		      begin
			let c, ids = T.extract_array_types tf in
			let all_values = List.map (
			  fun x ->
			    let vals = T.all_values x in
			      List.map (fun y -> (Ast.CInt y,x)) vals
			) ids 
			in
			let record_lv = Ast.RecordAccess (lv, off, tf) in
			let affected = match va with  
			    CInt _ 
			  | CFloat _  
			  | Lval (Var _ ) -> normalize_exp ~expected_type:c va
			  | _ -> Npkcontext.report_error "normalize:assign aggregate"
			      "Expected an integer or a float"
			in
		      	  handle_others record_lv affected [] (List.rev all_values)
	              end
			
		 |  Aggregate (NamedAggr assoc_l) when (T.is_record tf) ->
		      let record_lv = Ast.RecordAccess (lv, off, tf) in
		      let (assoc_l,other) = filter_aggregate assoc_l in
		      let other_l = getfields_other tf (List.map fst assoc_l) other in
			
			handle_record record_lv tf (assoc_l @ other_l) 
	
		 | _ -> 
		     (*cas ususel *)
		     let v = normalize_exp ~expected_type:tf aggr_val in
		       [Ast.Assign (Ast.RecordAccess (lv, off, tf), v), loc]	    
	 ) aggrs
	)

    in 
      handle_record nlv t_lv  (assoc_list @ other_list) 
	(* end of record_case *)
  in
    if      T.is_array  t_lv then array_case  ()
    else if T.is_record t_lv then record_case ()
    else Npkcontext.report_error "normalize_assign_agr"
           "Expecting an array or a record as lvalue"

and normalize_block ?return_type ?(force_lval = false) block =
  List.flatten (List.map (normalize_instr ?return_type ~force_lval) block)

and normalize_decl_part decl_part =
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
                                         ( "Declaration of \""
                                         ^ (Ast.name_of_spec sp)
                                         ^ "\" requires completion")
            end
    | _ -> ()
  ) ndp;
  ndp

and normalize_body body  = match body with
  | SubprogramBody(subprog_decl,decl_part,block) ->
	let norm_subprog_decl =
          normalize_sub_program_spec subprog_decl ~addparam:true in
	  Sym.enter_context ~desc:"SP body (locals)" gtbl;
	  let return_type = norm_subprog_decl.Ast.return_type in
	  let norm_decl_part = normalize_decl_part decl_part in
	  let norm_block = normalize_block ?return_type block in
	    Sym.exit_context gtbl;
	    Sym.exit_context gtbl; (* params *)
	    let current_loc = Npkcontext.get_loc () in
	    let block =
	      [(Ast.Block (norm_decl_part, norm_block), current_loc)] 
	    in
	      Ast.SubProgramBody (norm_subprog_decl, block)

  | PackageBody(name, package_spec, decl_part) ->
      let (nname,nspec) = normalize_package_spec
        (with_default package_spec
           (parse_package_specification name)
        )
      in
	Sym.set_current gtbl name;
        Sym.enter_context ~name ~desc:"Package body" gtbl;
        let ndp = normalize_decl_part decl_part in
        let norm_spec = (nname,nspec) in
        check_package_body_against_spec ~body:ndp ~spec:norm_spec;
        Sym.reset_current gtbl;
        Sym.exit_context gtbl;
	Ast.PackageBody (name, Some norm_spec, ndp)

and normalize_lib_item lib_item loc =
  Npkcontext.set_loc loc;
  match lib_item with
    | Spec spec -> Ast.Spec (normalize_spec spec)
    | Body body ->
	let previous_val = !set_package in 
	begin
	  match body with
	      (SubprogramBody ( Subprogram(n,_,_), _, _)) -> 
		set_package:=Some n;
	    | _ -> ()
		
	end;
	let tmp = Ast.Body (normalize_body body) in
	  set_package:=previous_val
	  ;  
	  tmp
	  

and add_extern_spec spec =
  let add_extern_basic_decl (basic_decl, loc) =
    Npkcontext.set_loc loc;
    match basic_decl with
      | Ast.ObjectDecl(ident, t, (Ast.Variable | Ast.Constant),_) ->
	  Sym.add_variable gtbl ident loc t
      | Ast.ObjectDecl(ident, t, Ast.StaticVal value,_) ->
	  Sym.add_variable gtbl ident loc t ~value
      | Ast.NumberDecl(ident, value) ->
	  add_numberdecl ident value loc
      | Ast.SpecDecl _ -> ()
    in match spec with
      | Ast.SubProgramSpec _ -> ()
      | Ast.PackageSpec(name, basic_decls) -> 
	  Sym.set_current gtbl name;
          Sym.enter_context ~name ~desc:"Package spec (extern)" gtbl;
          (*WG this package spec might have been added before*)
	  (*adding if cond...*)
	  (*if (not (Sym.is_with gtbl name)) then begin*)
	  if (not (Hashtbl.mem spec_tbl name)) then begin
	    List.iter add_extern_basic_decl basic_decls
          end;
	  Sym.reset_current gtbl;
          ignore (Sym.exit_context gtbl);
          (*Sym.add_with gtbl name*)
	    
and normalize_context context =
  List.fold_left ( 
    fun ctx item -> 
      match item with
	| With(nom, spec) -> 
	    (*if (not (Sym.is_with gtbl nom)) then*)
	    if (not (Hashtbl.mem spec_tbl nom) && not (Sym.is_ada_pck nom )) then
	      begin 
		let (b_decls, norm_spec, loc) = 
		  match spec with
		    | None   -> parse_extern_specification nom
		    | Some _ -> Npkcontext.report_error
			"Ada_normalize.normalize_context"
			  "internal error : spec provided"
		in
		  add_extern_spec norm_spec; 
		  (*Only add b_decls that are not in spec_tbl? *)
		  Hashtbl.add spec_tbl nom (norm_spec, loc);
		  (b_decls)@((norm_spec, loc)::ctx)
	      end
	    else 	
	      (*Etienne Millon = ctx*)
	      (*Not found for internal spec like  System *)	
	      begin try 
		let with_clause = Hashtbl.find spec_tbl nom in
		  with_clause::ctx
	      with Not_found -> ctx (*for "System"*)
	      end
		
	| UseContext n  -> 
	    Sym.add_use (Hashtbl.mem spec_tbl n) gtbl n; 
	    ctx 
  ) [] context
    
(**
 * Iterates through the abstract syntax tree, performing miscellaneous tasks.
 *   - match type identifiers to their declaration (or raise en error)
 *   - look for specs (.ads files)
 *   - transforms functions and type names to "package.ident" in their
 *     declarations.
 *)
    
and normalization compil_unit = 
 
  let cu_name = compilation_unit_name compil_unit in
    Npkcontext.print_debug ("Semantic checking " ^ cu_name ^ "...");
    let (context, lib_item,loc) = compil_unit in
    let norm_context = normalize_context context in
    let norm_lib_item = normalize_lib_item lib_item loc in 
      Npkcontext.forget_loc ();
      Npkcontext.print_debug ("Done semantic checking " ^ cu_name);
       (norm_context, norm_lib_item, loc)
	
