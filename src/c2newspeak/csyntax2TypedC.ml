(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2009  Charles Hymans
  
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

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org
*)

(* TODO: TypedC should be exactly Csyntax + types only!!!! *)
(* TODO: -> Parsing -> BareC -> Typing -> TypedC 
   -> Implicit casts addition, side-effects elimination, boolean expression
   normalization, redundant expressions/statements removal 
   -> CoreC -> optional Goto eliminatination *)
open Csyntax
module Nat = Newspeak.Nat
module C = TypedC

(* Constants *)
let ret_name = "!return"

(* TODO: remove put in csyntax2CoreC *)
(* TODO: code cleanup: find a way to factor this with create_cstr
   in Npkil *)
(* TODO: find a way to cleanup code, this is also present in firstpass!! *)
let seq_of_string str =
  let len = String.length str in
  let res = ref [(None, Data (exp_of_char '\x00'))] in
    for i = len - 1 downto 0 do
      res := (None, Data (exp_of_char str.[i]))::!res
    done;
    !res

let find_field f r =
  try List.assoc f r 
  with Not_found -> 
    Npkcontext.report_error "Firstpass.translate_lv" 
      ("unknown field '"^f^"' in union or structure")

let process globals =
  (* TODO: find a way to remove Symbtbl and use a standard Hashtbl here! 
     but first needs to put the whole typing phase before firstpass
  *)
  let symbtbl = Hashtbl.create 100 in
  let comptbl = Hashtbl.create 100 in
  (* Used to generate static variables names *)
  let current_fun = ref "" in
  (* Counter of static variables, necessary to distinguish 2 statics in 
     different scope of the same function, who would have the same name
  *)
  let static_cnt = ref 0 in

  let get_static_name x loc =
    let (fname, _, _) = loc in
    let prefix = "!"^fname^"." in
    let prefix = 
      if !current_fun = "" then prefix else prefix^(!current_fun)^"."
    in
    let name = prefix^(string_of_int !static_cnt)^"."^x in
      incr static_cnt;
      name
  in

  let complete_typ_with_init t init =
    let rec process (x, t) =
      match (x, t) with
(* TODO: find a way to simplify/remove this case?? *)
	  ((Data (Str str)|Sequence ([(None, Data (Str str))])), 
	   C.Array (C.Int (_, n), _)) when n = Config.size_of_char ->
	    let seq = seq_of_string str in
	      process (Sequence seq, t)

	| (Sequence seq, C.Array (t, None)) -> 
	    let n = C.exp_of_int (List.length seq) in
	      C.Array (t, Some n)
	| _ -> t
    in
      match init with
	  None -> t
	| Some init -> process (init, t)
  in
    
  let add_local (t, x) = Hashtbl.add symbtbl x (C.Local x, t) in

  let remove_var x = Hashtbl.remove symbtbl x in

(* TODO: find a way to factor declare_global and add_local
   maybe necessary to complete_typ_with_init in both cases...
   maybe just needs a is_global bool as argument..
   need to check with examples!!!
*)
  let update_global x name t = 
    Hashtbl.replace symbtbl x (C.Global name, t) 
  in

  let add_formals (args_t, ret_t) =
    add_local (ret_t, ret_name);
(* TODO: think about it, not nice to have this pattern match, since in
   a function declaration there are always arguments!! 
   None is not possible *)
    match args_t with
	Some args_t -> List.iter add_local args_t
      | None -> 
	  Npkcontext.report_error "Csyntax2CoreC.add_formals" "unreachable code"
  in

  let remove_formals (args_t, _) =
    remove_var ret_name;
(* TODO: think about it, not nice to have this pattern match, since in
   a function declaration there are always arguments!! 
   None is not possible *)
    match args_t with
	Some args_t -> List.iter (fun (_, x) -> remove_var x) args_t
      | None -> ()
  in
  let find_symb x = 
    try Hashtbl.find symbtbl x
    with Not_found -> 
      if (Gnuc.is_gnuc_token x) && (not !Npkcontext.accept_gnuc) then begin
	Npkcontext.report_accept_warning "Csyntax2TypedC.process.find_symb" 
	  ("unknown identifier "^x^", maybe a GNU C symbol") Npkcontext.GnuC
      end;
      Npkcontext.report_accept_warning "Csyntax2TypedC.process.find_symb" 
	("unknown identifier "^x^", maybe a function without prototype") 
	Npkcontext.MissingFunDecl;
      let info = (C.Global x, C.Fun (None, C.int_typ)) in
	(* TODO: clean up find_compdef + clean up accesses to Symbtbl *)
	Hashtbl.add symbtbl x info;
	info
  in

  let find_compdef name =
    try Hashtbl.find comptbl name
    with Not_found -> 
      let c = ref None in
	Hashtbl.add comptbl name c;
	c
  in

  let update_funtyp f ft1 =
    let (symb, t) = Hashtbl.find symbtbl f in
    let ft2 = TypedC.ftyp_of_typ t in
    let ft = TypedC.min_ftyp ft1 ft2 in
      Hashtbl.replace symbtbl f (symb, C.Fun ft)
  in

  let update_funsymb f static ft loc =
    let (fname, _, _) = loc in
    let f' = if static then "!"^fname^"."^f else f in begin
	try update_funtyp f ft
	with Not_found -> 
	  Hashtbl.add symbtbl f (C.Global f', C.Fun ft)
      end;
      f'
  in

  let translate_proto_ftyp f static (args, ret) loc =
    if args = None then begin
      Npkcontext.report_warning "Csyntax2CoreC.check_proto_ftyp" 
	("incomplete prototype for function "^f)
    end;
    let _ = update_funsymb f static (args, ret) loc in
      ()
  in

  let refine_ftyp f (args_t, ret_t) actuals = 
    match args_t with
	None -> 
	  Npkcontext.report_accept_warning "Csyntax2CoreC.refine_ftyp"  
            "unknown arguments type at function call"   
            Npkcontext.PartialFunDecl;
	  let ft = (Some actuals, ret_t) in begin
	      match f with
		  C.Fname x -> update_funtyp x ft
		| _ -> ()
	    end;
	    ft
      | Some _ -> (args_t, ret_t)
  in

  let translate_unop x t = 
    match (x, t) with
	(Not, C.Int _) -> (C.Not, C.int_typ)
      | (BNot, C.Int k) -> 
(* TODO: function promote should be in CoreC, not in Cir 
   (or even in Csyntax rather?) Or even better in Csyntax2CoreC??? *)
	  let k = C.promote k in
	    (C.BNot k, C.Int k)
      | _ -> 
	  Npkcontext.report_error "Csyntax2CoreC.translate_unop"
	    "unexpected unary operator and argument"
  in

  let translate_binop op t1 t2 = 
    let t_in = 
      match (op, t1, t2) with
	  (Minus, C.Ptr _, C.Ptr _) -> C.int_typ

	| ((Mult|Plus|Minus|Div|Mod|BAnd|BXor|BOr|Gt|Eq), 
	   C.Int k1, C.Int k2) -> 
	    C.Int (Newspeak.max_ikind (C.promote k1) (C.promote k2))

	| ((Mult|Plus|Minus|Div|Gt|Eq), C.Float n1, C.Float n2) ->
	    C.Float (max n1 n2)

	| ((Mult|Plus|Minus|Div|Gt|Eq), C.Float _, C.Int _) -> t1

	| ((Mult|Plus|Minus|Div|Gt|Eq), C.Int _, C.Float _) -> t2

	| ((Shiftl|Shiftr), C.Int (_, n), C.Int _) -> 
	    C.Int (Newspeak.Unsigned, n)

	| (Plus, C.Int _, C.Ptr _) -> 
	    Npkcontext.report_accept_warning "Firstpass.normalize_binop"
	      "addition of a pointer to an integer" Npkcontext.DirtySyntax;
	    t2

	| _ -> t1
    in
    let t_out =
      match op with
	  Gt|Eq -> C.int_typ
	| _ -> t_in
    in
    let op =
(* TODO: have csyntax use coreC operators!!! idem for unop!! *) 
      match op with
	  Mult -> C.Mult
	| Plus -> C.Plus
	| Minus -> C.Minus
	| Div -> C.Div
	| Mod -> C.Mod
	| BAnd -> C.BAnd
	| BXor -> C.BXor
	| BOr -> C.BOr
	| Shiftl -> C.Shiftl
	| Shiftr -> C.Shiftr
	| Gt -> C.Gt
	| Eq -> C.Eq
    in
      (op, t_in, t_out)
  in

  let rec translate_lv e =
    match e with
	Cst (c, t) -> 
	  let t = translate_typ t in
	    (C.Cst (c, t), t)
      | Var x -> find_symb x
      | RetVar -> find_symb ret_name
      | Field (e, f) -> 
	  let (e, t) = translate_exp e in
	  let (r, _) = C.comp_of_typ t in
	  let f_t = find_field f r in
	    (C.Field ((e, t), f), f_t)
	      (* TODO: should merge Index and Deref in Csyntax, only have one of them!! *)
      | Index (a, idx) -> 
	  let (a, t) = translate_lv a in
	  let idx = translate_exp idx in begin
	      match t with
		  C.Array (t, len) -> (C.Index (a, (t, len), idx), t)
		| C.Ptr elt_t -> 
		    (C.Deref (C.Binop ((C.Plus, t), (a, t), idx), t), elt_t)
		| _ -> 
		    Npkcontext.report_error "Csyntax2CoreC.translate_exp"
		      "pointer or array expected"
	    end

      | AddrOf e -> 
	  let (e, t) = translate_lv e in
	    (C.AddrOf (e, t), C.Ptr t)
      | Unop (op, e) -> 
	  let (e, t) = translate_exp e in
	  let (op, t) = translate_unop op t in
	    (C.Unop (op, e), t)
      | Binop (op, e1, e2) -> 
	  let (e1, t1) = translate_exp e1 in
	  let (e2, t2) = translate_exp e2 in
	  let (op, t_in, t_out) = translate_binop op t1 t2 in
	    (C.Binop ((op, t_in), (e1, t1), (e2, t2)), t_out)
      | IfExp (c, e1, e2) -> 
	  let (c, _) = translate_exp c in
	  let (e1, t1) = translate_exp e1 in
	  let (e2, t2) = translate_exp e2 in
	  let t = 
	    match (t1, t2) with
		(C.Ptr _, C.Int _) -> t1
	      | (C.Int _, C.Ptr _) -> t2
	      | _ when t1 = t2 -> t1
	      | _ -> 
		  Npkcontext.report_error "Csyntax2CoreC.translate_exp"
		    "compatible type expected"
	  in
	    (C.IfExp (c, (e1, t1), (e2, t2), t), t)
      | Call (f, args) -> 
	  let (f, ft) = translate_funexp f in
	  let (args, actuals) = translate_args args in
	  let ft = refine_ftyp f ft actuals in
	  let (_, ret_t) = ft in
	    (C.Call (f, ft, args), ret_t)
      | Sizeof t -> (C.Sizeof (translate_typ t), C.uint_typ)
      | SizeofE e -> 
	  let (_, t) = translate_lv e in
	    (C.Sizeof t, C.uint_typ)
      | Offsetof (t, f) -> (C.Offsetof (translate_typ t, f), C.uint_typ)
      | Str x -> 
	  let len = C.exp_of_int ((String.length x) + 1) in
	    (C.Str x, C.Array (C.char_typ, Some len))
      | FunName -> translate_exp (Str !current_fun)
      | Cast (e, t) -> 
	  let e = translate_exp e in
	  let t = translate_typ t in
	    (C.Cast (e, t), t)
      | Set (lv, op, e) -> 
	  let (lv, t1) = translate_exp lv in
	  let (e, t2) = translate_exp e in
	  let op =
	    match op with
		None -> None
	      | Some op -> 
		  let (op, t_in, _) = translate_binop op t1 t2 in
		    Some (op, t_in)
	  in
	    (C.Set ((lv, t1), op, (e, t2)), t1)
      | OpExp (op, e, is_after) ->
	  let (e, t) = translate_exp e in
	  let (op, t_in, t_out) = translate_binop op t C.int_typ in
	    (C.OpExp ((op, t_in), (e, t), is_after), t_out)

      | BlkExp blk -> 
	  let (blk, t) = translate_blk_exp blk in
(* TODO: remove is_after in TypedC! *)
	    (C.BlkExp (blk, false), t)
	      
  and translate_exp e = 
    let (e, t) = translate_lv e in
      match t with
	  C.Array (t, len) -> 
	    (C.AddrOf (C.Index (e, (t, len), (C.exp_of_int 0, C.int_typ)), t), 
		       C.Ptr t)
	| C.Fun _ -> (C.AddrOf (e, t), C.Ptr t)
	| _ -> (e, t)

  (* TODO: introduce type funexp in corec??*)
  and translate_funexp f =
    let (f, ft) = translate_lv f in
      match (f, ft) with
	  (C.Global f, C.Fun t) -> (C.Fname f, t)
	| (C.Deref f, C.Fun t) -> (C.FunDeref f, t)
	| (_, C.Ptr (C.Fun t)) -> (C.FunDeref (f, ft), t)
	| _ -> 
	    Npkcontext.report_error "Csyntax2CoreC.translate_call"
	      "function expression expected"

  and translate_args x = 
    let rec translate x i =
      match x with
	  e::tl -> 
	    let (e, t) = translate_exp e in
	    let (args, typs) = translate tl (i+1) in
	      ((e, t)::args, (t, "arg"^(string_of_int i))::typs)
	| [] -> ([], [])
    in
      translate x 0

  and translate_typ t =
    match t with
	Void -> C.Void
      | Int i -> C.Int i
      | Bitfield (i, e) -> 
	  let (e, _) = translate_exp e in
	    C.Bitfield (i, e)
      | Float i -> C.Float i
      | Ptr t -> C.Ptr (translate_typ t)
      | Array (t, len) -> 
	  let t = translate_typ t in
	  let len = 
	    match len with
		None -> None
	      | Some e -> 
		  let (e, _) = translate_exp e in
		    Some e
	  in
	    C.Array (t, len)
(* TODO: maybe the fact that it is a struct not needed in the type?? *)
      | Comp x -> C.Comp (find_compdef x)
      | Fun ft -> C.Fun (translate_ftyp ft)
      | Va_arg -> C.Va_arg
      | Typeof e -> 
	  let (_, t) = translate_lv e in 
	    t

  and translate_ftyp (args_t, ret_t) =
    let args_t = 
      match args_t with
	  None -> None
	| Some x -> 
	    let x = List.map (fun (t, x) -> (translate_typ t, x)) x in
	      Some x
    in
    let ret_t = translate_typ ret_t in
      (args_t, ret_t)


  and translate_decl is_global loc x d =
    Npkcontext.set_loc loc;
    match d with
	VDecl (_, _, extern, Some _) when extern -> 
	  Npkcontext.report_error "Firstpass.translate_global"
	    "extern globals can not be initizalized"
      | VDecl (_, static, extern, _) when static && extern -> 
	  Npkcontext.report_error "Firstpass.translate_global"
	    ("static variable can not be extern")
      | VDecl (Fun _, _, _, Some _) -> 
	  Npkcontext.report_error "Firstpass.translate_global"
	    ("unexpected initialization of function "^x)
      | VDecl (Fun ft, is_static, is_extern, _) -> 
	  let ft = translate_ftyp ft in
	    translate_proto_ftyp x is_static ft loc;
	    if (not is_global) then begin
	      Npkcontext.report_accept_warning "Firstpass.translate"
		"function declaration within block" Npkcontext.DirtySyntax
	    end;
	    C.VDecl (C.Fun ft, is_static, is_extern, None)
      | VDecl (t, is_static, is_extern, init) -> 
	  (* TODO: think about it, simplify?? *)
	  let t = translate_typ t in
	  let name = if is_static then get_static_name x loc else x in
	  let t = complete_typ_with_init t init in
	    if is_global then update_global x name t
	    else if is_static || is_extern
	    then Hashtbl.add symbtbl x (C.Global name, t)
	    else add_local (t, x);
	    let init =
	      match init with
		  None -> None
		| Some init -> Some (translate_init t init)
	    in
	      C.VDecl (t, is_static, is_extern, init)
      | EDecl e -> 
	  let (e, t) = translate_exp e in
	    Hashtbl.add symbtbl x (e, t);
	    C.EDecl e
      | CDecl (fields, is_struct) -> 
	  let fields = List.map translate_field_decl fields in
	  let decl = (fields, is_struct) in
	  let c = find_compdef x in
	    c := Some decl;
	    C.CDecl decl

  and translate_stmt (x, loc) = 
    Npkcontext.set_loc loc;
    match x with
	If (c, br1, br2) -> 
	  let (c, _) = translate_exp c in
	  let br1 = translate_blk br1 in
	  let br2 = translate_blk br2 in
	    C.If (c, br1, br2)
      | CSwitch (e, cases, default) -> 
	  let e = translate_exp e in
	  let cases = List.map translate_case cases in
	  let default = translate_blk default in
	    C.CSwitch (e, cases, default)
      | For (init, halt, body, continue) -> 
	  let init = translate_blk init in
	  let (halt, _) = translate_exp halt in
	  let body = translate_blk body in
	  let continue = translate_blk continue in
	    C.For (init, halt, body, continue)
      | DoWhile (body, e) -> 
	  let body = translate_blk body in
	  let (e, _) = translate_exp e in
	    C.DoWhile (body, e)
      | Exp e -> C.Exp (translate_exp e)
      | Break -> C.Break
      | Continue -> C.Continue
      | Return -> C.Return
      | Block body -> C.Block (translate_blk body)
      | Goto lbl -> C.Goto lbl
      | Label lbl -> C.Label lbl
      | UserSpec a ->  C.UserSpec (translate_assertion a)
      | LocalDecl _ -> 
	  Npkcontext.report_error "Csyntax2TypedC.translate_stmt"
	    "unreachable statement"

  and translate_case (e, body, loc) = 
    let (e, _) = translate_exp e in
      (e, translate_blk body, loc)

  and translate_spec_token x =
    match x with
	SymbolToken x -> C.SymbolToken x
      | IdentToken x -> begin
	  try
	    let (lv, t) = find_symb x in
	      match t with
		  C.Fun _ -> C.IdentToken x
		| _ -> C.LvalToken (lv, t)
	  with _ -> C.IdentToken x
	end
      | CstToken (c, _) -> C.CstToken c
	  
  and translate_assertion x = List.map translate_spec_token x

(* TODO: find a way to factor translate_blk and translate_blk_exp?? 
   rather remove side-effects before csyntax2CoreC.
*)
  and translate_blk x = 
    match x with 
	(LocalDecl (x, d), loc)::tl -> 
	  let decl = C.LocalDecl (x, translate_decl false loc x d) in
	  let tl = translate_blk tl in
	    remove_var x;
	    (decl, loc)::tl

      | (x, loc)::tl -> 
	  let stmt = translate_stmt (x, loc) in
	  let tl = translate_blk tl in
	    (stmt, loc)::tl
      | [] -> []

  and translate_blk_exp x =
    let rec translate_aux x =
      match x with
	  (Exp e, loc)::[] -> 
	    let (e, t) = translate_exp e in
	      ([], (e, t, loc))

	| (LocalDecl (x, d), loc)::tl -> 
	    let decl = C.LocalDecl (x, translate_decl false loc x d) in
	    let (tl, e) = translate_aux tl in
	      remove_var x;
	      ((decl, loc)::tl, e)
		
	| (hd, loc)::tl -> 
	    let hd = (translate_stmt (hd, loc), loc) in
	    let (blk, e) = translate_aux tl in
	      (hd::blk, e)
	| [] -> 
	    Npkcontext.report_error "Csyntax2CoreC.translate_blk_exp" 
	      "expression expected at end of block"
    in
    let (blk, (e, t, loc)) = translate_aux x in
      (blk@(C.Exp (e, t), loc)::[], t)
  
  and translate_field_decl (t, x, _) = (x, translate_typ t)

  and translate_init t x =
    match (x, t) with
(* TODO: redundant code with complete_init?? *)
	((Data (Str str)|Sequence ([(None, Data (Str str))])), 
	 C.Array (C.Int (_, n), _)) when n = Config.size_of_char ->
	  let seq = seq_of_string str in
	    translate_init t (Sequence seq)
	      
      | (Data e, _) -> C.Data (translate_exp e)

      | (Sequence seq, C.Array (t, _)) -> 
	  let seq = 
	    List.map (fun (x, init) -> (x, translate_init t init)) seq 
	  in
	    C.Sequence seq
	      
      | (Sequence seq, C.Comp { contents = Some (f, true) }) ->
	  C.Sequence (translate_field_sequence seq f)

      | (Sequence ((Some f, init)::[]), 
	 C.Comp { contents = Some (r, false)}) -> 
	  let t = find_field f r in
	  let seq = (Some f, translate_init t init)::[] in
	    C.Sequence seq

      | (Sequence _, _) -> 
	  Npkcontext.report_error "Csyntax2CoreC.translate_init"
	    "this type of initialization not implemented yet"

  and translate_field_sequence seq fields =
    match (fields, seq) with
	((_, t)::fields, (expected_f, init)::seq) -> 
	  let init = translate_init t init in
	  let seq = translate_field_sequence seq fields in
	    (expected_f, init)::seq
      
      | ([], []) -> []

      | (_, []) ->
	  Npkcontext.report_accept_warning 
	    "Firstpass.translate_init.translate_field_sequence" 
	    "missing initializers for structure" Npkcontext.DirtySyntax;
	    []

      | _ -> Npkcontext.report_error "Csyntax2CoreC" "case not implemented yet"
  in

  let translate_fundecl (f, f', ft, static, body, loc) =
    Npkcontext.set_loc loc;
    current_fun := f;
    add_formals ft;
    let body = translate_blk body in
      remove_formals ft;
      current_fun := "";
      (f', (ft, static, body, loc))
  in

  let rec translate_globals x = 
    let glbdecls = ref [] in
    let fundecls = ref [] in
    let specs = ref [] in
      
    let translate (x, loc) =
      Npkcontext.set_loc loc;
      match x with
	  FunctionDef (f, (args_t, ret_t), static, body) -> 
	    let args_t = 
	      match args_t with
		  None -> []
		| Some args_t -> args_t
	    in
	    let ft = (Some args_t, ret_t) in
	    let ft = translate_ftyp ft in
	    let f' = update_funsymb f static ft loc in
	      fundecls := (f, f', ft, static, body, loc)::(!fundecls)
		
	| GlbDecl (x, d) -> 
	    glbdecls := (x, (translate_decl true loc x d, loc))::(!glbdecls)
	      
	| GlbUserSpec x -> specs := (translate_assertion x)::!specs
    in
      
      List.iter translate x;
      (!glbdecls, !fundecls, !specs)
  in

  let (glbdecls, fundecls, specs) = translate_globals globals in
  let fundecls = List.map translate_fundecl fundecls in
    (glbdecls, fundecls, specs)
