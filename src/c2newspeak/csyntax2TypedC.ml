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

open Csyntax
module Nat = Newspeak.Nat
module C = TypedC


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

(* TODO: remove the maximum of occurences of symbtbl *)

let process (fname, globals) =
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

  let generate_global_name is_static x =
    if is_static then begin
      let prefix = "!"^fname^"." in
      let prefix = 
	if !current_fun = "" then prefix else prefix^(!current_fun)^"."
      in
      let counter = (string_of_int !static_cnt)^"." in
      let name = prefix^counter^x in
	incr static_cnt;
	name
    end else x
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
    
(* TODO: think about it, but I am not sure the distinction between Local and 
   Global should be made in TypedC right away... *)
  let add_local (t, x) = Hashtbl.add symbtbl x (C.Local x, t) in

  let remove_local x = Hashtbl.remove symbtbl x in

  let replace_symbol_type x t =
    let (i, _) = Hashtbl.find symbtbl x in
      Hashtbl.replace symbtbl x (i, t)
  in

(* TODO: find a way to factor declare_global and add_local
   maybe necessary to complete_typ_with_init in both cases...
   maybe just needs a is_global bool as argument..
   need to check with examples!!!
*)
  let update_global x name t = Hashtbl.replace symbtbl x (C.Global name, t) in

  let add_formals (args_t, ret_t) =
    add_local (ret_t, Temps.return_value);
(* TODO: think about it, not nice to have this pattern match, since in
   a function declaration there are always arguments!! 
   None is not possible *)
    match args_t with
	Some args_t -> List.iter add_local args_t
      | None -> 
	  Npkcontext.report_error "Csyntax2TypedC.add_formals" "unreachable code"
  in

  let remove_formals (args_t, _) =
    remove_local Temps.return_value;
(* TODO: think about it, not nice to have this pattern match, since in
   a function declaration there are always arguments!! 
   None is not possible *)
    match args_t with
	Some args_t -> List.iter (fun (_, x) -> remove_local x) args_t
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

  (* TODO: think about it: composite definitions are never removed from 
     the comptbl!! *)
  let find_compdef name =
    try Hashtbl.find comptbl name
    with Not_found -> 
      let c = TypedC.Unknown name in
	Hashtbl.add comptbl name c;
	c
  in

  let update_fdecl s (args, ret) =
    let rec update t =
      match t with 
	  C.Comp (C.Unknown s') when s = s' -> C.Comp (find_compdef s)
	| C.Comp (C.Known (l, is_struct))   -> C.Comp (C.Known (List.map (fun (s, t) -> (s, update t)) l, is_struct))
	| C.Ptr t'                          -> C.Ptr (update t')
	| C.Fun (args, ret) 		    ->
	    let ret' = update ret in
	    let args' = 
	      match args with 
		  None -> None 
		| Some l -> Some (List.map (fun (t, x) -> update t, x) l)
	    in 
	      C.Fun(args', ret')
	| _                                 -> t
    in
    let args' = 
      match args with 
	  Some args -> Some (List.map (fun (t, x) -> update t, x) args)
	| None -> None
    in
    let ret' = update ret in
      args', ret'
  in

  let update_struct_type s t =
    let new_type = find_compdef s in
    let rec update t = 
      match t with 
	  C.Known (fields, true) -> 
	    let fields' = List.map (fun (n, t) -> 
				      let t' = 
					match t with 
					    C.Ptr (C.Comp (C.Unknown s')) when s = s' ->  
					      C.Ptr (C.Comp new_type)
					  | _ -> t
				      in (n, t')) fields in
	      C.Known (fields', true)
	| _ -> t
    in
      update t
  in

  let update_vdecl s (x, (v, loc)) =
    let rec update t =
      match t with
	  C.Comp (C.Unknown s') when s = s' -> C.Comp (find_compdef s)
	| C.Comp t -> C.Comp (update_struct_type s t)
	| C.Ptr t' -> C.Ptr (update t')
	| C.Fun (args, ret) 		    ->
	    let ret' = update ret in
	    let args' = 
	      match args with 
		  None -> None 
		| Some l -> Some (List.map (fun (t, x) -> update t, x) l)
	    in 
	      C.Fun(args', ret')
	| _ -> t
    in 
    let t' = update v.C.t in
    let v = { v with C.t = t' } in
      replace_symbol_type x t';
      (x, (v, loc))
  in

(* TODO: think about this, but it seems to be costly!! *)
  let update_local_vdecls s =
    let vars, pvars = 
      Hashtbl.fold (fun
		      x (e, t) (vars, pvars) ->
			match e, t with 
			    C.Local _, C.Comp (C.Unknown s') when s = s' 	  -> 
			      x::vars, pvars
			  | C.Local _, C.Ptr (C.Comp (C.Unknown s')) when s = s' -> 
			      vars, x::pvars
			  | _ 							  -> 
			      vars, pvars	   
		   ) symbtbl ([], [])
    in
    let t' = C.Comp (find_compdef s) in
      List.iter (fun x -> replace_symbol_type x t') vars;
      List.iter (fun x -> replace_symbol_type x (C.Ptr t')) pvars
  in

  let update_funtyp f ft1 =
    let (_, t) = Hashtbl.find symbtbl f in
    let ft2    = TypedC.ftyp_of_typ t in
    let ft     = TypedC.min_ftyp ft1 ft2 in
      replace_symbol_type f (C.Fun ft)
  in

  let update_funsymb f is_static ft =
    try 
      update_funtyp f ft;
      let (symb, _) = Hashtbl.find symbtbl f in begin
	  match symb with
	      C.Global x -> x
	    | _ -> f
	end
    with Not_found -> 
      let f' = generate_global_name is_static f in 
	update_global f f' (C.Fun ft);
	f'
  in

  let refine_ftyp f (args_t, ret_t) actuals = 
    match args_t with
	None -> 
	  Npkcontext.report_accept_warning "Csyntax2TypedC.refine_ftyp"  
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
      | (Not, C.Ptr _) -> (C.Not, C.int_typ)
      | (BNot, C.Int k) -> 
(* TODO: function promote should be in CoreC, not in Cir 
   (or even in Csyntax rather?) Or even better in Csyntax2CoreC??? *)
	  let k = C.promote k in
	    (C.BNot k, C.Int k)
      | _ -> 
	  Npkcontext.report_error "Csyntax2TypedC.translate_unop"
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
      | RetVar -> find_symb Temps.return_value
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
		  C.Array (t, len) -> 
		    (C.Index (a, (t, len), idx), t)
		| C.Ptr elt_t -> 
		    (C.Deref (C.Binop ((C.Plus, t), (a, t), idx), t), elt_t)
		| _ -> 
		    Npkcontext.report_error "Csyntax2TypedC.translate_exp"
		      "pointer or array expected"
	    end

      | AddrOf e -> 
	  let (e, t) = translate_lv e in
	    (C.AddrOf (e, t), C.Ptr t)
      | Unop (op, e) -> 
	  let (e, t1) = translate_exp e in
	  let (op, t2) = translate_unop op t1 in
	    (C.Unop (op, t1, e), t2)
      | Binop (op, e1, e2) -> 
	  let (e1, t1) = translate_exp e1 in
	  let (e2, t2) = translate_exp e2 in
	  let (op, t_in, t_out) = translate_binop op t1 t2 in
	    (C.Binop ((op, t_in), (e1, t1), (e2, t2)), t_out)
      | IfExp (c, e1, e2) -> 
	  let (c, _) = translate_exp c in
	  let (e1, t1) = translate_exp e1 in
	  let (e2, t2) = translate_exp e2 in
	  let (e1, e2, t) = 
	    match (t1, t2) with
		(C.Ptr _, C.Int _) | (C.Float _, C.Int _) -> 
		  (e1, C.Cast ((e2, t2), t1), t1)
	      | (C.Int _, C.Ptr _) | (C.Int _, C.Float _) -> 
		  (C.Cast ((e1, t1), t2), e2, t2)
	      | (C.Ptr _, C.Ptr _) -> (e1, e2, t1)
	      | (C.Int k1, C.Int k2) when k1 <> k2 -> 
		  let k = Newspeak.max_ikind (C.promote k1) (C.promote k2) in
		  let t = C.Int k in
		  let e1 = if k = k1 then e1 else C.Cast ((e1, t1), t) in
		  let e2 = if k = k2 then e2 else C.Cast ((e2, t2), t) in
		    (e1, e2, t)
(* TODO: couldn't it run forever?? *)
	      | _ when t1 = t2 -> (e1, e2, t1)
	      | _ -> 
		  Npkcontext.report_error "Csyntax2TypedC.translate_exp"
		    "compatible type expected"
	  in
	    (* TODO: could simplify this?!!! *)
	    (C.IfExp (c, (e1, t), (e2, t), t), t)
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
	    Npkcontext.report_error "Csyntax2TypedC.translate_call"
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

  and translate_fdecl x ft is_static init = 
    match init with
	Some _ -> 
	  Npkcontext.report_error "Firstpass.translate_global"
	    ("unexpected initialization of function "^x)
      | _ -> 
	  let (args, ret) = translate_ftyp ft in
	    if args = None then begin
	      Npkcontext.report_warning "Csyntax2TypedC.check_proto_ftyp" 
		("incomplete prototype for function "^x)
	    end;
	    let _ = update_funsymb x is_static (args, ret) in
	      ()

  and translate_vdecl is_global loc x d =
    Npkcontext.set_loc loc;
    match d.initialization with
	Some _ when d.is_extern -> 
	  Npkcontext.report_error "Firstpass.translate_global"
	    "extern globals can not be initizalized"
      | _ when d.is_static && d.is_extern -> 
	  Npkcontext.report_error "Firstpass.translate_global"
	    ("static variable can not be extern")
      | _ -> 
	  (* TODO: think about it, simplify?? *)
	  let t = translate_typ d.t in
	  let t = complete_typ_with_init t d.initialization in
	  let name = generate_global_name d.is_static x in
	    if is_global then update_global x name t else add_local (t, x);
	    let init =
	      match d.initialization with
		  None -> None
		| Some init -> Some (translate_init t init)
	    in
	      { 
		C.name = name;
		C.t = t;
		C.is_static = d.is_static;
		C.is_extern = d.is_extern;
		C.initialization = init
	      }

  and translate_edecl x e = Hashtbl.add symbtbl x (translate_exp e)

  and translate_cdecl x (fields, is_struct) =
    let fields = List.map translate_field_decl fields in
    let decl = C.Known (fields, is_struct) in
    let is_unknown t =
      match t with
	  C.Comp (C.Unknown s) when s = x -> true
	| _ -> false
    in 
    let check_type s (_, t) symbols =
      if is_unknown t then s::symbols else symbols
    in 
    let update_type s =
      let symbols = try Hashtbl.find_all symbtbl s with Not_found -> [] in
      let symbols' = List.map (fun (v, t) -> if is_unknown t then (v, C.Comp decl) else (v, t)) symbols in
	List.iter (fun _ -> Hashtbl.remove symbtbl s) symbols;
	List.iter (fun x -> Hashtbl.add symbtbl s x) (List.rev symbols')
    in
      Hashtbl.add comptbl x decl;
      let symbols = Hashtbl.fold check_type symbtbl [] in
	List.iter update_type symbols

  and translate_stmt x = 
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
    let (blk, _) = translate_blk_exp x in
      blk

  and translate_blk_exp x =
    match x with
	(Exp e, loc)::[] -> 
	  Npkcontext.set_loc loc;
	  let (e, t) = translate_exp e in
	    ((C.Exp (e, t), loc)::[], t)

(* TODO: separate CDecl from other Decls in csyntax?? *)
      | (LocalDecl (x, CDecl d), loc)::tl -> 
	  Npkcontext.set_loc loc;
	  translate_cdecl x d;
	  let (tl, e) = translate_blk_exp tl in
	    update_local_vdecls x;
	    Hashtbl.remove comptbl x;
	    (tl, e)

(* TODO: find a way to factor this case with the next one *)
      | (LocalDecl (x, EDecl d), loc)::tl -> 
	  Npkcontext.set_loc loc;
	  translate_edecl x d;
	  let (tl, e) = translate_blk_exp tl in
	    remove_local x;
	    (tl, e)
(* TODO: find a way to factor this case with the next one and maybe 
   global declarations!! *)
      | (LocalDecl (x, VDecl d), loc)::tl -> begin
	    match d.t with
		Fun ft -> 
		  Npkcontext.set_loc loc;
		  translate_fdecl x ft d.is_static d.initialization;
		  Npkcontext.report_accept_warning "Firstpass.translate"
		    "function declaration within block" Npkcontext.DirtySyntax;
		  let (tl, e) = translate_blk_exp tl in
		    remove_local x;
		    (tl, e)
	      | _ -> 
		  Npkcontext.set_loc loc;
		  let is_global = d.is_static || d.is_extern in
		  let v = translate_vdecl is_global loc x d in
		  let decl = C.LocalDecl (x, v) in
		  let (tl, e) = translate_blk_exp tl in
		    if not is_global then remove_local x;
		    ((decl, loc)::tl, e)
	  end
	      
      | (x, loc)::tl -> 
	  Npkcontext.set_loc loc;
	  let hd = (translate_stmt x, loc) in
	  let (blk, t) = translate_blk_exp tl in
	    (hd::blk, t)

      | [] -> ([], C.Void)
      
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
	      
      | (Sequence seq, C.Comp (TypedC.Known (f, true))) ->
	  C.Sequence (translate_field_sequence seq f)

      | (Sequence ((None, init)::[]), C.Comp (TypedC.Known (r, false))) ->
	  let t = 
	    match r with
		(_, b)::_ -> b
	      | _ -> 
		  Npkcontext.report_error "Firstpass.translate_init"
		    "unexpected empty union"
	  in
	  let seq = (None, translate_init t init)::[] in
	    C.Sequence seq

      | (Sequence ((Some f, init)::[]), 
	 C.Comp (TypedC.Known (r, false))) -> 
	  let t = find_field f r in
	  let seq = (Some f, translate_init t init)::[] in
	    C.Sequence seq

      | (Sequence seq, C.Ptr t) ->
	  let seq = 
	    List.map (fun (x, init) -> (x, translate_init t init)) seq in
	    C.Sequence seq

      | (Sequence seq, C.Int _) ->
	  let seq = 
	    List.map (fun (x, init) -> (x, translate_init t init)) seq in
	    C.Sequence seq

      | (Sequence _, _) ->
	  Npkcontext.report_error "Csyntax2TypedC.translate_init"
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

      | _ -> Npkcontext.report_error "Csyntax2TypedC" "case not implemented yet"
  in

  (* TODO: remove the static field if not necessary! *)
  (* TODO: should do a grep on all _ to see if they shouldn't be removed *)
  let translate_fundecl (f, f', ft, _, body, loc) =
    Npkcontext.set_loc loc;
    current_fun := f;
    add_formals ft;
    let body = translate_blk body in
    let declaration = 
      {
	C.function_type = ft;
	C.body = body;
	C.position = loc;
      } 
    in
      remove_formals ft;
      current_fun := "";
      (f', declaration)
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
	    let f' = update_funsymb f static ft in
	      fundecls := (f, f', ft, static, body, loc)::(!fundecls)
		
	| GlbDecl (x, CDecl d) -> 
	    translate_cdecl x d;
	    let update_function_argument f (symb, t) l =
	      let t' = 
		match t with
		    TypedC.Fun ft -> TypedC.Fun (update_fdecl x ft)
		  | _             -> t
	      in
		(f, (symb, t'))::l	      
	    in
	    (* updating the sig of fun whose one of the parameters is
	       of the form C.Comp (Unknown ...) *)
	    (* TODO: think about this, but this must be costly *)
	    let fdecls = Hashtbl.fold update_function_argument symbtbl [] in
	      List.iter (fun (f, k) -> Hashtbl.replace symbtbl f k) fdecls;
	      (* updating the type of struct type whose one of the field
		 is of the form Ptr (C.Comp (Unknown ...)) *)
	      let comps = Hashtbl.fold (fun x' v l ->
					  let v' = update_struct_type x v in
					    if v' = v then l else (x', v')::l
				       ) comptbl []
	      in
		List.iter (fun (x, v) -> Hashtbl.replace comptbl x v) (List.rev comps);
		(* udapting the type of var of kind C.Comp (Unknown
		...)) ; has to be done after the update of struct
		types *)
		glbdecls := List.map (update_vdecl x) !glbdecls

	| GlbDecl (x, EDecl d) -> translate_edecl x d

	| GlbDecl (x, VDecl d) -> begin
	    match d.t with
		Fun ft -> translate_fdecl x ft d.is_static d.initialization
	      | _ -> 
	    let v = translate_vdecl true loc x d in
	      glbdecls := (x, (v, loc))::(!glbdecls)
	  end
	      
	| GlbUserSpec x -> specs := (translate_assertion x)::!specs
    in
      
      List.iter translate x;
      (!glbdecls, !fundecls, !specs)
  in
   
  let (glbdecls, fundecls, specs) = translate_globals globals in
  let fundecls = List.map translate_fundecl fundecls in
    {
      TypedC.global_variables = glbdecls;
      TypedC.function_declarations = fundecls;
      TypedC.user_specifications = specs;
    }
