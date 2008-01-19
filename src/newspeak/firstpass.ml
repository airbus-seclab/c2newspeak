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

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org
*)

(* Translates bare_csyntax to csyntax *)
open Csyntax
open Bare_csyntax
module C = Csyntax

let ret_pos = 1
let ret_name = "!return"

let char_typ = C.Int (Newspeak.Signed, Config.size_of_char)

let rec simplify_bexp e =
  match e with
      Var _ | Field _ | Index _ | Deref _ | Call _ -> 
	Unop (Not, Binop (Eq, e, exp_of_int 0))
    | Unop (Not, e) -> Unop (Not, simplify_bexp e)
    | _ -> e


(* TODO: code cleanup: find a way to factor this with create_cstr
   in Npkil *)
let seq_of_string str =
  let len = String.length str in
  let res = ref [Data (exp_of_int 0)] in
    for i = len - 1 downto 0 do
      let c = Char.code str.[i] in
	res := (Data (exp_of_int c))::!res
    done;
    !res

let translate_arithmop op (e1, k1) (e2, k2) =
  let k = Newspeak.max_ikind (C.promote k1) (C.promote k2) in
  let t = C.Int k in
    (op k, C.cast (e1, C.Int k1) t, C.cast (e2, C.Int k2) t, t)

let translate_floatop op (e1, n1) (e2, n2) =
  let n = max n1 n2 in
  let t = C.Float n in
    (op n, C.cast (e1, C.Float n1) t, C.cast (e2, C.Float n2) t, t)

let translate_cst c = (C.Const c, C.typ_of_cst c)

let rec simplify_binop op (e1, t1) (e2, t2) =
  match (op, t1, t2) with
    | (Minus, C.Ptr _, C.Int _) -> 
	let e2 = translate_binop Minus (C.exp_of_int 0, t2) (e2, t2) in
 	  (Plus, (e1, t1), e2)

    | (Plus, C.Array (elt_t, _), _) -> 
	let t = C.Ptr elt_t in
	  (Plus, (C.cast (e1, t1) t, t), (e2, t2))

    | (Minus, C.Array (elt_t, _), _) ->
	let t = C.Ptr elt_t in
	  simplify_binop Minus (C.cast (e1, t1) t, t) (e2, t2)
    | (Minus, _, C.Array (elt_t, _)) ->
	let t = C.Ptr elt_t in
	  simplify_binop Minus (e1, t1) (C.cast (e2, t2) t, t)

    | (Mult|Plus|Minus|Div|Gt|Eq as op, C.Float _, C.Int _) ->
	let e2 = C.cast (e2, t2) t1 in
	  (op, (e1, t1), (e2, t1))
	    
    | (Mult|Plus|Minus|Div|Gt|Eq as op, C.Int _, C.Float _) ->
	let e1 = C.cast (e1, t1) t2 in
	  (op, (e1, t2), (e2, t2))
	    
    | _ -> (op, (e1, t1), (e2, t2))

and translate_binop op e1 e2 =
  let (op, (e1, t1), (e2, t2)) = simplify_binop op e1 e2 in
  let (op, e1, e2, t) =
    match (op, t1, t2) with
	(* Arithmetic operations *)
	(* TODO: code cleanup? factor? *)
	(Mult, C.Int k1, C.Int k2) -> 
	  translate_arithmop (fun x -> C.Mult x) (e1, k1) (e2, k2)
      | (Plus, C.Int k1, C.Int k2) -> 
	  translate_arithmop (fun x -> C.Plus x) (e1, k1) (e2, k2)
      | (Minus, C.Int k1, C.Int k2) -> 
	  translate_arithmop (fun x -> C.Minus x) (e1, k1) (e2, k2)
      | (Div, C.Int k1, C.Int k2) -> 
	  translate_arithmop (fun x -> C.Div x) (e1, k1) (e2, k2)
      | (Mod, C.Int k1, C.Int k2) -> 
	  translate_arithmop (fun _ -> C.Mod) (e1, k1) (e2, k2)
      | (BAnd, C.Int k1, C.Int k2) -> 
	  translate_arithmop (fun x -> C.BAnd x) (e1, k1) (e2, k2)
      | (BXor, C.Int k1, C.Int k2) -> 
	  translate_arithmop (fun x -> C.BXor x) (e1, k1) (e2, k2)
      | (BOr, C.Int k1, C.Int k2) -> 
	  translate_arithmop (fun x -> C.BOr x) (e1, k1) (e2, k2)
	    
      | (Shiftl, C.Int (_, n), C.Int _) -> 
	  let k = (Newspeak.Unsigned, n) in
	  let t = C.Int k in
	  let e1 = C.cast (e1, t1) t in
	    (C.Shiftl k, e1, e2, t)

      | (Shiftr, C.Int (_, n), C.Int _) -> 
	  let k = (Newspeak.Unsigned, n) in
	  let t = C.Int k in
	  let e1 = C.cast (e1, t1) t in
	    (C.Shiftr k, e1, e2, t)
	      
      (* Float operations *)
      | (Mult, C.Float n1, C.Float n2) -> 
	  translate_floatop (fun x -> C.MultF x) (e1, n1) (e2, n2)
      | (Plus, C.Float n1, C.Float n2) -> 
	  translate_floatop (fun x -> C.PlusF x) (e1, n1) (e2, n2)
      | (Minus, C.Float n1, C.Float n2) -> 
	  translate_floatop (fun x -> C.MinusF x) (e1, n1) (e2, n2)
      | (Div, C.Float n1, C.Float n2) -> 
	  translate_floatop (fun x -> C.DivF x) (e1, n1) (e2, n2)
	    
      (* Pointer operations *)
      | (Plus, C.Ptr t, C.Int _) -> (C.PlusP t, e1, e2, t1)

      | (Minus, C.Ptr _, C.Ptr _) -> (C.MinusP, e1, e2, int_typ)
	  
      (* Integer comparisons *)
      | (Gt, C.Int k1, C.Int k2) -> 
	  translate_arithmop (fun x -> C.Gt (C.Int x)) (e1, k1) (e2, k2)
      | (Eq, C.Int k1, C.Int k2) -> 
	  translate_arithmop (fun x -> C.Eq (C.Int x)) (e1, k1) (e2, k2)

      (* Float comparisons *)
      | (Gt, C.Float n1, C.Float n2) -> 
	  translate_floatop (fun x -> C.Gt (C.Float x)) (e1, n1) (e2, n2)
      | (Eq, C.Float n1, C.Float n2) -> 
	  translate_floatop (fun x -> C.Eq (C.Float x)) (e1, n1) (e2, n2)
	    
      (* Pointer comparisons *)
      | (Eq, C.Ptr _, C.Ptr _) -> (C.Eq t1, e1, e2, int_typ)
      | (Eq, C.Int _, C.Ptr _) ->
	  let e1 = C.cast (e1, t1) t2 in
	    (C.Eq t2, e1, e2, int_typ)
      | (Eq, C.Ptr _, C.Int _) ->
	  let e2 = C.cast (e2, t2) t1 in
	    (C.Eq t1, e1, e2, int_typ)
	      
      | _ ->
	  Npkcontext.error "Csyntax.translate_binop" 
	    "Unexpected binary operator and arguments"
  in
    (C.Binop (op, e1, e2), t)

let translate_unop op (e, t) = 
  match (op, t, e) with
      (Neg, C.Int _, C.Const C.CInt c) 
	when Int64.compare c Int64.min_int <> 0 -> 
	  translate_cst (C.CInt (Int64.neg c))
    | (Neg, C.Int _, _) -> translate_binop Minus (C.exp_of_int 0, t) (e, t)
    | (Neg, C.Float _, _) -> 
	translate_binop Minus (C.exp_of_float 0., t) (e, t)
    | (Not, C.Int _, _) -> (C.Unop (C.Not, e), int_typ)
    | (BNot, C.Int k, _) -> 
	let k' = C.promote k in
	let t' = C.Int k' in
	  (C.Unop (C.BNot k', C.cast (e, t) t'), t')
    | _ -> 
	Npkcontext.error "Csyntax.translate_unop" 
	  "Unexpected unary operator and argument"

(* TODO: simplify by get a location for all of them *)
type symb =
    | Enum of Int64.t
    | Glb of string 
    | Local of (int * Newspeak.location)

(* TODO: remove variables hoisting at the function level *)
let translate fname (bare_compdefs, globals) =
  let compdefs = Hashtbl.create 100 in
  let glbdecls = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in

  let symbtbl = Hashtbl.create 100 in
  let vcnt = ref 0 in
  let locals = ref [] in

  let tmp_cnt = ref 0 in
  let static_pref = ref ("!"^fname^".") in

  let get_locals () =
    let res = !locals in
      locals := [];
      vcnt := 0;
      List.iter (fun (_, x, _) -> Hashtbl.remove symbtbl x) res;
      res
  in

  let update_global x name (t, loc, init) =
    try 
      let (prev_t, _, prev_init) = Hashtbl.find glbdecls name in
	if (t <> prev_t) then begin
	  Npkcontext.error "Firstpass.update_global" 
	    ("Global variable "^x^" declared with different types")
	end;
	let init = 
	  match (prev_init, init) with
	      (None, Some _) | (Some None, Some _) -> init
	    | (Some _, None) | (Some _, Some None) -> prev_init
	    | (None, None) -> None
	    | (Some Some _, Some Some _) -> 
		Npkcontext.error "Firstpass.update_global"
		  ("Global variable "^x^" initialized twice")
	in
	  Hashtbl.replace symbtbl x (Glb name, t);
	  Hashtbl.replace glbdecls name (t, loc, init)	  
    with Not_found -> 
      Hashtbl.add symbtbl x (Glb name, t);
      Hashtbl.add glbdecls name (t, loc, init)
  in

  let add_global x d = update_global x x d in

  let add_static x d =
    let name = !static_pref^x in
      update_global x name d
  in

  let remove_static x = Hashtbl.remove symbtbl x in

  let push_enum (x, i) loc = 
    Hashtbl.add symbtbl x (Enum i, C.int_typ) 
  in

  let pop_enum x = Hashtbl.remove symbtbl x in

  let push_var loc (t, x) = 
    incr vcnt;
    Hashtbl.add symbtbl x (Local (!vcnt, loc), t);
    !vcnt
  in

  let push_local = push_var in
  let pop_local x =
    let (v, t) = Hashtbl.find symbtbl x in
    let loc = 
      match v with
	  Local (_, loc) -> loc
	| _ -> Npkcontext.error "Firstpass.pop_local" "unreachable statement"
    in
      
      locals := (t, x, loc)::!locals;
      Hashtbl.remove symbtbl x 
  in
    
  let push_formals loc (args_t, _, ret_t) = 
    let _ = push_var loc (ret_t, ret_name) in
    let _ = List.map (push_var loc) args_t in
      ()
  in

  let create_tmp t loc = 
    let name = "tmp"^(string_of_int !tmp_cnt) in
    let x = push_local loc (t, name) in
      incr tmp_cnt;
      (C.Local x, name)
  in

(* TODO: could be a function name inside *)
  let lv_of_var x =
    match x with
	Local (n, _) -> C.Local n
      | Glb g -> C.Global g
      | Enum _ -> 
	  Npkcontext.error "Firstpass.translate.typ_of_var" 
	    "enum identifier can not be used as left value"
  in
    
  let find_symb x = 
    try Hashtbl.find symbtbl x
    with Not_found -> 
      Npkcontext.error "Firstpass.translate.typ_of_var" 
	("Unknown identifier "^x)
  in

  let rec translate_lv x =
    match x with
	Var x -> 
	  let (v, t) = find_symb x in
	  let lv = lv_of_var v in
	    ([], (lv, t))
      | Field (lv, f) -> 
	  let (pref, (lv, t)) = translate_lv lv in
	  let r = C.fields_of_typ compdefs t in
	  let (o, t) = List.assoc f r in
	    (pref, (C.Field (lv, o), t))
      | Index (lv, e) -> 
	  let (lv_pref, (lv', t)) = translate_lv lv in begin
	    match t with
		C.Array (t, n) ->
		  let (e_pref, (i, _)) = translate_exp e in
		    (lv_pref@e_pref, (C.Index (lv', (t, n), i), t))
	      | C.Ptr _ -> translate_lv (Deref (Binop (Plus, lv, e)))
	      | _ -> 
		  Npkcontext.error "Firstpass.translate_lv" 
		    "Array or pointer type expected"
	  end
      | Deref e -> 
	  let (pref, (e, t)) = translate_exp e in
	  let t = deref_typ t in
	    (pref, (C.Deref (e, t), t))

      | _ -> Npkcontext.error "Firstpass.translate_lv" "Left value expected"

  and translate_exp x =
    match x with
	Cst i -> ([], translate_cst i)

      | Var x -> 
	  let (v, t) = find_symb x in
	  let e = 
	    match v with
		Enum i -> C.Const (C.CInt i)
	      | _ -> 
		  let lv = lv_of_var v in
		    C.Lval (lv, t)
	  in
	    ([], (e, t))

      | Field _ | Index _ | Deref _ -> 
	  let (pref, (lv, t)) = translate_lv x in
	    (pref, (C.Lval (lv, t), t))

      | AddrOf lv -> 
	  let (pref, (lv, t)) = translate_lv lv in
	    (pref, (C.AddrOf (lv, t), C.Ptr t))

      | Unop (op, e) -> 
	  let (pref, e) = translate_exp e in
	  let e = translate_unop op e in
	    (pref, e)

      | Binop (op, e1, e2) -> 
	  let (pref1, e1) = translate_exp e1 in
	  let (pref2, e2) = translate_exp e2 in
	  let e = translate_binop op e1 e2 in
	    (pref1@pref2, e)

      | Call (f, args) -> translate_call f args

      | SizeofE (Str str) ->
	  let sz = String.length str + 1 in
	    ([], (C.exp_of_int sz, int_typ))

      | SizeofE e ->
	  let (_, (_, t)) = translate_exp e in
	  let sz = (C.size_of compdefs t) / 8 in
	    ([], (C.exp_of_int sz, int_typ))

      (* TODO: should check that the size of all declarations is less than max_int *)
      | Sizeof t -> 
	  let t = translate_typ t in
	  let sz = (C.size_of compdefs t) / 8 in
	    ([], (C.exp_of_int sz, int_typ))

      | Str str -> 
	  let e = add_glb_cstr str in
	    ([], e)

      (* TODO: this could be the unique place with And (remove the And down in 
	 the code of if then else !!! *)
      | And (e1, e2) -> 
	  let loc = Npkcontext.get_loc () in
	  let (tmp, tmp_name) = create_tmp int_typ loc in
	  let tmp_e = (C.Lval (tmp, int_typ), int_typ) in
	  let stmt = 
	    If (And (e1, e2), 
	       (Exp (Set (Var tmp_name, exp_of_int 1)), loc)::[], 
	       (Exp (Set (Var tmp_name, exp_of_int 0)), loc)::[])
	  in
	  let pref = translate_stmt (stmt, loc) in
	    pop_local tmp_name;
	    (pref, tmp_e)

      | Or (e1, e2) -> 
	  let loc = Npkcontext.get_loc () in
	  let (tmp, tmp_name) = create_tmp int_typ loc in
	  let tmp_e = (C.Lval (tmp, int_typ), int_typ) in
	  let stmt = 
	    If (Or (e1, e2), 
	       (Exp (Set (Var tmp_name, exp_of_int 1)), loc)::[], 
	       (Exp (Set (Var tmp_name, exp_of_int 0)), loc)::[])
	  in
	  let pref = translate_stmt (stmt, loc) in
	    pop_local tmp_name;
	    (pref, tmp_e)


      | Cast (e, t) -> 
	  let (pref, e) = translate_exp e in
	  let t = translate_typ t in
	  let e = C.cast e t in
	    (pref, (e, t))

      | Set (lv, e) -> 
	  let loc = Npkcontext.get_loc () in
	  let (lv_pref, (lv, t)) = translate_lv lv in
	  let (e_pref, e) = translate_exp e in
	  let e = C.cast e t in
	    (lv_pref@e_pref@((C.Set (lv, t, e), loc)::[]), (Lval (lv, t), t))

      | ExpPlusPlus lv ->
	  let loc = Npkcontext.get_loc () in
	  let (pref_lv, (lv', t)) = translate_lv lv in
	  let (tmp, tmp_name) = create_tmp t loc in
	  let e = C.Lval (lv', t) in
	  let sav_set = (C.Set (tmp, t, e), loc) in
	  let incr_set = Set (lv, Binop (Plus, Var tmp_name, exp_of_int 1)) in
	  let (pref, _) = translate_exp incr_set in
	    pop_local tmp_name;
	    (pref_lv@(sav_set::pref), (C.Lval (tmp, t), t))

  and translate_ftyp (args, va_list, ret) =
    let translate_arg (t, x) =
      let t =
	match t with
	    Array (t, _) -> C.Ptr (translate_typ t)
	  | Void -> 
	      Npkcontext.error "Firstpass.translate_atyp"
		"Argument type void not allowed"
	  | _ -> translate_typ t
      in
	(t, x)
    in
    let args =
      match args with
	  (Void, _)::[] -> []
	| _ -> List.map translate_arg args
    in
    let ret = translate_typ ret in
      (args, va_list, ret)
	
  and translate_proto_ftyp f (args, va_list, ret) = 
    if args = [] then begin
      Npkcontext.print_warning "Firstpass.translate_proto_ftyp" 
	("Incomplete prototype for function "^f);
    end;
    translate_ftyp (args, va_list, ret)

  and translate_typ t =
    match t with
	Void -> C.Void
      | Int k -> C.Int k
      | Float n -> C.Float n
      | Ptr t -> C.Ptr (translate_typ t)
      | Array (t, len) -> 
	  let t = translate_typ t in
	  let len = 
	    match len with
		None -> None
	      | Some e -> 
		  let (pref, (e, _)) = translate_exp e in
		    if (pref <> []) 
		    then begin 
		      Npkcontext.error "Firstpass.translate_typ" 
			("expression without side-effects expected "
			  ^"for array size")
		    end;
		    let i = C.len_of_exp e in
		      Some i
	  in
	    C.Array (t, len)
      | Struct n -> C.Struct n
      | Union n -> C.Union n
      | Fun ft -> C.Fun (translate_ftyp ft)
      | Bitfield _ -> 
	  Npkcontext.error "Firstpass.translate_typ" 
	    "bitfields not allowed outside of structures"

  and translate_arg e (t, _) = 
    let (pref, e) = translate_exp e in
      (pref, C.cast e t)

  (*
    TODO: a final optimization to remove unused variables....
  *)
  and translate_call f args =
    let loc = Npkcontext.get_loc () in
    let (pref_fn, fn, ft) = translate_fn f in
    let (args_t, _, ret_t) = ft in
    let (pref, args) = 
      try List.split (List.map2 translate_arg args args_t) 
      with Invalid_argument "List.map2" ->
	Npkcontext.error "Compiler.translate_call" 
	  ("Different types at function call")
    in
    let pref = List.concat pref in
    let (lv, e) =
      match ret_t with
	  (* Any expression is ok, since it is going to be thrown away *)
	  C.Void -> (None, C.exp_of_int 1)
	| _ -> 
	    let (tmp, tmp_name) = create_tmp ret_t loc in
	      pop_local tmp_name;
	      (Some tmp, C.Lval (tmp, ret_t))
    in
    let call = (C.Call (lv, (fn, ft), args), loc) in
      (pref_fn@pref@(call::[]), (e, ret_t))

  and translate_fn f =
    let (pref, (f, t)) = translate_lv f in
    let (fn, t) = 
      match t with
	  C.Ptr ft -> (C.Deref (C.Lval (f, t), ft), ft)
	| _ -> (f, t)
    in
    let t = C.ftyp_of_typ t in
      (pref, fn, t)

  and translate_exp_option e =
    match e with
	None -> ([], None)
      | Some e -> 
	  let (pref, e) = translate_exp e in
	    (pref, Some e)

  and translate_init t x =
    let pref = ref [] in
    let res = ref [] in
    let rec translate o t x =
      match (x, t) with
	  (Data (Str str), C.Array _) -> 
	    let seq = seq_of_string str in
	      translate o t (Sequence seq)

	| (Data e, _) -> 
	    let (pref_e, e) = translate_exp e in 
	    let e = cast e t in
	      res := (o, t, e)::!res;
	      pref := !pref@pref_e;
	      t

	| (Sequence seq, C.Array (t, sz)) -> 
	    let n = 
	      match sz with
		  Some n -> n
		| None -> List.length seq
	    in
	      translate_sequence o t n seq;
	      C.Array (t, Some n)

	| (Sequence seq, C.Struct n) -> 
	    let (f, _, _) = Hashtbl.find compdefs n in
	      List.iter2 (translate_field_sequence o) f seq;
	      C.Struct n

	| _ -> 
	    Npkcontext.error "Firstpass.translate_init"
	      "This type of initialization not implemented yet"

    and translate_field_sequence o (f, (f_o, t)) x =
      let o = o + f_o in
      let _ = translate o t x in
	()

    and translate_sequence o t n seq =
      match seq with
	  hd::tl when n > 0 -> 
	    let _ = translate o t hd in
	    let o = o + C.size_of compdefs t in
	      translate_sequence o t (n-1) tl
	| _::_ -> 
	    Npkcontext.print_warning 
	      "Firstpass.translate_init.translate_sequence" 
	      "Too many initializers for array"

	(* TODO: code cleanup: We fill with zeros, because CIL does too. 
	   But it shouldn't be done like that:
	   the region should be init to 0 by default and then filled with
	   values ?? *)
	| [] when n > 0 -> 
	    let _ = fill_with_zeros o t in
	    let o = o + C.size_of compdefs t in
	      translate_sequence o t (n-1) []
	| [] -> ()

    and fill_with_zeros o t =
      match t with
	  C.Int _ -> res := (o, t, C.exp_of_int 0)::!res
	| C.Float _ -> res := (o, t, C.exp_of_float 0.)::!res
	| C.Array (t, Some n) -> 
	    let sz = C.size_of compdefs t in
	    let o = ref o in
	      for i = 0 to n - 1 do
		fill_with_zeros !o t;
		o := !o + sz
	      done

	| _ -> 
	    Npkcontext.error "Firstpass.translate_init.fill_with_zeros"
	      "This type of initialization not implemented yet"
    in
    match x with
	None -> ([], t, None)
      | Some init -> 
	  let t = translate 0 t init in
	    (!pref, t, Some (List.rev !res))

  and add_glb_cstr str =
    let name = "!"^fname^".const_str_"^str in
    let a = (char_typ, Some ((String.length str) + 1)) in
    let t = C.Array a in
      if not (Hashtbl.mem glbdecls name) then begin
	let loc = (fname, -1, -1) in
	let (_, t, init) = translate_init t (Some (Data (Str str))) in
	  add_global name (t, loc, Some init)
      end;
      (C.AddrOf (C.Index (C.Global name, a, C.exp_of_int 0), t), 
      C.Ptr char_typ)

  and translate_blk x =
    match x with
      | (EDecl (x, v), loc)::body -> 
	  push_enum (x, v) loc;
	  let body = translate_blk body in
	    pop_enum x;
	    body
      | (VDecl (x, t, static, init), loc)::tl when static -> 
	  let t = translate_typ t in
	  let (pref, t, init) = translate_init t init in
	    if (pref <> []) then begin 
	      Npkcontext.error "Firstpass.translate_init"
		"Expression without side-effects expected"
	    end;
	    add_static x (t, loc, Some init);
	    let tl = translate_blk tl in
	      remove_static x;
	      tl

      | (VDecl (x, t, _, init), loc)::tl -> 
	  Npkcontext.set_loc loc;
	  let t = translate_typ t in
	  let (pref, t, init) = translate_init t init in
	  let n = push_local loc (t, x) in
	  let tl = translate_blk tl in
	    pop_local x;
	    begin match init with
		None -> pref@tl
	      | Some init -> pref@((C.Init (n, init), loc)::tl)
	  end

      | hd::tl -> 
	  let hd = translate_stmt hd in
	  let tl = translate_blk tl in
	    hd@tl

      | [] -> []

  and translate_stmt (x, loc) =
    Npkcontext.set_loc loc;
    match x with
	If (And (e1, e2), blk1, blk2) ->
	  let if_e1_blk = (If (e2, blk1, blk2), loc)::[] in
	    translate_stmt (If (e1, if_e1_blk, blk2), loc)

      | If (Or (e1, e2), blk1, blk2) ->
	  let if_not_e1_blk = (If (e2, blk1, blk2), loc)::[] in
	    translate_stmt (If (e1, blk1, if_not_e1_blk), loc)

      | If (Unop (Not, (And _ as e)), blk1, blk2) ->
	  translate_stmt (If (e, blk2, blk1), loc)

      | If (e, blk1, blk2) ->
	  let e = simplify_bexp e in
	  let (pref, (e, _)) = translate_exp e in
	  let blk1 = translate_blk blk1 in
	  let blk2 = translate_blk blk2 in
	    pref@((C.If (e, blk1, blk2), loc)::[])

      | While (e, body) ->
	  let loop_exit = translate_stmt (If (e, [], (Break, loc)::[]), loc) in
	  let body = translate_blk body in
	    (C.Loop (loop_exit@body, []), loc)::[]

      | DoWhile (body, e) -> 
	  let body = translate_blk body in
	  let loop_exit = translate_stmt (If (e, [], (Break, loc)::[]), loc) in
	    (C.Loop (body@loop_exit, []), loc)::[]

      | For (init, e, body, step) ->
	  let init = translate_blk init in
	  let loop_exit = translate_stmt (If (e, [], (Break, loc)::[]), loc) in
	  let body = translate_blk body in
	  let step = translate_blk step in
	    init@(C.Loop (loop_exit@body, step), loc)::[]

      | Return None -> (C.Return, loc)::[]
	      
      | Return (Some e) ->
	  let set = (Exp (Set (Var ret_name, e)), loc) in
	  let return = (Return None, loc) in
	    translate_blk (set::return::[])

      | Exp e ->
	  let (pref, _) = translate_exp e in
	    pref

      | Switch (e, cases) -> 
	  let (pref, (e, _)) = translate_exp e in
	  let pref = ref pref in
	  let translate_case (e, body, loc) = 
	    let (pref_case, e) = translate_exp_option e in
	      pref := pref_case@(!pref);
	      (e, translate_blk body, loc) 
	  in
	  let cases = List.map translate_case cases in
	    !pref@(C.Switch (e, cases), loc)::[]

      | Break -> (C.Break, loc)::[]

      | Continue -> (C.Continue, loc)::[]

      | Block body -> translate_blk body

      | VDecl _ | EDecl _ -> 
	  Npkcontext.error "Firstpass.translate.translate_stmt" 
	    "Unreachable statement"
  in
    
  let update_funtyp f t loc =
    try
      let (prev_t, _, _) = Hashtbl.find fundefs f in
	if not (ftyp_equal t prev_t) then begin
	  Npkcontext.error "Firstpass.update_fundef"
	    ("Different types for function "^f)
	end
    with Not_found -> 
      Hashtbl.add symbtbl f (Glb f, C.Fun t);
      Hashtbl.add fundefs f (t, loc, None)
  in

  let update_funbody f body =
    try
      let (t, loc, prev_body) = Hashtbl.find fundefs f in
	match prev_body with
	    None -> Hashtbl.replace fundefs f (t, loc, Some body)
	  | Some _ -> 
	      Npkcontext.error "Firstpass.update_fundef"
		("Multiple definitions of function "^f^" body")
    with Not_found ->
      Npkcontext.error "Firstpass.update_funbody" "Unreachable statement"
  in

  let translate_global (x, loc) =
    Npkcontext.set_loc loc;
    match x with
	FunctionDef (x, t, body) ->
	  static_pref := "!"^fname^"."^x^".";
	  let t = translate_typ t in
	  let ft = C.ftyp_of_typ t in
	    update_funtyp x ft loc;
	    let _ = push_formals loc ft in
	    let body = translate_blk body in
	    let locals = get_locals () in
	      update_funbody x (locals, body)

      | GlbEDecl d -> push_enum d loc

(* TODO: put this check in parser ?? *)
      | GlbVDecl ((_, _, _, Some _), is_extern) when is_extern -> 
	  Npkcontext.error "Firstpass.translate_global"
	    "Extern globals can not be initizalized"
 
      | GlbVDecl ((x, t, static, init), is_extern) ->
	  static_pref := "!"^fname^".";
	  begin match (t, init) with
	      (Fun ft, None) -> 
		let ft = translate_proto_ftyp x ft in
		  update_funtyp x ft loc

	    | (Fun ft, Some _) -> 
		Npkcontext.error "Firstpass.translate_global"
		  ("Unexpected initialization of function "^x)
	    | _ -> 
		let t = translate_typ t in
		let (pref, t, init) = translate_init t init in
		  if (pref <> []) then begin 
		    Npkcontext.error "Firstpass.translate_init"
		      "Expression without side-effects expected"
		  end;
		  let init = if is_extern then None else Some init in
		    if static then add_static x (t, loc, init)
		    else add_global x (t, loc, init)
	  end
  in

  let translate_struct_fields f =
    let o = ref 0 in
    let last_align = ref 1 in
    let rec translate (t, x) =
      match t with
	  Bitfield ((_, n), sz) when sz > n ->
	    Npkcontext.error "Firstpass.translate_struct_fields"
	      "width of bitfield exceeds its type"
	| Bitfield ((s, n), sz) ->
	    let t = translate_typ (Int (s, n)) in
	    let cur_align = C.align_of compdefs t in
	    let o' = C.next_aligned !o cur_align in
	    let o' = if !o + sz <= o' then !o else o' in
	    let t = translate_typ (Int (s, sz)) in
	      last_align := max !last_align cur_align;
	      o := !o + sz;
	      (x, (o', t))
(* TODO
	  (Bitfield ((s, n), sz), x)::f -> 
	    let t = translate_typ (Int (s, sz)) in
	    let next_align = C.align o n in
	    let o = if o + sz <= next_align then o else next_align in
	    let (f, n) = translate (o+sz) f in
	      ((x, (o, t))::f, n)
*) (*????*)
	| _ ->
	    let t = translate_typ t in
	    let sz = C.size_of compdefs t in
	    let cur_align = C.align_of compdefs t in
	    let o' = C.next_aligned !o cur_align in
	      last_align := max !last_align cur_align;
	      o := o'+sz;
	      (x, (o', t))
    in
    let f = List.map translate f in
      (f, C.next_aligned !o !last_align, !last_align)
      
  in

  let translate_union_fields f =
    let n = ref 0 in
    let align = ref 0 in
    let translate (t, x) =
      let t = translate_typ t in
      let sz = C.size_of compdefs t in
      let align' = C.align_of compdefs t in
	align := max !align align';
	if !n < sz then n := sz;
	(x, (0, t))
    in
    let f = List.map translate f in
      (f, !n, !align)
  in

  let translate_compdef (n, is_struct, fields) =
    let fields =
      if is_struct then translate_struct_fields fields
      else translate_union_fields fields
    in
      Hashtbl.add compdefs n fields
  in
    
    List.iter translate_compdef bare_compdefs;
    List.iter translate_global globals;
    (compdefs, glbdecls, fundefs)
