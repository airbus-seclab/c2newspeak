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

(* TODO: should rename firstpass to semantic ??? see compiler Appel book *)
open Csyntax
module C = Cir
module K = Npkil
module N = Newspeak
module Nat = Newspeak.Nat

(* Constants *)
let ret_name = "!return"

let ret_lbl = 0
let cnt_lbl = 1
let brk_lbl = 2
let default_lbl = 3

(* types *)
type symb =
    | VarSymb of C.lv 
    | Enum of C.exp


type btree =
    TLeft
  | TRight
  | TExp of (exp * btree * btree)

let build_tree e =
  let rec build_tree e t1 t2 =
    match e with
	IfExp (c, e1, e2) ->
	  let t1' = build_tree e1 t1 t2 in
	  let t2' = build_tree e2 t1 t2 in
	    build_tree c t1' t2'
	      
      | Unop (Not, (IfExp _ as e)) -> build_tree e t2 t1
	  
      | Cst (C.CInt c, _) when Nat.compare c Nat.zero <> 0 -> t1
	  
      | Cst (C.CInt _, _) -> t2
	  
      | _ -> 
	  let (t1, l1, r1) = t1 in
	  let (t2, l2, r2) = t2 in
	    (TExp (e, t1, t2), l1 + l2, r1 + r2)
  in
  let (t, l, r) = build_tree e (TLeft, 1, 0) (TRight, 0, 1) in
    (t, l > 1, r > 1)

(* functions *)
(* [align o x] returns the smallest integer greater or equal than o,
   which is equal to 0 modulo x *)
let next_aligned o x =
  let m = o mod x in
    if m = 0 then o else o + x - m

let rec simplify_bexp e =
  match e with
      Var _ | Field _ | Index _ | Deref _ | Call _ | OpExp _ 
    | Set _ | SetOp _ -> 
	Unop (Not, Binop (Eq, e, exp_of_int 0))
    | Unop (Not, e) -> Unop (Not, simplify_bexp e)
    | _ -> e

(* TODO: code cleanup: find a way to factor this with create_cstr
   in Npkil *)
let seq_of_string str =
  let len = String.length str in
  let res = ref [(None, Data (exp_of_int 0))] in
    for i = len - 1 downto 0 do
      let c = Char.code str.[i] in
	res := (None, Data (exp_of_int c))::!res
    done;
    !res

(*
   Sets scope of variables so that no goto escapes a variable declaration
   block
*)
let translate (globals, spec) =
  let compdefs = Hashtbl.create 100 in
  let glbdecls = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in
    
  let symbtbl = Hashtbl.create 100 in
  (* Used to generate static variables names *)
  let current_fun = ref "" in
  (* Counter of static variables, necessary to distinguish 2 statics in 
     different scope of the same function, who would have the same name
  *)
  let static_cnt = ref 0 in

  let tmp_cnt = ref 0 in

  let lbl_tbl = Hashtbl.create 10 in
  let lbl_cnt = ref default_lbl in

  let new_lbl () =
    incr lbl_cnt;
    !lbl_cnt
  in

  let add_var loc (t, x) =
    let id = C.fresh_id () in
      Hashtbl.add symbtbl x (VarSymb (C.Var id), t, loc);
      id
  in

  let remove_symb x = Hashtbl.remove symbtbl x in

  let add_formals loc (args_t, ret_t) =
    let ret_id = add_var loc (ret_t, ret_name) in
    let args_id = List.map (add_var loc) args_t in
      (ret_id, args_id)
  in
    
  let remove_formals args_name =
    remove_symb ret_name;
    List.iter remove_symb args_name
  in

  let find_symb x = 
    try Hashtbl.find symbtbl x
    with Not_found -> 
      Npkcontext.error "Firstpass.translate.typ_of_var" 
	("Unknown identifier "^x)
  in

  let update_global x name loc (ct, init, t) =
    let v = VarSymb (C.Global name) in
      try 
	let (prev_t, _, prev_init) = Hashtbl.find glbdecls name in
	  if not (Cir.is_subtyp t prev_t) then begin
	    Npkcontext.error "Firstpass.update_global" 
	      ("global variable "^x^" declared with different types")
	  end;
	  let init = 
	    match (prev_init, init) with
		(None, Some _) | (Some None, Some _) -> init
	      | (Some _, None) | (Some _, Some None) -> prev_init
	      | (None, None) -> None
	      | (Some Some _, Some Some _) -> 
		  Npkcontext.error "Firstpass.update_global"
		    ("global variable "^x^" initialized twice")
	  in
	    Hashtbl.replace symbtbl x (v, ct, loc);
	    Hashtbl.replace glbdecls name (t, loc, init)
      with Not_found -> 
	Hashtbl.add symbtbl x (v, ct, loc);
	Hashtbl.add glbdecls name (t, loc, init)
  in

  let add_global x loc d = update_global x x loc d in

  let add_static x loc d =
    let (fname, _, _) = loc in
    let prefix = "!"^fname^"." in
    let prefix =
      if !current_fun = "" then prefix
      else prefix^(!current_fun)^"."
    in
    let name = prefix^(string_of_int !static_cnt)^"."^x in
      incr static_cnt;
      update_global x name loc d
  in

  let push_enum (x, i) loc = Hashtbl.add symbtbl x (Enum i, int_typ, loc) in

  let update_funtyp f static ct t loc =
    let (fname, _, _) = loc in
    let f' = if static then "!"^fname^"."^f else f in
      if Hashtbl.mem symbtbl f then begin
	try
	  let (prev_t, _, _) = Hashtbl.find fundefs f' in
	    if (t <> prev_t) then begin
	      Npkcontext.error "Firstpass.update_fundef"
		("different types for function "^f)
	    end
	with Not_found ->
	  Npkcontext.error "Firstpass.update_fundef"
	    ("previous definition of "^f^" does not match")
      end else begin
	Hashtbl.add symbtbl f (VarSymb (C.Global f'), Fun ct, loc);
	Hashtbl.add fundefs f' (t, loc, None)
      end;
      f'
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

  let translate_lbl lbl =
    try Hashtbl.find lbl_tbl lbl
    with Not_found -> 
      let lbl' = new_lbl () in
	Hashtbl.add lbl_tbl lbl lbl';
	lbl'
  in

  let rec cast (e, t) t' = 
    (* TODO: code cleanup and factor??? 
       let e = translate_exp_wo_array e in
    *)
    let t = translate_typ t in
    let t' = translate_typ t' in
      C.cast (e, t) t'

  and translate_init t x =
    let res = ref [] in
    let rec translate o t x =
      match (x, t) with
	  ((Data (Str str) | Sequence ([(None, Data (Str str))])), 
	  Array (Int (_, n), _)) when n = Config.size_of_char ->
	    let seq = seq_of_string str in
	      translate o t (Sequence seq)

	| (Data e, _) -> 
	    let e = cast (translate_exp_wo_array e) t in
	      res := (o, translate_scalar_typ t, e)::!res;
	      t
	      
	| (Sequence seq, Array (t, len)) -> 
	    let n = 
	      match translate_array_len len with
		  Some n -> n
		| None -> List.length seq
	    in
	      translate_sequence o t n seq;
	      Array (t, Some (exp_of_int n))
	
	| (Sequence seq, Struct (n, f)) ->
	    let (f, _) = process_struct (n, f) in
	      translate_field_sequence o f seq;
	      Struct (n, None)

	| (Sequence _, _) -> 
	    Npkcontext.error "Firstpass.translate_init"
	      "this type of initialization not implemented yet"

    and translate_field_sequence o fields seq =
      match (fields, seq) with
	  ((fname, (f_o, t))::fields, (expected_f, hd)::seq) ->
	    let f_o = o + f_o in
	    let _ = 
	      match expected_f with
		  Some f when fname <> f ->
		    Npkcontext.error "Firstpass.translate_field_sequence" 
		      ("initialization of field "^fname^" expected")
		| _ -> ()
	    in
	    let _ = translate f_o t hd in
	      translate_field_sequence o fields seq

	| ([], []) -> ()

	| ((_, (f_o, t))::fields, []) ->
	    let f_o = o + f_o in
	    let _ = fill_with_zeros f_o t in
	      if (fields = []) then begin
		Npkcontext.report_dirty_warning 
		  "Firstpass.translate_init.translate_field_sequence" 
		  "not enough initializers for structure"
	      end;
	      translate_field_sequence o fields []

	| ([], _) -> 
	    Npkcontext.report_dirty_warning 
	      "Firstpass.translate_init.translate_field_sequence" 
	      "too many initializers for structure"
	  
    and translate_sequence o t n seq =
      match seq with
	  (None, hd)::tl when n > 0 -> 
	    let _ = translate o t hd in
	    let o = o + size_of t in
	      translate_sequence o t (n-1) tl
	| (Some _, _)::_ ->
	    Npkcontext.error "Firstpass.translate_init.translate_sequence" 
	      "anonymous initializer expected for array"
	    
	| _::_ -> 
	    Npkcontext.report_dirty_warning 
	      "Firstpass.translate_init.translate_sequence" 
	      "too many initializers for array"
	      
	(* TODO: code cleanup: We fill with zeros, because CIL does too. 
	   But it shouldn't be done like that:
	   the region should be init to 0 by default and then filled with
	   values ?? *)
	| [] when n > 0 -> 
	    let _ = fill_with_zeros o t in
	    let o = o + size_of t in
	    let n = n - 1 in
	      if (n = 0) then begin
		Npkcontext.print_warning 
		  "Firstpass.translate_init.translate_sequence" 
		  "not enough initializers for array"
	      end;
	      translate_sequence o t n []
	| [] -> ()
	    
    and fill_with_zeros o t =
      match t with
	  Int _ -> res := (o, translate_scalar_typ t, C.exp_of_int 0)::!res
	| Ptr _ -> 
	    (* TODO: inefficient: t is translated twice *)
	    let e = cast (translate_exp (exp_of_int 0)) t in
	      res := (o, translate_scalar_typ t, e)::!res
	| Float _ -> 
	    res := (o, translate_scalar_typ t, C.exp_of_float 0.)::!res
	| Array (t, n) ->
	    let n = 
	      match translate_array_len n with
		  Some n -> n
		| None -> 
		    Npkcontext.error "Firstpass.translate_init.fill_with_zeros"
		      "unreachable statement"
	    in
	    let sz = size_of t in
	    let o = ref o in
	      for i = 0 to n - 1 do
		fill_with_zeros !o t;
		o := !o + sz
	      done
		
	| Struct s -> 
	    let (f, _) = process_struct s in
	    let fill_field (_, (f_o, t)) = fill_with_zeros (o + f_o) t in
	      List.iter fill_field f

	| _ -> 
	    Npkcontext.error "Firstpass.translate_init.fill_with_zeros"
	      "this type of zero initialization not implemented yet"
    in
    let t = translate 0 t x in
      (List.rev !res, t)
  
  and translate_glb_init t x =
    match x with
	None -> (t, None)
      | Some init -> 
	  let (init, t) = translate_init t init in
	    (t, Some init)

  and add_glb_cstr str =
    let fname = Npkcontext.get_fname () in
    let name = "!"^fname^".const_str_"^str in
    let t = Array (char_typ, Some (exp_of_int ((String.length str) + 1))) in
    let loc = Npkcontext.get_loc () in
    let (t, init) = translate_glb_init t (Some (Data (Str str))) in
(* TODO: is it necessary to have translate_typ t ??? *)
      if not (Hashtbl.mem glbdecls name) 
      then add_global name loc (t, Some init, translate_typ t);
      (C.Global name, t)
  
  and translate_lv x =
    match x with
	Var x ->
	  let (v, t, _) = find_symb x in
	  let lv = 
	    match v with
		VarSymb lv -> lv
	      | Enum _ -> 
		  Npkcontext.error "Firstpass.translate_lv"
		    "Invalid left value: unexpected enum"
	  in
	    (lv, t)

      | Field (lv, f) -> 
	  let (lv, t) = translate_lv lv in
	  let n = comp_of_typ t in
	  let (r, _, _) = Hashtbl.find compdefs n in
	  let (o, t) = List.assoc f r in
	  let o = C.exp_of_int o in
	    (C.Shift (lv, o), t)

      | Index (e, idx) -> 
	  let (e', t) = translate_exp e in begin
	    match (e', t) with
		(C.Lval (lv, _), Array (t, len)) ->
		  let (i, _) = translate_exp idx in
		  let n = translate_array_len len in
		  let len = C.length_of_array n lv in
		  let sz = C.exp_of_int (size_of t) in
		  let o = C.Unop (K.Belongs_tmp (Nat.zero, len), i) in
		  let o = C.Binop (N.MultI, o, sz) in
		    (C.Shift (lv, o), t)

	      | (_, Ptr _) -> translate_lv (Deref (Binop (Plus, e, idx)))
	      | _ -> 
		  Npkcontext.error "Firstpass.translate_lv" 
		    "Array or pointer type expected"
	  end

      | Deref e -> deref e

      | OpExp (op, lv, is_after) ->
	  let loc = Npkcontext.get_loc () in
	  let e = Binop (op, lv, Cst (C.CInt (Nat.of_int 1), int_typ)) in
	  let (incr, t) = translate_set (lv, e) in
	  let (lv, _, _) = incr in
	    (C.Stmt_lv ((C.Set incr, loc), lv, is_after), t)

      | Str str -> add_glb_cstr str

      | _ -> Npkcontext.error "Firstpass.translate_lv" "left value expected"


  and translate_exp_wo_array e =
    let (e, t) = translate_exp e in
      match (e, t) with
	| (C.Lval (lv, _), Array (t', _)) -> 
	    let (e, _) = addr_of (lv, t) in
	      (e, Ptr t')
	| _ -> (e, t)

  and deref e =
    let (e, t) = translate_exp_wo_array e in
      match t with
	  Ptr t -> (C.Deref (e, translate_typ t), t)
	| _ -> Npkcontext.error "Firstpass.deref_typ" "pointer type expected"
	    
  and addr_of (e, t) = (C.AddrOf (e, translate_typ t), Ptr t)

  and translate_exp e = 
    match e with
      | Cst (c, t) -> (C.Const c, t)

      | Var x -> 
	  let (v, t, _) = find_symb x in
	  let e = 
	    match v with
		Enum i -> i
	      | VarSymb lv -> C.Lval (lv, translate_typ t)
	  in
	    (e, t)
	      
      | Field _ | Index _ | Deref _ | OpExp _ | Str _ -> 
	  let (lv, t) = translate_lv e in
	    (C.Lval (lv, translate_typ t), t)
	    
      | AddrOf (Deref e) when !Npkcontext.dirty_syntax ->
	  Npkcontext.print_warning "Firstpass.translate_exp" 
	    ("unnecessary creation of a pointer from a dereference:"
	     ^" rewrite the code");
	    addr_of (deref e)

      | AddrOf (Deref _) -> 
	  Npkcontext.error "Firstpass.translate_exp" 
	    ("unnecessary creation of a pointer from a dereference:"
	      ^" rewrite the code")

      | AddrOf (Index (lv, Cst (C.CInt i, _)))
	  when Nat.compare i Nat.zero = 0 ->
	  let (lv', t) = translate_lv lv in begin
	    match t with
		Array (elt_t, _) -> 
		  let (e, _) = addr_of (lv', t) in
		    (e, Ptr elt_t)
	      | Ptr _ -> translate_exp (AddrOf (Deref lv))
	      | _ -> 
		  Npkcontext.error "Firstpass.translate_lv" 
		    "Array type expected"
	  end

      | AddrOf (Index (lv, e)) -> 
	  let base = AddrOf (Index (lv, exp_of_int 0)) in
	    translate_exp (Binop (Plus, base, e))
	    
      | AddrOf lv -> addr_of (translate_lv lv)
	      
(* Here c is necessarily positive *)
      | Unop (Neg, Cst (C.CInt c, Int (_, sz))) -> 
	  (C.Const (C.CInt (Nat.neg c)), Int (N.Signed, sz))

      | Unop (op, e) -> 
	  let e = translate_exp_wo_array e in
	    translate_unop op e

      | Binop (op, e1, e2) -> 
	  let e1 = translate_exp_wo_array e1 in
	  let e2 = translate_exp_wo_array e2 in
	    translate_binop op e1 e2

      | IfExp (c, e1, e2) ->
	  let loc = Npkcontext.get_loc () in
	    (* TODO: this is a bit inefficient, e1 gets translated twice!! *)
	  let (_, t) = translate_exp_wo_array e1 in
	  let (x, decl, v) = gen_tmp loc t in
	  let blk1 = (Exp (Set (Var x, e1)), loc)::[] in
	  let blk2 = (Exp (Set (Var x, e2)), loc)::[] in
	  let set = (If (c, blk1, blk2), loc) in
	  let set = translate_stmt set in
	    remove_symb x;
	    (C.Pref (decl::set, C.Lval (v, translate_typ t)), t)

      | SizeofE e ->
	  let (_, t) = translate_exp e in
	  let sz = (size_of t) / 8 in
	    (C.exp_of_int sz, uint_typ)

      | Sizeof t -> 
	  let sz = (size_of t) / 8 in
	    (C.exp_of_int sz, uint_typ)

      | Cast (e, t) -> 
(* TODO: is this ok?? *)
	  let e = translate_exp_wo_array e in
	  let e = cast e t in
	    (e, t)

      | Call (f, args) -> 
	  let (lv, t) = translate_lv f in
	    (* TODO: code not nice: improve!!! *)
	    (* TODO: not nice, think about it *)
	  let (f, ft, ft') = 
	    match (lv, t) with
		(_, Ptr (Fun ft)) -> 
		  let t = translate_typ t in
		    (* TODO: code simplification, not good that translate_ftyp
		       does not give just a ftyp *)
	    (* TODO: this normalization shouldn't be necessary, it should
	       be redundant. problem...*)
		  let (ft, _) = normalize_ftyp ft in
		  let ft' = translate_ftyp ft in
		    (C.FunDeref (C.Lval (lv, t), ft'), ft, ft')
	      | (C.Global f, Fun ft) -> 
	    (* TODO: this normalization shouldn't be necessary, it should
	       be redundant. problem...*)
		  let (ft, _) = normalize_ftyp ft in
		  let ft' = translate_ftyp ft in
		    (C.Fname f, ft, ft')
	      | (C.Deref (e, _), Fun ft) -> 
	    (* TODO: this normalization shouldn't be necessary, it should
	       be redundant. problem...*)
		  let (ft, _) = normalize_ftyp ft in
		  let ft' = translate_ftyp ft in
		    (C.FunDeref (e, ft'), ft, ft')
	      | _ -> 
		  Npkcontext.error "Firstpass.translate_exp" 
		    "Function type expected"
	  in
	  let (args_t, ret_t) = ft in
	  let args = translate_args args args_t in
	    (C.Call (ft', f, args), ret_t)

(* TODO: should find a way to put this and SetOp together!! *)
      | Set set when !Npkcontext.dirty_syntax ->
	  let loc = Npkcontext.get_loc () in
	  let (set, t) = translate_set set in
	  let (lv, t', _) = set in
	  let e = C.Lval (lv, t') in
	    Npkcontext.print_warning "Firstpass.translate_exp" 
	      "avoid assignments within expressions";
	    (C.Pref ((C.Set set, loc)::[], e), t)

      | SetOp (lv, op, e) when !Npkcontext.dirty_syntax ->
(* TODO: is this check really necessary!!! *)
	  let (lv', _) = translate_lv lv in
	  let (pref, _, post) = C.normalize_lv lv' in
	    (* TODO: should factor this code *)
	    if (pref <> []) || (post <> []) then begin
	      Npkcontext.error "Firstpass.translate_stmt" 
		"expression without side-effects expected"
	    end;
	    let e = Binop (op, lv, e) in
	      translate_exp (Set (lv, e))
 
      | Set _ | SetOp _ -> 
	  Npkcontext.error "Firstpass.translate_exp" 
	    "avoid assignments within expressions"

  and translate_set (lv, e) =
    let (lv, t) = translate_lv lv in
    let e = cast (translate_exp_wo_array e) t in
      ((lv, translate_typ t, e), t)

  and init_va_args loc lv x =
    let rec init_va_args lv x =
      match x with
	  (e, t)::tl ->
	    let sz = size_of t in
	    let t = translate_typ t in
	    let set = (C.Set (lv, t, e), loc) in
	    let lv = C.Shift (lv, C.exp_of_int sz) in
	    let init = init_va_args lv tl in
	      set::init
	| [] -> []
    in
      init_va_args lv x

  and translate_va_args x =
    match x with
	e::tl -> 
	  let (args, sz) = translate_va_args tl in
	  let (e, t) = translate_exp_wo_array e in
	    ((e, t)::args, size_of t + sz)
      | [] -> ([], 0)

  and translate_args args args_t =
    let rec translate_args args args_t =
      match (args, args_t) with
	  ([], (Va_arg, _)::[]) ->
	    let e = cast (translate_exp (exp_of_int 0)) (Ptr char_typ) in
	      e::[]
	| (_, (Va_arg, _)::[]) -> 
	    let (args, sz) = translate_va_args args in
	    let loc = Npkcontext.get_loc () in
	    let sz = if sz mod 8 = 0 then sz/8 else (sz/8)+1 in
	    let t = Array (char_typ, Some (exp_of_int sz)) in
	    let (_, decl, v) = gen_tmp loc t in
	    let (e, _) = addr_of (v, t) in
	    let init = init_va_args loc v args in
	      (C.Pref (decl::init, e))::[]

	| (e::args, (t, _)::args_t) ->
	    let e = cast (translate_exp_wo_array e) t in
	      e::(translate_args args args_t)
	| ([], []) -> []
	| _ -> 
	    Npkcontext.error "Firstpass.translate_exp" 
	      "different types at function call"
    in
      translate_args args args_t

  and gen_tmp loc t =
    let x = "tmp"^(string_of_int !tmp_cnt) in
    let id = add_var loc (t, x) in
    let t = translate_typ t in
    let decl = (C.Decl (t, x, id), loc) in
      incr tmp_cnt;
      (x, decl, C.Var id)

  and translate_field (x, (o, t)) = (x, (o, translate_typ t))

  and translate_scalar_typ t =
    match t with
      | Int k -> N.Int k
      | Float n -> N.Float n	
      | Ptr (Fun _) -> N.FunPtr
      | Ptr _ -> N.Ptr
      | Va_arg -> N.Ptr
      | _ -> 
	  Npkcontext.error "Firstpass.translate_scalar_typ" 
	    "scalar type expected"

  and translate_typ t =
    match t with
	Void -> C.Void
      | Int _ | Float _ | Ptr (Fun _) | Ptr _ | Va_arg -> 
	  C.Scalar (translate_scalar_typ t)
      | Fun ft -> 
	  (* TODO: merge normalize_ftyp and translate_ftyp together!!! *)
	  let (ft, _) = normalize_ftyp ft in
	    C.Fun (translate_ftyp ft)
      | Array (t, len) -> 
	  let t = translate_typ t in
	  let len = translate_array_len len in
	    C.Array (t, len)
(* TODO: do the same coding for Union *)
      | Struct (n, f) ->
	  let (f, sz) = process_struct (n, f) in
	  let f = List.map translate_field f in
	    C.Struct (f, sz)
      | Union (n, _) when Hashtbl.mem compdefs n -> 
	  let (f, sz, _) = Hashtbl.find compdefs n in
	  let f = List.map translate_field f in
	    C.Union (f, sz)
      | Union (n, Some f) -> 
	  let (f, sz) = process_union_fields n f in
	  let f = List.map translate_field f in
	    C.Union (f, sz)
      | Union (n, _) -> 
	  Npkcontext.error "Firstpass.translate_typ" ("unknown union "^n)

      | Bitfield _ -> 
	  Npkcontext.error "Firstpass.translate_typ" 
	    "bitfields not allowed outside of structures"

  and translate_array_len x =
    match x with
	None -> None
      | Some e -> 
	  let (e, _) = translate_exp e in
	  let i = 
	    try C.int_of_exp e 
	    with Invalid_argument _ -> 
(* TODO: should print the expression e?? *)
	      Npkcontext.error "Firstpass.translate_typ" 
		"invalid size for array"
	  in
	    if (i < 0) || (i > Config.max_array_length) then begin
(* TODO: should print the expression e?? *)
	      Npkcontext.error "Firstpass.translate_typ" 
		"invalid size for array"
	    end;
	    if (i = 0) then begin
	      Npkcontext.error "Firstpass.translate_typ" 
		"array should have at least 1 element"
	    end;
	    Some i

  and process_struct (name, f) =
    try
      let (f, sz, _) = Hashtbl.find compdefs name in
	(f, sz)
    with Not_found ->
      match f with
	  Some f -> 
	    let (f, sz, a) = process_struct_fields f in
	      Hashtbl.add compdefs name (f, sz, a);
	      (f, sz)
	| None -> 
	    Npkcontext.error "Firstpass.process_struct" 
	      ("unknown structure "^name)

  and process_struct_fields f =
    let o = ref 0 in
    let last_align = ref 1 in
    let rec translate (t, x, loc) =
      Npkcontext.set_loc loc;
      match t with
	  Bitfield ((s, n), sz) ->
	    let (sz, _) = translate_exp sz in
	      (* TODO: factor this code *)
	    let (pref, sz, post) = C.normalize_exp sz in
	    let sz = C.int_of_exp sz in
	      if (pref <> []) || (post <> []) then begin
		Npkcontext.error "Firstpass.push_enum" 
		  "expression without side-effects expected"
	      end;
	      if sz > n then begin
		Npkcontext.error "Firstpass.process_struct_fields"
		  "width of bitfield exceeds its type"
	      end;
	      let cur_align = align_of (Int (s, n)) in
	      let o' = next_aligned !o cur_align in
	      let o' = if !o + sz <= o' then !o else o' in
		last_align := max !last_align cur_align;
		o := !o + sz;
		(x, (o', Int (s, sz)))
	| _ ->
	    let sz = size_of t in
	    let cur_align = align_of t in
	    let o' = next_aligned !o cur_align in
	      last_align := max !last_align cur_align;
	      o := o'+sz;
	      (x, (o', t))
    in
    let f = List.map translate f in
    let sz = next_aligned !o !last_align in
      (f, sz, !last_align)

  and process_union_fields name f =
    let n = ref 0 in
    let align = ref 0 in
    let translate (t, x, loc) =
      Npkcontext.set_loc loc;
      let sz = size_of t in
      let align' = align_of t in
	align := max !align align';
	if !n < sz then n := sz;
	(x, (0, t))
    in
    let f = List.map translate f in
      Hashtbl.add compdefs name (f, !n, !align);
      (f, !n)

  and translate_ftyp (args, ret) =
    let args = List.map (fun (t, _) -> translate_typ t) args in
    let ret = translate_typ ret in
      (args, ret)

  and translate_local_decl (x, t, init) loc =
    Npkcontext.set_loc loc;
    let (init, t) = 
      match init with
	  None -> ([], t)
	| Some init -> translate_init t init
    in
    let id = add_var loc (t, x) in
    let v = C.Var id in
    let build_set (o, t, e) =
      let lv = C.Shift (v, C.exp_of_int o) in
	(C.Set (lv, C.Scalar t, e), loc)
    in
    let init = List.map build_set init in
    let decl = (C.Decl (translate_typ t, x, id), loc) in
      decl::init

  and translate_enum (x, v) loc =
    let v = translate_exp v in
    let v = cast v int_typ in
      (* TODO: factor this code *)
    let (pref, v, post) = C.normalize_exp v in
      if (pref <> []) || (post <> []) then begin
	Npkcontext.error "Firstpass.push_enum" 
	  "expression without side-effects expected"
      end;
      push_enum (x, v) loc

  and translate_blk x = 
    let rec translate x =
    match x with
      | (EDecl (x, v), loc)::body -> 
	  translate_enum (x, v) loc;
	  let body = translate_blk body in
	    remove_symb x;
	    (body, [])
	      
      (* TODO: not good, simplify code *)
      | (VDecl (None, t, _, _), loc)::body -> 
	  Npkcontext.set_loc loc;
	  let _ = translate_typ t in
	    (translate_blk body, [])
	      
      | (VDecl (Some x, t, static, init), loc)::body when static -> 
	  Npkcontext.set_loc loc;
	  let (t, init) = translate_glb_init t init in
	  let t' = translate_typ t in
          (* TODO: code not nice: the signature of this function is not good *)
	    add_static x loc (t, Some init, t');
	    let body = translate_blk body in
	      remove_symb x;
	      (body, [])
	  
      | (VDecl (Some x, t, _, init), loc)::body -> 
	  let init = translate_local_decl (x, t, init) loc in
	  let body = translate_blk body in
	    remove_symb x;
	    (init@body, [])
	      
      (* TODO: do the case where suffix is <> [] *)
      (* TODO: remove body, suffix from For, use goto and labels
	 remove break. Use goto... *)
      | ((Label _, _) as stmt)::(For ([], e, body, []), loc)::tl ->
	  let blk = ((For ([], e, body@(stmt::[]), []), loc)::tl) in
	    translate blk

      | (Label lbl, loc)::tl -> 
	  let lbl = translate_lbl lbl in
	  let (x, tl) = translate tl in
	    ([], (lbl, loc, x)::tl)

      | hd::tl -> 
	  let hd = translate_stmt hd in
	  let (x, tl) = translate tl in
	    (hd@x, tl)
	      
      | [] -> ([], [])      
    in
    let rec stitch (x, tl) =
      match tl with
	  [] -> x
	| (lbl, loc, blk)::tl -> 
	    let blk = (C.Block (x, Some (lbl, [])), loc)::blk in
	      stitch (blk, tl)
    in
      stitch (translate x)

  and translate_stmt (x, loc) = 
    Npkcontext.set_loc loc;
    match x with
      | Exp (Set set) -> 
	  let (set, _) = translate_set set in
	    (C.Set set, loc)::[]

      | Exp (SetOp (lv, op, e)) ->
	  let (lv', _) = translate_lv lv in
	  let (pref, _, post) = C.normalize_lv lv' in
	    (* TODO: should factor this code *)
	    if (pref <> []) || (post <> []) then begin
	      Npkcontext.error "Firstpass.translate_stmt" 
		"expression without side-effects expected"
	    end;
	    let e = Binop (op, lv, e) in
	      translate_stmt (Exp (Set (lv, e)), loc)

      | Exp (Cast (e, Void)) -> 
	  Npkcontext.report_dirty_warning "Firstpass.translate_stmt" 
	    "cast to void should be avoided";
	  translate_stmt (Exp e, loc)

      | Exp e -> 
	  let (e, _) = translate_exp e in
	    (C.Exp e, loc)::[]

      | Break -> (C.Goto brk_lbl, loc)::[]

      | Continue -> (C.Goto cnt_lbl, loc)::[]

      | Return None -> (C.Goto ret_lbl, loc)::[]

      | Return (Some e) ->
	  let set = (Exp (Set (Var ret_name, e)), loc) in
	  let return = (Return None, loc) in
	    translate_blk (set::return::[])

      | Goto lbl -> 
	  let lbl = translate_lbl lbl in
	    (C.Goto lbl, loc)::[]

      | If (e, blk1, blk2) ->
	  let blk1 = translate_blk blk1 in
	  let blk2 = translate_blk blk2 in
	    translate_if loc (e, blk1, blk2)

      | Block body -> (C.Block (translate_blk body, None), loc)::[]

      | For (init, e, body, suffix) ->
	  let init = (C.Block (translate_blk init, Some (cnt_lbl, [])), loc) in
	  let guard = translate_stmt (If (e, [], (Break, loc)::[]), loc) in
	  let body = translate_blk body in
	  let body = (C.Block (guard@body, Some (cnt_lbl, [])), loc) in
	  let suffix = translate_blk suffix in
	  let loop = (C.Loop (body::suffix), loc) in
	    (C.Block (init::loop::[], Some (brk_lbl, [])), loc)::[]

      | CSwitch (e, choices, default) -> 
	  let (e, _) = translate_exp e in
	  let (last_lbl, switch) = translate_switch choices in
	  let default_action = (C.Goto default_lbl, loc)::[] in
	  let switch = (C.Switch (e, switch, default_action), loc)::[] in
	  let body = translate_cases (last_lbl, switch) choices in
	  let default = translate_blk default in
	  let body = (C.Block (body, Some (default_lbl, [])), loc)::default in
	    (C.Block (body, Some (brk_lbl, [])), loc)::[]

      | Label _ | VDecl _ | EDecl _ -> 
	  Npkcontext.error "Firstpass.translate_stmt"
	    "unreachable code"

  and translate_if loc (e, blk1, blk2) =
    let (t, duplicate_blk1, duplicate_blk2) = build_tree e in
    let duplicate_blk1 = duplicate_blk1 && C.is_large_blk blk1 in
    let duplicate_blk2 = duplicate_blk2 && C.is_large_blk blk2 in
    let lbl1 = if duplicate_blk1 then new_lbl () else -1 in
    let lbl2 = if duplicate_blk2 then new_lbl () else -1 in
    let branch1 = if duplicate_blk1 then (C.Goto lbl1, loc)::[] else blk1 in
    let branch2 = if duplicate_blk2 then (C.Goto lbl2, loc)::[] else blk2 in
    let rec translate_tree t =
      match t with
	  TLeft -> branch1
	| TRight -> branch2
	| TExp (e, t1, t2) ->
	    let e = simplify_bexp e in
	    let (e, _) = translate_exp e in
	    let blk1 = translate_tree t1 in
	    let blk2 = translate_tree t2 in
	      (C.If (e, blk1, blk2), loc)::[]
    in
    let x = translate_tree t in
    let x = 
      if duplicate_blk1 then (C.Block (x, Some (lbl1, blk1)), loc)::[] else x 
    in
    let x = 
      if duplicate_blk2 then (C.Block (x, Some (lbl2, blk2)), loc)::[] else x 
    in
      x


  and translate_switch x =
    match x with
	(e, body, loc)::tl ->
	  let (e, t) = translate_exp e in
	  let t = translate_scalar_typ t in
	  let (lbl, tl) = translate_switch tl in
	  let lbl = if body = [] then lbl else lbl+1 in
	    (lbl, ((e, t), (C.Goto lbl, loc)::[])::tl)
      | [] -> (default_lbl, [])

  and translate_cases (lbl, body) x =
    match x with
	(_, [], _)::tl -> translate_cases (lbl, body) tl
      | (_, case, loc)::tl ->
	  let case = translate_blk case in
	  let body = (C.Block (body, Some (lbl, [])), loc)::case in
	    translate_cases (lbl-1, body) tl
      | [] -> body

  and normalize_binop op (e1, t1) (e2, t2) =
    match (op, t1, t2) with
      | (Minus, Ptr _, Int _) -> 
	  let e2 = translate_binop Minus (C.exp_of_int 0, t2) (e2, t2) in
 	    (Plus, (e1, t1), e2)
	      
      | (Mult|Plus|Minus|Div|Mod|BAnd|BXor|BOr|Gt|Eq as op, Int k1, Int k2) -> 
(* TODO: put promote in csyntax!! *)
	  let k = Newspeak.max_ikind (C.promote k1) (C.promote k2) in
	  let t = Int k in
	  let e1 = cast (e1, Int k1) t in
	  let e2 = cast (e2, Int k2) t in
	    (op, (e1, t), (e2, t))
	      
      | (Mult|Plus|Minus|Div|Gt|Eq as op, Float n1, Float n2) -> 
	  let n = max n1 n2 in
	  let t = Float n in
	  let e1 = cast (e1, Float n1) t in
	  let e2 = cast (e2, Float n2) t in
	    (op, (e1, t), (e2, t))
	      
      | (Mult|Plus|Minus|Div|Gt|Eq as op, Float _, Int _)
      | (Gt|Eq as op, Ptr _, Int _) ->
	  let e2 = cast (e2, t2) t1 in
	    (op, (e1, t1), (e2, t1))
	      
      | (Mult|Plus|Minus|Div|Gt|Eq as op, Int _, Float _)
      | (Gt|Eq as op, Int _, Ptr _) -> 
	  let e1 = cast (e1, t1) t2 in
	    (op, (e1, t2), (e2, t2))
	      
      | (Shiftl|Shiftr as op, Int (_, n), Int _) -> 
	  let k = (N.Unsigned, n) in
	  let t = Int k in
	  let e1 = cast (e1, t1) t in
	    (op, (e1, t), (e2, t))

      | (Plus, Int _, Ptr _) -> 
	  Npkcontext.report_dirty_warning 
	    "Firstpass.normalize_binop"
	    ("addition of a pointer to an integer,"
	      ^" please swap the arguments of operator '+'");
	  (Plus, (e2, t2), (e1, t1))
	      
      | _ -> (op, (e1, t1), (e2, t2))
	  
  and translate_binop op e1 e2 =
    (* TODO: think about it, maybe there are nicer ways to write this!!!*)
    let (op, (e1, t1), (e2, t2)) = normalize_binop op e1 e2 in
    let (op, t) =
      match (op, t1, t2) with
	  (* Arithmetic operations *)
	  (* Thanks to normalization t1 = t2 *)
	  (Mult, Int _, Int _) -> (N.MultI, t1)
	| (Plus, Int _, Int _) -> (N.PlusI, t1)
	| (Minus, Int _, Int _) -> (N.MinusI, t1)
	| (Div, Int _, Int _) -> (N.DivI, t1)
	| (Mod, Int _, Int _) -> (N.Mod, t1)
	| (BAnd, Int k, Int _) -> (N.BAnd (Newspeak.domain_of_typ k), t1)
	| (BXor, Int k, Int _) -> (N.BXor (Newspeak.domain_of_typ k), t1)
	| (BOr, Int k, Int _) -> (N.BOr (Newspeak.domain_of_typ k), t1)
	    
	(* Thanks to normalization t1 = t2 *)
	| (Shiftl, Int _, Int _) -> (N.Shiftlt, t1)
	| (Shiftr, Int _, Int _) -> (N.Shiftrt, t1)
	    
	(* Float operations *)
	(* Thanks to normalization t1 = t2 *)
	| (Mult, Float n, Float _) -> (N.MultF n, t1)
	| (Plus, Float n, Float _) -> (N.PlusF n, t1)
	| (Minus, Float n, Float _) -> (N.MinusF n, t1)
	| (Div, Float n, Float _) -> (N.DivF n, t1)
	    
	(* Pointer operations *)
	| (Plus, Ptr _, Int _) -> (N.PlusPI, t1)
	    
	| (Minus, Ptr _, Ptr _) -> (N.MinusPP, int_typ)
	    
	(* Integer comparisons *)
	(* Thanks to normalization t1 = t2 *)
	(* Function translate_scalar_typ will ensure they are both scalar 
	   types *)
	| (Gt, _, _) -> (N.Gt (translate_scalar_typ t1), int_typ)
	| (Eq, _, _) -> (N.Eq (translate_scalar_typ t1), int_typ)
	    	    
	| _ ->
	    Npkcontext.error "Csyntax.translate_binop" 
	      "unexpected binary operator and arguments"
    in
    let e2 = 
      match (op, t) with
	  (N.PlusPI, Ptr (Fun _)) ->
	    Npkcontext.error "Firstpass.translate_binop"
	      "pointer arithmetic forbidden on function pointers"
	| (N.PlusPI, Ptr t) -> 
	    let t = translate_typ t in
	    let step = C.exp_of_int (C.size_of t) in
	      C.Binop (N.MultI, e2, step)
	| _ -> e2
    in
    let e = C.Binop (op, e1, e2) in
      (* add coerce if necessary *)
    let e =
      match (op, t1, t) with
	  ((N.PlusI|N.MinusI|N.MultI|N.DivI|N.Shiftlt|N.Shiftrt), _, Int k) -> 
	    C.Unop (K.Coerce (Newspeak.domain_of_typ k), e)
	| (N.MinusPP, Ptr t, _) -> 
	    let t = translate_typ t in
	    let step = C.size_of t in
	    let e = C.Binop (N.DivI, e, C.exp_of_int step) in
	      C.Unop (K.Coerce (Newspeak.domain_of_typ C.int_kind), e)
	| _ -> e
    in
      (e, t)

  and translate_unop op (e, t) = 
    match (op, t, e) with
      | (Neg, Int _, _) -> translate_binop Minus (C.exp_of_int 0, t) (e, t)
      | (Neg, Float _, _) -> 
	  translate_binop Minus (C.exp_of_float 0., t) (e, t)
      | (Not, Int _, _) -> (C.Unop (K.Not, e), int_typ)
      | (BNot, Int k, _) -> 
	  let k' = C.promote k in
	  let t' = Int k' in
	    (C.Unop (K.BNot (Newspeak.domain_of_typ k'), cast (e, t) t'), t')
      | _ -> 
	  Npkcontext.error "Csyntax.translate_unop" 
	    "Unexpected unary operator and argument"

  and size_of t = C.size_of (translate_typ t)

  and align_of t =
    match t with
	Struct (n, _) | Union (n, _) ->
	  let (_, _, a) = 
	    try Hashtbl.find compdefs n
	    with Not_found -> 
	      Npkcontext.error "Firstpass.size_of_struct" 
		("unknown structure or union"^n) 
	  in
	    a
      | Array (t, _) -> align_of t
      | _ -> size_of t
  in

  let translate_proto_ftyp f (args, ret) = 
    if args = [] then begin
      Npkcontext.print_warning "Firstpass.translate_proto_ftyp" 
	("Incomplete prototype for function "^f);
    end;
    let (ft, _) = normalize_ftyp (args, ret) in
      translate_ftyp ft
  in

  let translate_global (x, loc) =
    Npkcontext.set_loc loc;
    match x with
	FunctionDef (f, Fun ft, static, body) ->
	  current_fun := f;
	  let (ft, args) = normalize_ftyp ft in
	  let ft'= translate_ftyp ft in
	    (* TODO: not nice the signature of this function is not good *)
	  let f' = update_funtyp f static ft ft' loc in
	  let formalids = add_formals loc ft in
	  let body = translate_blk body in
	  let body = (C.Block (body, Some (ret_lbl, [])), loc)::[] in
	    update_funbody f' (formalids, body);
	    remove_formals args;
	    current_fun := "";
	    Hashtbl.clear lbl_tbl;
	    lbl_cnt := default_lbl

      | FunctionDef _ -> 
	  Npkcontext.error "Firstpass.translate_global" 
	    "function type expected"

      | GlbEDecl d -> translate_enum d loc

      (* TODO: not good, simplify this code *)
      | GlbVDecl ((None, t, _, _), _) -> 
	  let _ = translate_typ t in
	    ()

(* TODO: put this check in parser ?? *)
      | GlbVDecl ((_, _, _, Some _), is_extern) when is_extern -> 
	  Npkcontext.error "Firstpass.translate_global"
	    "Extern globals can not be initizalized"
 
      | GlbVDecl ((Some x, t, static, init), is_extern) ->
	  begin match (t, init) with
	      (Fun ft, None) -> 
		let ft' = translate_proto_ftyp x ft in
		  (* TODO not nice, this function signature is not good *)
		let _ = update_funtyp x static ft ft' loc in
		  ()

	    | (Fun _, Some _) -> 
		Npkcontext.error "Firstpass.translate_global"
		  ("Unexpected initialization of function "^x)
	    | _ -> 
		let (t, init) = translate_glb_init t init in
		let init = if is_extern then None else Some init in
		let t' = translate_typ t in
		  (* TODO not nice, this function signature is not good *)
		  if static then add_static x loc (t, init, t')
		  else add_global x loc (t, init, t')
	  end
  in
    
  let translate_token x =
    match x with
	SymbolToken x -> Newspeak.SymbolToken x
      | IdentToken x when Hashtbl.mem symbtbl x -> 
	  let (v, _, _) = find_symb x in
	  let x = 
	    match v with
		VarSymb (C.Global x) -> x
	      | _ -> 
		  Npkcontext.error "Firstpass.translate_token"
		    "unexpected variable in specification"
	  in
	    Newspeak.VarToken x
      | IdentToken x -> Newspeak.IdentToken x
(* TODO: not good, do this in compile phase *)
      | CstToken (C.CInt i, _) -> Newspeak.CstToken (Newspeak.CInt i)
      | CstToken (C.CFloat f, _) -> Newspeak.CstToken (Newspeak.CFloat f)
  in

(* TODO: a tad hacky!! Think about it *)
  let collect_glb_structdefs (x, _) =
    match x with
	GlbVDecl ((_, t, _, _), _) -> 
	  begin try 
	    let _ = translate_typ t in
	      ()
	  with _ -> ()
	  end
      | _ -> ()
  in

(* TODO: a tad inefficient *)
    List.iter collect_glb_structdefs globals;
    List.iter translate_global globals;
    let spec = List.map (List.map translate_token) spec in
      (glbdecls, fundefs, spec)
