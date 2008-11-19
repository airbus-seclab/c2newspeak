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

(* TODO: get in a first pass the type of all functions 
   no type inference at function call
   no refinment of function argument types other than None, Some args -> args
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
(* TODO: not minimal, think about it *)
type symb =
  | GlobalSymb of string 
  | FunSymb of string
  | LocalSymb of C.lv 
  | EnumSymb of C.exp

(* functions *)
(* [next_aligned o x] returns the smallest integer greater or equal than o,
   which is equal to 0 modulo x *)
let next_aligned o x =
  let m = o mod x in
    if m = 0 then o else o + (x - m)

(* TODO: code cleanup: find a way to factor this with create_cstr
   in Npkil *)
let seq_of_string str =
  let len = String.length str in
  let res = ref [(None, Data (exp_of_char '\x00'))] in
    for i = len - 1 downto 0 do
      res := (None, Data (exp_of_char str.[i]))::!res
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
  let used_globals = Hashtbl.create 100 in
  (* Used to generate static variables names *)
  let current_fun = ref "" in
  (* Counter of static variables, necessary to distinguish 2 statics in 
     different scope of the same function, who would have the same name
  *)
  let static_cnt = ref 0 in

  let tmp_cnt = ref 0 in

  let lbl_tbl = Hashtbl.create 10 in
  let lbl_cnt = ref default_lbl in

  let find_compdef name =
    try Hashtbl.find compdefs name
    with Not_found -> 
      Npkcontext.error "Firstpass.find_compdef" 
	("unknown structure or union "^name)
  in

  let fields_of_comp name =
    let (_, f, _, _) = find_compdef name in
      f
  in

  let use_global name =
    let (t, loc, init, used) = Hashtbl.find used_globals name in
      if not used 
      then Hashtbl.replace used_globals name (t, loc, init, true)
  in

  let new_lbl () =
    incr lbl_cnt;
    !lbl_cnt
  in

  let add_var (t, x) =
    let id = C.fresh_id () in
      Hashtbl.add symbtbl x (LocalSymb (C.Var id), t);
      id
  in

  let update_var_typ x id t =
    Hashtbl.replace symbtbl x (LocalSymb (C.Var id), t)
  in

  let remove_symb x = Hashtbl.remove symbtbl x in

  let add_formals (args_t, ret_t) =
    let ret_id = add_var (ret_t, ret_name) in
    let args_id = List.map add_var args_t in
      (ret_id, args_id)
  in
    
  let remove_formals (args_t, _) =
    remove_symb ret_name;
    List.iter (fun (_, x) -> remove_symb x) args_t
  in

  let find_symb x = 
    try Hashtbl.find symbtbl x
    with Not_found -> 
      if Gnuc.is_gnuc_token x && not !Npkcontext.gnuc then begin
	Npkcontext.report_accept_warning "Firstpass.translate.typ_of_var" 
	  ("unknown identifier "^x^", probably a GNU C token") Npkcontext.GnuC
      end;
      Npkcontext.error "Firstpass.translate.typ_of_var" 
	("unknown identifier "^x)
  in

  let is_enum x =
    let (v, _) = find_symb x in
    match v with
	EnumSymb _ -> true
      | _ -> false
  in

  let find_enum x =
    let (v, t) = find_symb x in
    match v with
	EnumSymb i -> (i, t)
      | _ -> 
	  Npkcontext.error "Firstpass.translate.typ_of_var" 
	  ("enum identifier expected: "^x)
  in

  let find_var x =
    let (v, t) = find_symb x in
    match v with
	LocalSymb v -> (v, t)
      | FunSymb v -> (C.Global v, t)
      | GlobalSymb v -> 
	  use_global v;
	  (C.Global v, t)
      | _ -> 
	  Npkcontext.error "Firstpass.find_var" 
	    ("variable identifier expected: "^x)
  in

  let is_fname x =
    let (v, _) = find_symb x in
    match v with
	FunSymb _ -> true
      | _ -> false
  in

  let find_fname x =
    let (v, t) = find_symb x in
      match (v, t) with
	  (FunSymb f, Fun (Some args_t, ret_t)) -> (f, (args_t, ret_t))
	| (FunSymb _, Fun _) -> 
	    Npkcontext.error "Firstpass.find_fname"
	      "unknown arguments type at function call"
	| _ -> raise Not_found
  in

  let update_global x name loc (t, init) =
    let v = GlobalSymb name in
    let init =
      try 
	let (_, _, prev_init, _) = Hashtbl.find used_globals name in
	  Hashtbl.replace symbtbl x (v, t);
	  match (prev_init, init) with
	      (None, Some _) | (Some None, Some _) -> init
	    | (Some _, None) | (Some _, Some None) -> prev_init
	    | (None, None) -> None
	    | (Some Some _, Some Some _) -> 
		Npkcontext.error "Firstpass.update_global"
		  ("global variable "^x^" initialized twice")
      with Not_found -> 
	Hashtbl.add symbtbl x (v, t);
	init
    in
      if (init = None) 
      then Hashtbl.replace used_globals name (t, loc, init, false)
      else Hashtbl.replace used_globals name (t, loc, init, true)
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

  let push_enum (x, i) = 
    Hashtbl.add symbtbl x (EnumSymb i, int_typ)
  in

  let add_fundef f body t loc = Hashtbl.replace fundefs f (t, loc, Some body) in

  let translate_lbl lbl =
    try Hashtbl.find lbl_tbl lbl
    with Not_found -> 
      let lbl' = new_lbl () in
	Hashtbl.add lbl_tbl lbl lbl';
	lbl'
  in

  let rec cast (e, t) t' = 
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
	    (* TODO: should I be using translate_set here too??? *)
	    let e = cast (translate_exp e) t in
	      res := (o, translate_typ t, e)::!res;
	      t
	      
	| (Sequence seq, Array (t, len)) -> 
	    let n = 
	      match translate_array_len len with
		  Some n -> n
		| None -> List.length seq
	    in
	      translate_sequence o t n seq;
	      Array (t, Some (exp_of_int n))
	
	| (Sequence seq, Comp s) ->
	    let f = fields_of_comp s in
	      translate_field_sequence o f seq;
	      t

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
		Npkcontext.report_accept_warning 
		  "Firstpass.translate_init.translate_field_sequence" 
		  "missing initializers for structure" Npkcontext.DirtySyntax
	      end;
	      translate_field_sequence o fields []

	| ([], _) -> 
	    Npkcontext.report_accept_warning 
	      "Firstpass.translate_init.translate_field_sequence" 
	      "extra initializer for structure" Npkcontext.DirtySyntax
	  
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
	    Npkcontext.report_accept_warning 
	      "Firstpass.translate_init.translate_sequence" 
	      "extra initializer for array" Npkcontext.DirtySyntax
	      
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
	  Int _ -> res := (o, translate_typ t, C.exp_of_int 0)::!res
	| Ptr _ -> 
	    (* TODO: inefficient: t is translated twice *)
	    let e = cast (translate_exp (exp_of_int 0)) t in
	      res := (o, translate_typ t, e)::!res
	| Float _ -> 
	    res := (o, translate_typ t, C.exp_of_float 0.)::!res
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
		
	| Comp s -> 
	    let f = fields_of_comp s in
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
	  let get_scalar (o, t, e) = (o, C.scalar_of_typ t, e) in
	  let init = List.map get_scalar init in
	    (t, Some init)

  and add_glb_cstr str =
    let fname = Npkcontext.get_fname () in
    let name = "!"^fname^".const_str_"^(String.escaped str) in
    let t = Array (char_typ, Some (exp_of_int ((String.length str) + 1))) in
    let loc = Npkcontext.get_loc () in
    let (t, init) = translate_glb_init t (Some (Data (Str str))) in
      if not (Hashtbl.mem used_globals name) 
      then add_global name loc (t, Some init);
      (C.Global name, t)
  
  and translate_lv x =
    match x with
	Var x -> find_var x

      | Field (lv, f) -> 
	  let (lv, t) = translate_lv lv in
	  let r = fields_of_comp (Csyntax.comp_of_typ t) in
	  let (o, t) = 
	    try List.assoc f r 
	    with Not_found -> 
	      Npkcontext.error "Firstpass.translate_lv" 
		("unknown field '"^f^"' in union or structure")
	  in
	  let o = C.exp_of_int o in
	    (C.Shift (lv, o), t)

      | Index (e, idx) -> 
	  let (lv, t) = translate_lv e in begin
	    match t with
		Array (t, len) ->
		  let i = translate_exp idx in
		  let n = translate_array_len len in
		    translate_array_access (lv, t, n) i

	      | Ptr _ -> translate_lv (Deref (Binop (Plus, e, idx)))
	      | _ -> 
		  Npkcontext.error "Firstpass.translate_lv" 
		    "array or pointer type expected"
	  end

      | Deref e -> deref (translate_exp e)

      | OpExp (op, lv, is_after) ->
	  let loc = Npkcontext.get_loc () in
	  let e = Cst (C.CInt (Nat.of_int 1), int_typ) in
	  let (incr, t) = translate_set (lv, Some op, e) in
	  let (lv, _, _) = incr in
	    (C.Stmt_lv ((C.Set incr, loc), lv, is_after), t)

      | Str str -> add_glb_cstr str

      | Cast (lv, t) -> 
	  Npkcontext.report_accept_warning "Firstpass.translate_stmt" 
	    "cast of left value" Npkcontext.DirtySyntax;
	  let (lv, _) = translate_lv lv in
	    (lv, t)

      | _ -> Npkcontext.error "Firstpass.translate_lv" "left value expected"

  and translate_array_access (lv, t, n) i =
    try
      let (i, _) = i in
      let len = 
	try C.length_of_array n lv 
	with Invalid_argument _ when !Npkcontext.accept_flex_array -> 
	  raise Exit
      in
      let sz = C.exp_of_int (size_of t) in
      let o = C.Unop (K.Belongs_tmp (Nat.zero, len), i) in
      let o = C.Binop (N.MultI, o, sz) in
	(C.Shift (lv, o), t)
    with Exit -> 
      let e = C.remove_fst_deref lv in
      let e = translate_binop Plus (e, Ptr t) i in
	deref e

  and translate_exp e =
    let rec translate e =
      match e with
	  Cst (c, t) -> (C.Const c, t)
	    
	| Var x when is_enum x -> find_enum x
	    
	| Var _ | Field _ | Index _ | Deref _ | OpExp _ | Str _ -> 
	    let (lv, t) = translate_lv e in
	      (C.Lval (lv, translate_typ t), t)
		
	| AddrOf (Deref e) -> translate_exp e
	      	      
	| AddrOf (Index (lv, Cst (C.CInt i, _)))
	    when Nat.compare i Nat.zero = 0 ->
	    let (lv', t) = translate_lv lv in begin
		match t with
		    Array (elt_t, _) -> 
		      let (e, _) = addr_of (lv', t) in
			(e, Ptr elt_t)
		  | Ptr _ -> translate (AddrOf (Deref lv))
		  | _ -> 
		      Npkcontext.error "Firstpass.translate_lv" 
			"Array type expected"
	      end
						
	| AddrOf (Index (lv, e)) -> 
	    let base = AddrOf (Index (lv, exp_of_int 0)) in
	      translate (Binop (Plus, base, e))
		
	| AddrOf lv -> addr_of (translate_lv lv)
	    
	(* Here c is necessarily positive *)
	| Unop (Neg, Cst (C.CInt c, Int (_, sz))) -> 
	    (C.Const (C.CInt (Nat.neg c)), Int (N.Signed, sz))
	      
	| Unop (op, e) -> 
	    let e = translate_exp e in
	      translate_unop op e
		
	| Binop (op, e1, e2) -> 
	    let e1 = translate_exp e1 in
	    let e2 = translate_exp e2 in
	      translate_binop op e1 e2
		
	| IfExp (c, e1, e2) -> begin
	    try 
	      let (c, _) = translate c in
	      let v = C.eval_exp c in
	      let e = if Nat.compare v Nat.zero <> 0 then e1 else e2 in
		translate e
	    with Invalid_argument _ -> 
	      let loc = Npkcontext.get_loc () in
		(* TODO: this is a bit inefficient, 
		   e1 gets translated twice!! *)
	      let (_, t) = translate_exp e1 in
	      let (x, decl, v) = gen_tmp loc t in
	      let blk1 = (Exp (Set (Var x, None, e1)), loc)::[] in
	      let blk2 = (Exp (Set (Var x, None, e2)), loc)::[] in
	      let set = (If (c, blk1, blk2), loc) in
	      let set = translate_stmt set in
		remove_symb x;
		(C.Pref (decl::set, C.Lval (v, translate_typ t)), t)
	  end
	    
	| SizeofE e ->
	    let (_, t) = translate e in
	    let sz = (size_of t) / 8 in
	      (C.exp_of_int sz, uint_typ)
		
	| Sizeof t -> 
	    let sz = (size_of t) / 8 in
	      (C.exp_of_int sz, uint_typ)
		
	| Cast (e, t) -> 
	    let e = translate_exp e in
	    let e = cast e t in
	      (e, t)
		
	| Call f -> translate_call f
				
	| Set set ->
	    Npkcontext.report_accept_warning "Firstpass.translate_exp" 
	      "assignment within expression" Npkcontext.DirtySyntax;
	    let loc = Npkcontext.get_loc () in
	    let (set, t) = translate_set set in
	    let (lv', t', _) = set in
	    let e = C.Lval (lv', t') in
	      (C.Pref ((C.Set set, loc)::[], e), t)
	      		  
	| BlkExp blk -> translate_blk_exp blk
    in
      match translate e with
	  (C.Lval (lv, _), (Array (t', _) as t)) -> 
	    let (e, _) = addr_of (lv, t) in
	      (e, Ptr t')
	| (C.Lval (lv, _), (Fun _ as t)) -> 
	    let (e, _) = addr_of (lv, t) in
	      (e, Ptr t)
	| v -> v

(* TODO: remove this function *)	    
  and refine_args_t args_t args =
    match args_t with
	None -> 
	  Npkcontext.report_accept_warning "Firstpass.refine_args_t"
	    "unknown arguments type at function call" 
	    Npkcontext.PartialFunTyp;
	  let infer_typ i e =
	    let (_, t) = translate_exp e in
	      (t, "arg"^(string_of_int i))
	  in
	    List_utils.mapi infer_typ args
      | Some args_t -> args_t

  and translate_call (f, args) =
    match f with
	Var x when is_fname x -> 
	  let (f, (args_t, ret_t)) = find_fname x in
	  let args = translate_args args args_t in
	  let ft' = translate_ftyp (args_t, ret_t) in
	    (C.Call (ft', C.Fname f, args), ret_t)
	      
      | Deref e | e -> 
	  let (e, t) = translate_exp e in
	  let (args_t, ret_t) =
	    match t with
		Ptr (Fun t) -> t
	      | _ -> 
		  Npkcontext.error "Firstpass.translate_call"
		    "function pointer expected"
	  in
	  let args_t = refine_args_t args_t args in
	  let args = translate_args args args_t in
	  let ft' = translate_ftyp (args_t, ret_t) in
	    (C.Call (ft', C.FunDeref (e, ft'), args), ret_t)
	      
      
  and deref (e, t) =
    match t with
	Ptr t -> (C.Deref (e, translate_typ t), t)
      | _ -> Npkcontext.error "Firstpass.deref_typ" "pointer type expected"
	    
(* TODO: code cleanup: get rid of call to length_of_array in cir2npkil 
   with AddrOf and put it in here *)
  and addr_of (e, t) = (C.AddrOf (e, translate_typ t), Ptr t)

  and translate_set (lv, op, e) =
    let (lv', t) = translate_lv lv in
    let (lv', e) =
      match op with
	  None -> (lv', e)
	| Some op -> 
	    let (_, lv', _) = C.normalize_lv lv' in
	    let e = Binop (op, lv, e) in
	      (lv', e)
    in
    let e = cast (translate_exp e) t in
    let t' = translate_typ t in
    let set = (lv', t', e) in
      (set, t)
	
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
	  let (e, t) = translate_exp e in
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
	    let e = cast (translate_exp e) t in
	      e::(translate_args args args_t)
	| ([], []) -> []
	| _ -> 
	    Npkcontext.error "Firstpass.translate_exp" 
	      "different types at function call"
    in
      translate_args args args_t

  and gen_tmp loc t =
    let x = "tmp"^(string_of_int !tmp_cnt) in
    let id = add_var (t, x) in
    let t = translate_typ t in
    let decl = (C.Decl (t, x, id), loc) in
      incr tmp_cnt;
      (x, decl, C.Var id)

  and translate_field (x, (o, t)) = (x, (o, translate_typ t))

  and process_struct_fields name f =
    let o = ref 0 in
    let last_align = ref 1 in
    let translate (t, x, loc) =
      Npkcontext.set_loc loc;
      let cur_align = align_of t in
      let o' = next_aligned !o cur_align in
      let (o', t, sz) =
	match t with
	    Bitfield ((s, n), sz) ->
	      let (sz, _) = translate_exp sz in
	      let sz = Nat.to_int (C.eval_exp sz) in
		if sz > n then begin
		  Npkcontext.error "Firstpass.process_struct_fields"
		    "width of bitfield exceeds its type"
		end;
		let o' = if !o+sz <= o' then !o else o' in
		  (o', Int (s, sz), sz)
	  | Array (_, None) -> 
	      Npkcontext.report_accept_warning 
		"Firstpass.process_struct_fields" "flexible array member"
		Npkcontext.FlexArray;
	      (!o, t, 0)
	  | _ -> (o', t, size_of t)
      in
	if o' > max_int-sz then begin
	  Npkcontext.error "Firstpass.process_struct_fields" 
	    "invalid size for structure"
	end;
	o := o'+sz;
	last_align := max !last_align cur_align;
	(x, (o', t))
    in
    let f = List.map translate f in
    let sz = next_aligned !o !last_align in
      Hashtbl.add compdefs name (true, f, sz, !last_align);
      f

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
      Hashtbl.add compdefs name (false, f, !n, !align);
      f

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
      | Fun _ -> C.Fun
      | Array (t, len) -> 
	  let t = translate_typ t in
	  let len = translate_array_len len in
	    C.Array (t, len)
      | Comp s ->
	  let (is_struct, f, sz, _) = find_compdef s in
	  let f = List.map translate_field f in
	    if is_struct then C.Struct (f, sz) else C.Union (f, sz)
      | Bitfield _ -> 
	  Npkcontext.error "Firstpass.translate_typ" 
	    "bitfields not allowed outside of structures"

  and translate_array_len x =
    match x with
	None -> None
      | Some e -> 
	  let (e, _) = translate_exp e in
	  let i = 
	    try Nat.to_int (C.eval_exp e) 
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

  and translate_ftyp (args, ret) =
    let translate_arg (t, _) = translate_typ t in
    let args = List.map translate_arg args in
    let ret = translate_typ ret in
      (args, ret)

  and translate_local_decl (x, t, init) loc =
    Npkcontext.set_loc loc;
    let id = add_var (t, x) in
    let (init, t) = 
      match init with
	  None -> ([], t)
	| Some init -> translate_init t init
    in
      update_var_typ x id t;
      let v = C.Var id in
      let build_set (o, t, e) =
	let lv = C.Shift (v, C.exp_of_int o) in
	  (C.Set (lv, t, e), loc)
      in
      let init = List.map build_set init in
      let decl = (C.Decl (translate_typ t, x, id), loc) in
	decl::init

  and add_enum (x, v) =
    let v = translate_exp v in
    let v = cast v int_typ in
    let v = 
      try C.Const (C.CInt (C.eval_exp v)) 
      with Invalid_argument _ -> v
    in
      push_enum (x, v)

  and add_compdecl (x, is_struct, f) =
    let _ = 
      if is_struct then process_struct_fields x f
      else process_union_fields x f
    in
      ()

  and translate_blk x =
    let (body, _) = translate_blk_aux false x in
      body

  and translate_blk_exp x =
    let (body, e) = translate_blk_aux true x in
      match e with
	  Some (e, t) -> (C.Pref (body, e), t)
	| None -> 
	    Npkcontext.error "Firstpass.translate_blk_exp" "expression expected"

      (* type and translate blk *)
(* TODO: do a translate_blk_exp blk -> blk, typ_exp
   a translate_blk blk -> blk
   and a translate_blk_aux ends_with_exp blk -> blk, typ_exp option *)
  and translate_blk_aux ends_with_exp x = 
    let rec translate x =
      match x with
	  (Exp e, _)::[] when ends_with_exp -> 
	    let e = translate_exp e in
	      (([], []), Some e)
	    
(* TODO: maybe this loc is unnecessary *)
	| (EDecl d, _)::body -> 
	    add_enum d;
(* TODO: try translate body *)
	    let (body, e) = translate_blk_aux ends_with_exp body in
	    let (x, _) = d in
	      remove_symb x;
	      ((body, []), e)

	| (CDecl d, _)::body ->
	    add_compdecl d;
(* TODO: try translate body *)
	    let (body, e) = translate_blk_aux ends_with_exp body in
	    let (x, _, _) = d in
	      Hashtbl.remove compdefs x;
	      ((body, []), e)

	| (VDecl (x, Fun ft, static, _, _), loc)::body -> 
	    Npkcontext.report_accept_warning "Firstpass.translate" 
	      "function declaration within block" Npkcontext.DirtySyntax;
	    translate_proto_ftyp x static ft loc;
	    translate body

	| (VDecl (_, _, _, extern, _), _)::_ when extern ->
	    Npkcontext.error "Firstpass.translate" 
	      "local variable declared extern"

	| (VDecl (x, t, static, _, init), loc)::body when static -> 
	    Npkcontext.set_loc loc;
	    let (t, init) = translate_glb_init t init in
	      add_static x loc (t, Some init);
	      (* TODO: try translate body *)
	      let (body, e) = translate_blk_aux ends_with_exp body in
		remove_symb x;
		((body, []), e)
	  
	| (VDecl (x, t, _, _, init), loc)::body -> 
	    let init = translate_local_decl (x, t, init) loc in
	      (* TODO: try translate body *)
	    let (body, e) = translate_blk_aux ends_with_exp body in
	      remove_symb x;
	      ((init@body, []), e)
	      
	(* TODO: do the case where suffix is <> [] *)
	(* TODO: remove body, suffix from For, use goto and labels
	   remove break. Use goto... *)
	| ((Label _, _) as stmt)::(For ([], e, body, []), loc)::tl ->
	    let blk = ((For ([], e, body@(stmt::[]), []), loc)::tl) in
	      translate blk
		
	| (Label lbl, loc)::tl -> 
	    let lbl = translate_lbl lbl in
	    let ((x, tl), e) = translate tl in
	      (([], (lbl, loc, x)::tl), e)

	| hd::tl -> 
	    let hd = translate_stmt hd in
	    let ((x, tl), e) = translate tl in
	      ((hd@x, tl), e)
		
	| [] -> (([], []), None)
    in
    let rec stitch (x, tl) =
      match tl with
	  [] -> x
	| (lbl, loc, blk)::tl -> 
	    let blk = (C.Block (x, Some (lbl, [])), loc)::blk in
	      stitch (blk, tl)
    in
    let (blk, e) = translate x in
      (stitch blk, e)

  and translate_stmt_exp loc e =
    match e with
	Set (lv, op, IfExp (c, e1, e2)) ->
	  let e = IfExp (c, Set (lv, op, e1), Set (lv, op, e2)) in
	    translate_stmt_exp loc e

      | Set set -> 
	  let (set, _) = translate_set set in
	    (C.Set set, loc)::[]

      | Cast (e, Void) -> 
	  Npkcontext.report_accept_warning "Firstpass.translate_stmt" 
	    "cast to void" Npkcontext.DirtySyntax;
	  translate_stmt (Exp e, loc)

      | IfExp (c, e1, e2) ->
	  let blk1 = (Exp e1, loc)::[] in
	  let blk2 = (Exp e2, loc)::[] in
	    translate_stmt (If (c, blk1, blk2), loc)

      | _ -> 
	  let (e, _) = translate_exp e in
	    (C.Exp e, loc)::[]

	(* type and translate_stmt *)
  and translate_stmt (x, loc) = 
    Npkcontext.set_loc loc;
    match x with
	Exp e -> translate_stmt_exp loc e

      | Break -> (C.Goto brk_lbl, loc)::[]

      | Continue -> (C.Goto cnt_lbl, loc)::[]

      | Return None -> (C.Goto ret_lbl, loc)::[]

      | Return (Some e) ->
(* TODO: put this in parser already?? *)
	  let set = (Exp (Set (Var ret_name, None, e)), loc) in
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

      | Label _ | VDecl _ | EDecl _ | CDecl _ -> 
	  Npkcontext.error "Firstpass.translate_stmt" "unreachable code"

  (* TODO: 
     - should take advantage of obviously side-effect free expression 
     - add option to translate if using tmp variables!
*)
  and translate_if loc (e, blk1, blk2) =
    let duplicate branch blk e1 e2 =
      let b =
	match (e1, e2) with
	    (Cst (C.CInt c, _), _) | (_, Cst (C.CInt c, _)) -> 
	      (Nat.compare c Nat.zero <> 0) = branch
	  | _ -> true
      in
	b && (C.is_large_blk blk)
    in
    let build_branch branch blk e1 e2 =
      if duplicate branch blk e1 e2 then begin
	let lbl = new_lbl () in
	let goto = (C.Goto lbl, loc)::[] in
	  (Some (lbl, blk), goto)
      end else (None, blk)
    in
    let rec translate e blk1 blk2 =
      match e with
	  IfExp (c, e1, e2) -> 
	    let (lbl1, goto1) = build_branch true blk1 e1 e2 in
	    let (lbl2, goto2) = build_branch false blk2 e1 e2 in

	    let br1 = translate e1 goto1 goto2 in
	    let br2 = translate e2 goto1 goto2 in

	    let x = translate c br1 br2 in
	    let x = (C.Block (x, lbl1), loc)::[] in
	    let x = (C.Block (x, lbl2), loc)::[] in
	      x
	| Unop (Not, (IfExp _ as e)) -> translate e blk2 blk1
	| Cst (C.CInt c, _) when Nat.compare c Nat.zero <> 0 -> blk1
	| Cst (C.CInt _, _) -> blk2
	| _ -> 
	    let (e, _) = translate_exp e in
	      (C.If (e, blk1, blk2), loc)::[]
    in
      translate e blk1 blk2

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

(* TODO: all this is not good, think about it
   min_ftp should be done in Cir?? *)
  and update_funtyp f ft1 =
    let (symb, t) = Hashtbl.find symbtbl f in
    let ft2 = Csyntax.ftyp_of_typ t in
    let ft = Csyntax.min_ftyp ft1 ft2 in
      Hashtbl.replace symbtbl f (symb, Fun ft)

  and update_funsymb f static ct loc =
    let (fname, _, _) = loc in
    let f' = if static then "!"^fname^"."^f else f in
    let _ =
      try update_funtyp f ct
      with Not_found -> Hashtbl.add symbtbl f (FunSymb f', Fun ct)
    in
      match ct with
	  (Some args_t, ret_t) -> 
	    if not (Hashtbl.mem fundefs f') then begin
	      let ft = translate_ftyp (args_t, ret_t) in
		Hashtbl.add fundefs f' (ft, loc, None);
	    end;
	    f'
	| _ -> f'

  and translate_proto_ftyp f static (args, ret) loc =
    if args = None then begin
      Npkcontext.print_warning "Firstpass.check_proto_ftyp" 
	("incomplete prototype for function "^f)
    end;
    let _ = update_funsymb f static (args, ret) loc in
      ()

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
	  Npkcontext.report_accept_warning "Firstpass.normalize_binop"
	    "addition of a pointer to an integer" Npkcontext.DirtySyntax;
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
	    let step = C.exp_of_int (size_of t) in
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
	    let step = size_of t in
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
	Comp n ->
	  let (_, _, _, a) = 
	    try Hashtbl.find compdefs n
	    with Not_found -> 
	      Npkcontext.error "Firstpass.size_of_struct" 
		("unknown structure or union "^n) 
	  in
	    a
      | Array (t, _) -> align_of t
      | Bitfield (k, _) -> align_of (Int k)
      | _ -> size_of t
  in

  let translate_global (x, loc) =
    Npkcontext.set_loc loc;
    match x with
	FunctionDef (f, _, _, body) ->
	  current_fun := f;
	  let (f', ft) = find_fname f in
	  let formalids = add_formals ft in
	  let body = translate_blk body in
	  let body = (C.Block (body, Some (ret_lbl, [])), loc)::[] in
	    add_fundef f' (formalids, body) (translate_ftyp ft) loc;
	    remove_formals ft;
	    current_fun := "";
	    Hashtbl.clear lbl_tbl;
	    lbl_cnt := default_lbl
	      
(* TODO: put this check in parser ?? *)
      | GlbVDecl (_, _, _, extern, Some _) when extern -> 
	  Npkcontext.error "Firstpass.translate_global"
	    "extern globals can not be initizalized"
 
      | GlbVDecl (_, Fun _, _, _, _) -> ()

      | GlbVDecl (x, t, static, extern, init) ->
	  let (t, init) = translate_glb_init t init in
	  let init = if extern then None else Some init in
	    if static then add_static x loc (t, init)
	    else add_global x loc (t, init)

      | GlbEDecl _ | GlbCDecl _ -> ()
  in
    
  let translate_token x =
    match x with
	SymbolToken x -> Newspeak.SymbolToken x
      | IdentToken x when Hashtbl.mem symbtbl x -> 
	  let (v, _) = find_var x in
	  let x = 
	    match v with
		C.Global name -> name
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
(* TODO: could be done in the parser *)
  let collect_glb_structdefs (x, _) =
    match x with
	GlbCDecl d -> add_compdecl d
      | GlbEDecl d -> add_enum d
      | _ -> ()
  in

  let add_glbdecl name (t, loc, init, used) =
    if used || (not !Npkcontext.remove_temp) then begin
      Npkcontext.set_loc loc;
      let t = translate_typ t in
	Hashtbl.add glbdecls name (t, loc, init)
    end
  in

  let collect_funtyps (x, loc) =
    Npkcontext.set_loc loc;
    match x with
	FunctionDef (f, Fun (args_t, ret_t), static, _) ->
	  let args_t = 
	    match args_t with
		None -> []
	      | Some args_t -> args_t
	  in
(* TODO: maybe update_funsymb should not return anything
   TODO: update of a function type should be easy (None, Some)
   TODO: there shouldn't be any type inference at all
   TODO: print a warning for forwar declaractions with dirty_syntax, they are
   not nice!
*)
	  let _ = update_funsymb f static (Some args_t, ret_t) loc in
	    ()

      | FunctionDef _ -> 
	  Npkcontext.error "Firstpass.translate_global" 
	    "function type expected"

      | GlbVDecl (f, Fun ft, static, _, None) -> 
	  translate_proto_ftyp f static ft loc

      | GlbVDecl (f, Fun _, _, _, Some _) -> 
	  Npkcontext.error "Firstpass.translate_global"
	    ("unexpected initialization of function "^f)

      | _ -> ()
  in

(* TODO: a tad inefficient *)
    List.iter collect_glb_structdefs globals;
    List.iter collect_funtyps globals;
    List.iter translate_global globals;
    Hashtbl.iter add_glbdecl used_globals;
    let spec = List.map (List.map translate_token) spec in
      (glbdecls, fundefs, spec)
