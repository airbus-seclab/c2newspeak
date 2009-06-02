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

(* TODO: should not fill initializations with 0 by default!!!! 
   should push array length evaluation till newspeak
*)

(* TODO: should rename firstpass to semantic ??? see compiler Appel book *)
open CoreC
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
  | LocalSymb of C.lv 
  | EnumSymb of C.exp
  | CompSymb of ((string * (int * typ)) list * int * int)

(* functions *)
let find_field f r =
  try List.assoc f r 
  with Not_found -> 
    Npkcontext.report_error "Firstpass.translate_lv" 
      ("unknown field '"^f^"' in union or structure")

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
  let glbdecls = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in
    
(* TODO: find a way to remove Symbtbl and use a standard Hashtbl here! 
   but first needs to put the whole typing phase before firstpass
*)
  let symbtbl = Symbtbl.create () in
(* TODO: used_globals and one pass could be removed, if cir had a structure
   type with only the name and a hashtbl of structure names to type!!!, 
   should be done!*)
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
    try 
      let (c, _) = Symbtbl.find symbtbl name in
	match c with
	    CompSymb c -> c
	  | _ -> raise Not_found
    with Not_found ->
      Npkcontext.report_error "Firstpass.find_compdef" 
	("unknown structure or union "^name)
  in
    (* TODO: remove this function, put a record, instead of a tuple in compdef
    *)
  let fields_of_comp name =
    let (f, _, _) = find_compdef name in
      f
  in

  let align_of_comp name =
    let (_, _, a) = find_compdef name in
      a
  in

  let new_lbl () =
    incr lbl_cnt;
    !lbl_cnt
  in

  let add_var (t, x) = Symbtbl.bind symbtbl x (LocalSymb (C.Local x), t) in

  let update_var_typ x t =
    Symbtbl.update symbtbl x (LocalSymb (C.Local x), t)
  in

  let add_formals (args_t, ret_t) =
    add_var (ret_t, ret_name);
    List.iter add_var args_t;
    let args_id = List.map snd args_t in
      (ret_name, args_id)
  in
    
  let find_symb x = 
    try Symbtbl.find symbtbl x
    with Not_found -> 
      if (Gnuc.is_gnuc_token x) && (not !Npkcontext.accept_gnuc) then begin
	Npkcontext.report_accept_warning "Firstpass.translate.find_symb" 
	  ("unknown identifier "^x^", maybe a GNU C symbol") Npkcontext.GnuC
      end;
      Npkcontext.report_accept_warning "Firstpass.translate.find_symb" 
	("unknown identifier "^x^", maybe a function without prototype") 
	Npkcontext.MissingFunDecl;
      let info = (GlobalSymb x, Fun (None, CoreC.int_typ)) in
	(* TODO: clean up find_compdef + clean up accesses to Symbtbl *)
	Symbtbl.bind symbtbl x info;
	info
  in

  let update_funtyp f ft1 =
    let (symb, t) = Symbtbl.find symbtbl f in
    let ft2 = CoreC.ftyp_of_typ t in
    let ft = CoreC.min_ftyp ft1 ft2 in
      Symbtbl.update symbtbl f (symb, Fun ft)
  in

  let update_funsymb f static ft loc =
    let (fname, _, _) = loc in
    let f' = if static then "!"^fname^"."^f else f in
      try update_funtyp f ft
      with Not_found -> Symbtbl.bind symbtbl f (GlobalSymb f', Fun ft)
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
	  Npkcontext.report_error "Firstpass.translate.typ_of_var" 
	  ("enum identifier expected: "^x)
  in

  let find_var x =
    let (v, t) = find_symb x in
      match v with
	  LocalSymb v -> (v, t)
	| GlobalSymb v -> (C.Global v, t)
	| _ -> 
	    Npkcontext.report_error "Firstpass.find_var" 
	      ("variable identifier expected: "^x)
  in

  let is_fname x =
    let (_, t) = find_symb x in
      match t with
	  Fun _ -> true
	| _ -> false
  in

  let find_fname x =
    let (v, t) = find_symb x in
      match (v, t) with
	  (GlobalSymb f, Fun ft) -> (f, ft)
	| _ -> 
	    Npkcontext.report_error "Firstpass.find_fname" 
	      ("unknown function: "^x)
  in

  let update_global x name loc (t, init) =
    let v = GlobalSymb name in
    let (loc, init) =
      try 
	let (_, prev_loc, prev_init) = Hashtbl.find used_globals name in
	  Symbtbl.update symbtbl x (v, t);
	  match (prev_init, init) with
	      (None, Some _) | (Some None, Some _) -> (loc, init)
	    | (Some _, None) | (Some _, Some None) -> (prev_loc, prev_init)
	    | (None, None) -> (loc, None)
	    | (Some Some _, Some Some _) -> 
		Npkcontext.report_error "Firstpass.update_global"
		  ("global variable "^x^" initialized twice")
      with Not_found -> 
	Symbtbl.bind symbtbl x (v, t);
	(loc, init)
    in
(* TODO:TODO: remove used_globals in firstpass, done in cir2npkil?? *)
      Hashtbl.replace used_globals name (t, loc, init)
  in

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

  let add_fundef f (ret, args) body t = 
    Hashtbl.replace fundefs f (ret, args, t, body) 
  in

  let translate_lbl lbl =
    try Hashtbl.find lbl_tbl lbl
    with Not_found -> 
      let lbl' = new_lbl () in
	Hashtbl.add lbl_tbl lbl lbl';
	lbl'
  in

  let rec cast (e, t1) t2 = 
    match t2 with
	Comp (name, false) when t1 <> t2 -> 
	  let f = fields_of_comp name in
	    if not (List.exists (fun (_, (_, f_t)) -> f_t = t1) f) 
	    then Npkcontext.report_error "Firstpass.cast" "incompatible type";
	    (e, t1)
      | _ -> 
	  let t1' = translate_typ t1 in
	  let t2' = translate_typ t2 in
	    (C.cast (e, t1') t2', t2)

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
	    let (e, t) = cast (translate_exp e) t in
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
	
	| (Sequence seq, Comp (s, true)) ->
	    let f = fields_of_comp s in
	      translate_field_sequence o f seq;
	      t

	| (Sequence ((Some f, v)::[]), Comp (s, false)) ->
	    let r = fields_of_comp s in
	    let (f_o, f_t) = find_field f r in
	    let _ = translate (o + f_o) f_t v in
	      t
		
	| (Sequence _, _) -> 
	    Npkcontext.report_error "Firstpass.translate_init"
	      "this type of initialization not implemented yet"
	  
    and translate_field_sequence o fields seq =
      match (fields, seq) with
	  ((fname, (f_o, t))::fields, (expected_f, hd)::seq) ->
	    let f_o = o + f_o in
	    let _ = 
	      match expected_f with
		  Some f when fname <> f ->
		    Npkcontext.report_error 
		      "Firstpass.translate_field_sequence" 
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
	    Npkcontext.report_error 
	      "Firstpass.translate_init.translate_sequence" 
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
		Npkcontext.report_warning 
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
	    let (e, t) = cast (translate_exp (exp_of_int 0)) t in
	      res := (o, translate_typ t, e)::!res
	| Float _ -> 
	    res := (o, translate_typ t, C.exp_of_float 0.)::!res
	| Array (t, n) ->
	    let n = 
	      match translate_array_len n with
		  Some n -> n
		| None -> 
		    Npkcontext.report_error 
		      "Firstpass.translate_init.fill_with_zeros"
		      "unreachable statement"
	    in
	    let sz = size_of t in
	    let o = ref o in
	      for i = 0 to n - 1 do
		fill_with_zeros !o t;
		o := !o + sz
	      done
		
	| Comp (s, _) -> 
	    let f = fields_of_comp s in
	    let fill_field (_, (f_o, t)) = fill_with_zeros (o + f_o) t in
	      List.iter fill_field f

	| _ -> 
	    Npkcontext.report_error "Firstpass.translate_init.fill_with_zeros"
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
      if not (Hashtbl.mem used_globals name) then begin
	let loc = Npkcontext.get_loc () in
	  declare_global false false name loc t (Some (Data (Str str)))
      end;
      (C.Global name, t)
 
  and is_array e =
    match e with
	Call _ -> false
      | Cast (_, Array _) -> true
      | Cast (_, _) -> false
      | _ -> 
	  let (_, t) = translate_lv e in
	    match t with
		Array _ -> true
	      | _ -> false

  and translate_lv x =
    match x with
	Var x -> find_var x

      | Field (lv, f) -> 
	  let (lv, t) = translate_lv lv in
	  let r = fields_of_comp (CoreC.comp_of_typ t) in
	  let (o, t) = find_field f r in
	  let o = C.exp_of_int o in
	    (C.Shift (lv, o), t)

      | Index (e, idx) when is_array e ->  (* TODO: think about this is_array, 
					      a bit hacky!! *)
	  let (lv, t) = translate_lv e in
	  let (t, len) = CoreC.array_of_typ t in
	  let i = translate_exp idx in
	  let n = translate_array_len len in
	    translate_array_access (lv, t, n) i

      | Index (e, idx) -> translate_lv (Deref (Binop (Plus, e, idx)))
	  
      | Deref e -> deref (translate_exp e)

      | OpExp (op, lv, is_after) ->
	  let loc = Npkcontext.get_loc () in
	  let e = Cst (C.CInt (Nat.of_int 1), CoreC.int_typ) in
	  let (incr, t) = translate_set (lv, Some op, e) in
	  let (lv, _, _) = incr in
	    (C.BlkLv ((C.Set incr, loc)::[], lv, is_after), t)

      | Str str -> add_glb_cstr str

      | FunName -> add_glb_cstr !current_fun

      | Cast (lv, t) -> 
	  Npkcontext.report_accept_warning "Firstpass.translate_stmt" 
	    "cast of left value" Npkcontext.DirtySyntax;
	  let (lv, _) = translate_lv lv in
	    (lv, t)

      | BlkExp blk -> 
	  let (body, (e, t)) = translate_blk_exp blk in
	  let lv =
	    match e with
		C.Lval (lv, _) -> lv
	      | _ -> 
		  Npkcontext.report_error "Firstpass.translate_lv" 
		    "left value expected"
	  in
	    (C.BlkLv (body, lv, false), t)

      | _ -> 
	  Npkcontext.report_error "Firstpass.translate_lv" "left value expected"

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
	    
	| Var _ | Field _ | Index _ | Deref _ | OpExp _ | Str _ | FunName -> 
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
		      Npkcontext.report_error "Firstpass.translate_lv" 
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
		(C.Pref (decl::set, C.Lval (v, translate_typ t)), t)
	  end
	    
	| SizeofE e ->
	    let (_, t) = translate e in
	    let sz = (size_of t) / 8 in
	      (C.exp_of_int sz, CoreC.uint_typ)
		
	| Sizeof t -> 
	    let sz = (size_of t) / 8 in
	      (C.exp_of_int sz, CoreC.uint_typ)
		
	| Cast (e, t) -> 
	    let e = translate_exp e in
	      cast e t
		
	| Call (Var x, args) when is_fname x -> 
	    let (f, (tmp_args_t, ret_t)) = find_fname x in
	    let args_t = refine_args_t tmp_args_t args in
	    let (args, args_t) = translate_args args args_t in
	    let ft' = translate_ftyp (args_t, ret_t) in
	      if tmp_args_t = None then update_funtyp x (Some args_t, ret_t); 
	      (C.Call (ft', C.Fname f, args), ret_t)
	      
	| Call ((Deref e | e), args) -> 
	    let (e, t) = translate_exp e in
	    let (args_t, ret_t) =
	      match t with
		  Ptr (Fun t) -> t
		| _ -> 
		    Npkcontext.report_error "Firstpass.translate_call"
		      "function pointer expected"
	    in
	    let args_t = refine_args_t args_t args in
	    let (args, args_t) = translate_args args args_t in
	    let ft' = translate_ftyp (args_t, ret_t) in
	      (C.Call (ft', C.FunDeref e, args), ret_t)
		
	| Set set ->
	    Npkcontext.report_accept_warning "Firstpass.translate_exp" 
	      "assignment within expression" Npkcontext.DirtySyntax;
	    let loc = Npkcontext.get_loc () in
	    let (set, t) = translate_set set in
	    let (lv', t', _) = set in
	    let e = C.Lval (lv', t') in
	      (C.Pref ((C.Set set, loc)::[], e), t)
	      		  
	| BlkExp blk -> 
	    let (body, (e, t)) = translate_blk_exp blk in
	      (C.Pref (body, e), t)

	| Offsetof (t, f) -> 
	    let r = fields_of_comp (CoreC.comp_of_typ t) in
	    let (o, _) = find_field f r in
	    let o = C.exp_of_int (o / Config.size_of_byte) in
	      (o, CoreC.uint_typ)
    in
      match translate e with
	  (C.Lval (lv, _), (Array (t', _) as t)) -> 
	    let (e, _) = addr_of (lv, t) in
	      (e, Ptr t')
	| (C.Lval (lv, _), (Fun _ as t)) -> 
	    let (e, _) = addr_of (lv, t) in
	      (e, Ptr t)
	| v -> v

  and refine_args_t args_t args =  
    match args_t with  
        None ->   
          Npkcontext.report_accept_warning "Firstpass.refine_args_t"  
            "unknown arguments type at function call"   
            Npkcontext.PartialFunDecl;  
          let infer_typ i e =  
	    let (_, t) = translate_exp e in  
	      (t, "arg"^(string_of_int i))  
	  in
	    List_utils.mapi infer_typ args  
      | Some args_t -> args_t  	      
      
  and deref (e, t) =
    match t with
	Ptr t -> (C.Deref (e, translate_typ t), t)
      | _ -> 
	  Npkcontext.report_error "Firstpass.deref_typ" "pointer type expected"
	    
(* TODO: code cleanup: get rid of call to length_of_array in cir2npkil 
   with AddrOf and put it in here *)
  and addr_of (e, t) = 
    let e =
      match (e, t) with
	  (C.Global f, Fun (Some args_t, ret_t)) -> 
	    C.AddrOfFun (f, translate_ftyp (args_t, ret_t))
	| (_, Fun (None, _)) -> 
	    Npkcontext.report_error "Firstpass.addr_of" 
	      "incomplete type for function"
	| _ -> C.AddrOf (e, translate_typ t)
    in
      (e, Ptr t)

  and translate_set (lv, op, e) =
    let (lv, t) = translate_lv lv in
    let e = translate_exp e in
    let t' = translate_typ t in
    let (lv, e) =
      match op with
	  None -> (lv, e)
	| Some op -> 
	    let (pref, lv', post) = C.normalize_lv lv in
	    let e' = (C.Lval (lv', t'), t) in
	    let e = translate_binop op e' e in
	    let lv = C.BlkLv (post, C.BlkLv (pref, lv', false), true) in
	      if (post <> []) then begin
		Npkcontext.report_warning "Firstpass.translate_set" 
		  "expression without post effects expected"
	      end;
	      (lv, e)
    in
    let (e, t) = cast e t in
    let set = (lv, t', e) in
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
	  ([], (Va_arg, id)::[]) ->
	    let (e, _) = cast (translate_exp (exp_of_int 0)) (Ptr char_typ) in
	      (e::[], (Va_arg, id)::[])
	| (_, (Va_arg, id)::[]) -> 
	    let (args, sz) = translate_va_args args in
	    let loc = Npkcontext.get_loc () in
	    let sz = if sz mod 8 = 0 then sz/8 else (sz/8)+1 in
	    let t = Array (char_typ, Some (exp_of_int sz)) in
	    let (_, decl, v) = gen_tmp loc t in
	    let (e, _) = addr_of (v, t) in
	    let init = init_va_args loc v args in
	      ((C.Pref (decl::init, e))::[], (Va_arg, id)::[])

	| (e::args, (t, id)::args_t) ->
	    let (e, t) = cast (translate_exp e) t in
	    let (args, args_t) = translate_args args args_t in
	      (e::args, (t, id)::args_t)
	| ([], []) -> ([], [])
	| _ -> 
	    Npkcontext.report_error "Firstpass.translate_exp" 
	      "different types at function call"
    in
      translate_args args args_t

  and gen_tmp loc t =
    let x = "tmp"^(string_of_int !tmp_cnt) in
      add_var (t, x);
      let t = translate_typ t in
      let decl = (C.Decl (t, x), loc) in
	incr tmp_cnt;
	(x, decl, C.Local x)

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
		  Npkcontext.report_error "Firstpass.process_struct_fields"
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
	  Npkcontext.report_error "Firstpass.process_struct_fields" 
	    "invalid size for structure"
	end;
	o := o'+sz;
	last_align := max !last_align cur_align;
	(x, (o', t))
    in
    let f = List.map translate f in
    let sz = next_aligned !o !last_align in
    let data = (CompSymb (f, sz, !last_align), Comp (name, true)) in
      Symbtbl.bind symbtbl name data

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
    let data = (CompSymb (f, !n, !align), Comp (name, false)) in
      Symbtbl.bind symbtbl name data

  and translate_scalar_typ t =
    match t with
      | Int k -> N.Int k
      | Float n -> N.Float n	
      | Ptr (Fun _) -> N.FunPtr
      | Ptr _ -> N.Ptr
      | Va_arg -> N.Ptr
      | Typeof v -> 
	  let (_, t) = find_var v in
	    translate_scalar_typ t
      | _ -> 
	  Npkcontext.report_error "Firstpass.translate_scalar_typ" 
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
      | Comp (s, is_struct) ->
	  let (f, sz, _) = find_compdef s in
	  let f = List.map translate_field f in
	    if is_struct then C.Struct (f, sz) else C.Union (f, sz)
      | Typeof v -> 
	  let (_, t) = find_var v in
	    translate_typ t
      | Bitfield _ -> 
	  Npkcontext.report_error "Firstpass.translate_typ" 
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
	      Npkcontext.report_error "Firstpass.translate_typ" 
		"invalid size for array"
	  in
	    if (i < 0) || (i > Config.max_array_length) then begin
(* TODO: should print the expression e?? *)
	      Npkcontext.report_error "Firstpass.translate_typ" 
		"invalid size for array"
	    end;
	    if (i = 0) then begin
	      Npkcontext.report_error "Firstpass.translate_typ" 
		"array should have at least 1 element"
	    end;
	    Some i

  and translate_ftyp (args, ret) =
    let translate_arg (t, _) = translate_typ t in
    let args = List.map translate_arg args in
    let ret = translate_typ ret in
      (args, ret)

  and add_enum (x, v) =
    let v = translate_exp v in
    let (v, _) = cast v CoreC.int_typ in
    let v = 
      try C.Const (C.CInt (C.eval_exp v)) 
      with Invalid_argument _ -> v
    in
      Symbtbl.bind symbtbl x (EnumSymb v, CoreC.int_typ)

  and add_compdecl (x, (is_struct, f)) =
    if is_struct then process_struct_fields x f
    else process_union_fields x f

  and translate_blk x =
    Symbtbl.save symbtbl;
    let (body, _) = translate_blk_aux false x in
      Symbtbl.restore symbtbl;
      body

  and translate_blk_exp x =
    let (body, e) = translate_blk_aux true x in
      match e with
	  Some e -> (body, e)
	| None -> 
	    Npkcontext.report_error "Firstpass.translate_blk_exp" 
	      "expression expected"

  and translate_decl loc x d =
    match d with
	EDecl e -> add_enum (x, e)
      | CDecl d -> add_compdecl (x, d)
      | VDecl (_, _, extern, Some _) when extern -> 
	  (* TODO: make a test for this case in local *)
	  Npkcontext.report_error "Firstpass.translate_global"
	    "extern globals can not be initizalized"
      | VDecl (_, static, extern, _) when static && extern -> 
	  (* TODO: make a test for this case in local *)
	  Npkcontext.report_error "Firstpass.translate_global"
	    ("static variable can not be extern")
      | VDecl (Fun _, _, _, Some _) -> 
	  (* TODO: make a test for this case in local *)
	  Npkcontext.report_error "Firstpass.translate_global"
	    ("unexpected initialization of function "^x)
      | VDecl (Fun ft, static, _, None) -> update_funsymb x static ft loc
      | _ -> ()

  and translate_local_decl loc x d =
    translate_decl loc x d;
    match d with
	EDecl _ | CDecl _ -> []
      | VDecl (Fun _, _, _, _) -> 
	  Npkcontext.report_accept_warning "Firstpass.translate" 
	    "function declaration within block" Npkcontext.DirtySyntax;
	  []
      | VDecl (t, static, extern, init) when static || extern -> 
	  declare_global static extern x loc t init;
	  []
      | VDecl (t, _, _, init) ->
(* TODO: see if more can be factored with translate_global_decl *) 
	  add_var (t, x);
	  let (init, t) = 
	    match init with
		None -> ([], t)
	      | Some init -> translate_init t init
	  in
	    update_var_typ x t;
	    let v = C.Local x in
	    let build_set (o, t, e) =
	      let lv = C.Shift (v, C.exp_of_int o) in
		(C.Set (lv, t, e), loc)
	    in
	    let init = List.map build_set init in
	    let decl = (C.Decl (translate_typ t, x), loc) in
	      decl::init
		
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
	    
	| (LocalDecl (x, d), loc)::body -> 
	    Npkcontext.set_loc loc;
	    let decl = translate_local_decl loc x d in
	    let (body, e) = translate_blk_aux ends_with_exp body in
	      ((decl@body, []), e)

	(* TODO: do the case where suffix is <> [] *)
	(* TODO: remove body, suffix from For, use goto and labels
	   remove break. Use goto... *)
	| ((Label lbl, loc) as stmt)::tl -> 
	    let lbl = translate_lbl lbl in
	    let tl = 
	      match tl with
		  (For ([], e, body, []), loc)::tl ->
		    (For ([], e, body@(stmt::[]), []), loc)::tl
		| _ -> tl
	    in
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

      | DoWhile (body, e) when !Npkcontext.remove_do_loops ->
	  (* carefull: this transformation duplicates code and has an 
	     exponential behavior in the imbrication depths of do loops *)
	  let loop = For (body, e, body, []) in 
	    translate_stmt (loop, loc)

      | DoWhile (body, e) -> 
	  let body = translate_blk body in
	  let guard = translate_stmt (If (e, [], (Break, loc)::[]), loc) in
	  let body = (C.Block (body@guard, Some (cnt_lbl, [])), loc)::[] in
	    (C.Block ((C.Loop body, loc)::[], Some (brk_lbl, [])), loc)::[]

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

      | UserSpec x -> (C.UserSpec (translate_assertion x), loc)::[]

      | Label _ | LocalDecl _ -> 
	  Npkcontext.report_error "Firstpass.translate_stmt" "unreachable code"

  and translate_assertion x = List.map translate_token x

  and translate_token x =
    match x with
	SymbolToken x -> C.SymbolToken x
      | IdentToken x -> begin
	  try 
	    let (lv, t) = find_var x in begin
	      match t with
		  Fun _ -> C.IdentToken x
		| _ -> C.LvalToken lv
	      end
	  with _ -> C.IdentToken x
	end
(* TODO: not good, do this in compile phase *)
      | CstToken (c, _) -> C.CstToken c

(* TODO: think about this: simplify *)
  and translate_if loc (e, blk1, blk2) =
(* TODO: this is a bit of a hack!! *)
    let rec select (blk1, blk2) =
      match (blk1, blk2) with
	  (_::(C.Guard c, _)::_, blk) when C.exp_is_false c -> blk
	| (blk, _::(C.Guard c, _)::_) when C.exp_is_false c -> blk
	| ((C.Guard c, _)::_, blk) when C.exp_is_false c -> blk
	| (blk, (C.Guard c, _)::_) when C.exp_is_false c -> blk
	| (hd1::tl1, hd2::tl2) when hd1 == hd2 -> hd1::(select (tl1, tl2))
	| _ -> (C.Select (blk1, blk2), loc)::[]
    in
    let rec translate_guard e =
      match e with
	  IfExp (Cst (C.CInt c, _), t, _) when (Nat.compare c Nat.zero <> 0)-> 
	    translate_guard t
	| IfExp (Cst (C.CInt c, _), _, f) when (Nat.compare c Nat.zero = 0) -> 
	    translate_guard f
	| IfExp (IfExp (c, t1, f1), t2, f2) -> 
	    translate_guard (IfExp (c, IfExp (t1, t2, f2), IfExp (f1, t2, f2)))
	| IfExp (c, t, f) -> 
	    let (c, _) = translate_exp c in
	    let (pref, c, post) = C.normalize_exp c in
	    let guard_c = (C.Guard c, loc) in
	    let guard_not_c = (C.Guard (C.Unop (K.Not, c)), loc) in
	    let (t1, t2) = translate_guard t in
	    let (f1, f2) = translate_guard f in
	    let e1 = select (guard_c::post@t1, guard_not_c::post@f1) in
	    let e2 = select (guard_c::post@t2, guard_not_c::post@f2) in
	      (pref@e1, pref@e2)
	| Unop (Not, e) -> 
	    let (e1, e2) = translate_guard e in
	      (e2, e1)
	| e -> 
	    let (e, _) = translate_exp e in
	    let (pref, e, post) = C.normalize_exp e in
	    let guard_e = pref@(C.Guard e, loc)::post in
	    let guard_not_e = pref@(C.Guard (C.Unop (K.Not, e)), loc)::post in
	      (guard_e, guard_not_e)
    in
    let (guard1, guard2) = translate_guard e in
    let blk1 = guard1@blk1 in
    let blk2 = guard2@blk2 in
      select (blk1, blk2)

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
    let cast e t = fst (cast e t) in
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
	    
	| (Minus, Ptr _, Ptr _) -> (N.MinusPP, CoreC.int_typ)
	    
	(* Integer comparisons *)
	(* Thanks to normalization t1 = t2 *)
	(* Function translate_scalar_typ will ensure they are both scalar 
	   types *)
	| (Gt, _, _) -> (N.Gt (translate_scalar_typ t1), CoreC.int_typ)
	| (Eq, _, _) -> (N.Eq (translate_scalar_typ t1), CoreC.int_typ)
	    	    
	| _ ->
	    Npkcontext.report_error "Firstpass.translate_binop" 
	      "unexpected binary operator and arguments"
    in
    let e2 = 
      match (op, t) with
	  (N.PlusPI, Ptr (Fun _)) ->
	    Npkcontext.report_error "Firstpass.translate_binop"
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
      | (Not, Int _, _) -> (C.Unop (K.Not, e), CoreC.int_typ)
      | (BNot, Int k, _) -> 
	  let k' = C.promote k in
	  let t' = Int k' in
	  let (e, t') = cast (e, t) t' in
	    (C.Unop (K.BNot (Newspeak.domain_of_typ k'), e), t')
      | _ -> 
	  Npkcontext.report_error "Firstpass.translate_unop" 
	    "Unexpected unary operator and argument"

  and size_of t = C.size_of_typ (translate_typ t)

  and align_of t =
    match t with
	Comp (n, _) -> align_of_comp n
      | Array (t, _) -> align_of t
      | Bitfield (k, _) -> align_of (Int k)
      | _ -> size_of t

(* TODO: use this function at all points where translate_glb_init is called
   + simplify code of global declaration!!! *)
  and declare_global static extern x loc t init =
    let name = if static then get_static_name x loc else x in
      update_global x name loc (t, None);
      let (t, init) = translate_glb_init t init in
      let init = if extern then None else Some init in
	update_global x name loc (t, init)
  in

  let translate_global (x, loc) =
    Npkcontext.set_loc loc;
    match x with
	FunctionDef (f, _, _, body) ->
	  current_fun := f;
	  let (f', ft) = find_fname f in
	  let ft = 
	    match ft with
		(Some args_t, ret_t) -> (args_t, ret_t)
	      | (None, _) -> 
		  Npkcontext.report_error "Firstpass.translate_global" 
		    "unreachable code"
	  in
	    Symbtbl.save symbtbl;
	    let formalids = add_formals ft in
	    let body = translate_blk body in
	    let body = (C.Block (body, Some (ret_lbl, [])), loc)::[] in
	      add_fundef f' formalids body (translate_ftyp ft);
	      Symbtbl.restore symbtbl;
	      current_fun := "";
	      Hashtbl.clear lbl_tbl;
	      lbl_cnt := default_lbl
	      
      | GlbDecl _ -> ()
  in
    
(* TODO: a tad hacky!! Think about it *)
(* TODO: could be done in the parser *)
(* TODO: should be done in csyntax2CoreC *)
  let collect_glbtyps (x, loc) =
    Npkcontext.set_loc loc;
    match x with
	FunctionDef (f, ft, static, _) -> update_funsymb f static ft loc

      | GlbDecl (x, d) -> 
	  translate_decl loc x d;
	  (* TODO: check if this could not be incorporated in translate_decl!!!
	  *)
	  match d with
	      VDecl (Fun _, _, _, _) -> ()
	    | VDecl (t, static, extern, init) -> 
		declare_global static extern x loc t init
	    | _ -> ()
  in

  let add_glbdecl name (t, loc, init) =
    Npkcontext.set_loc loc;
    try
      let t = translate_typ t in
	Hashtbl.add glbdecls name (t, loc, init)
    with _ -> 
      (* TODO: could at least print a warning here *)
      ()
  in

(* TODO: a tad inefficient *)
(* seems necessary because of 536.c and 540.c and 679.c 
   maybe should really think about this and be stricter
   so as to perform everything in one pass
   Or better: should do all typing first.
   Then compile.
*)
    List.iter collect_glbtyps globals;
    List.iter translate_global globals;
(* TODO: optimization: could remove this phase if cir had a type 
   structure of name 
   and all the structures' type were in a hashtbl *)
    Hashtbl.iter add_glbdecl used_globals;
    let spec = List.map translate_assertion spec in
      { C.globals = glbdecls; C.fundecs = fundefs; C.specs = spec }
