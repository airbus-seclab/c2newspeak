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

(* Constants *)
let ret_name = "!return"
let tmp_name = "!tmp"

let ret_lbl = 0
let cnt_lbl = 1
let brk_lbl = 2
let default_lbl = 3

(* types *)
type symb =
    | VarSymb of C.lv 
    | Enum of C.exp

(* functions *)
(* TODO: put in cir.ml *)
let translate_array lv n =
  match (n, lv) with
      (Some n, _) -> K.Known n
	(* TODO: there may be a problem if x is a function ?? *)
    | (_, C.Global x) -> K.Length x
    | _ -> 
	Npkcontext.error "Firstpass.translate_array" "Unknown array length"

let rec simplify_bexp e =
  match e with
      Var _ | Field _ | Index _ | Deref _ | Call _ | ExpPlusPlus _ -> 
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

let translate_cst c = (C.Const c, C.typ_of_cst c)

let translate_arithmop op (e1, k1) (e2, k2) =
  let k = Newspeak.max_ikind (C.promote k1) (C.promote k2) in
  let t = C.Int k in
    (op k, C.cast (e1, C.Int k1) t, C.cast (e2, C.Int k2) t, t)

let translate_floatop op (e1, n1) (e2, n2) =
  let n = max n1 n2 in
  let t = C.Float n in
    (op n, C.cast (e1, C.Float n1) t, C.cast (e2, C.Float n2) t, t)

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

      | (Minus, C.Ptr _, C.Ptr _) -> (C.MinusP, e1, e2, C.int_typ)
	  
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
      | (Eq, C.Ptr _, C.Ptr _) -> (C.Eq t1, e1, e2, C.int_typ)
      | (Eq, C.Int _, C.Ptr _) ->
	  let e1 = C.cast (e1, t1) t2 in
	    (C.Eq t2, e1, e2, C.int_typ)
      | (Eq, C.Ptr _, C.Int _) ->
	  let e2 = C.cast (e2, t2) t1 in
	    (C.Eq t1, e1, e2, C.int_typ)
	      
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
    | (Not, C.Int _, _) -> (C.Unop (C.Not, e), C.int_typ)
    | (BNot, C.Int k, _) -> 
	let k' = C.promote k in
	let t' = C.Int k' in
	  (C.Unop (C.BNot k', C.cast (e, t) t'), t')
    | _ -> 
	Npkcontext.error "Csyntax.translate_unop" 
	  "Unexpected unary operator and argument"


(*
   Sets scope of variables so that no goto escapes a variable declaration
   block
*)
let translate (bare_compdefs, globals) =
  let compdefs = Hashtbl.create 100 in
  let glbdecls = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in
    
  let symbtbl = Hashtbl.create 100 in
  (* Used to generate static variables names *)
  let current_fun = ref "" in

  let tmp_cnt = ref 0 in

  let add_var loc (t, x) =
    let id = C.fresh_id () in
    let decl = (C.Decl (t, x, id), loc) in
      Hashtbl.add symbtbl x (VarSymb (C.Var id), t, loc);
      (decl, id)
  in

  let gen_tmp loc t =
    let x = "tmp"^(string_of_int !tmp_cnt) in
    let (decl, id) = add_var loc (t, x) in
      incr tmp_cnt;
      (x, decl, C.Var id)
  in

  let remove_symb x = Hashtbl.remove symbtbl x in

  let add_formals loc (args_t, _, ret_t) =
    let add_var x = 
      let (_, id) = add_var loc x in
	id
    in
    let ret_id = add_var (ret_t, ret_name) in
    let args_id = List.map add_var args_t in
      (ret_id, args_id)
  in
    
  let remove_formals (args_t, _, ret_t) = 
    remove_symb ret_name;
    List.iter (fun (_, x) -> remove_symb x) args_t
  in

  let find_symb x = 
    try Hashtbl.find symbtbl x
    with Not_found -> 
      Npkcontext.error "Firstpass.translate.typ_of_var" 
	("Unknown identifier "^x)
  in

  let update_global x name loc (t, init) =
    let v = VarSymb (C.Global name) in
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
	  Hashtbl.replace symbtbl x (v, t, loc);
	  Hashtbl.replace glbdecls name (t, loc, init)	  
    with Not_found -> 
      Hashtbl.add symbtbl x (v, t, loc);
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
    let name = prefix^x in
      update_global x name loc d
  in

  let push_enum (x, i) loc = 
    let (pref, i, post) = C.normalize_exp i in
      if (pref <> []) || (post <> []) then begin
	Npkcontext.error "Firstpass.push_enum" 
	  "expression without side-effects expected"
      end;
      let i = C.make_int_coerce C.int_kind i in
	Hashtbl.add symbtbl x (Enum i, C.int_typ, loc) 
  in

  let update_funtyp f t loc =
    try
      let (prev_t, _, _) = Hashtbl.find fundefs f in
	if not (C.ftyp_equal t prev_t) then begin
	  Npkcontext.error "Firstpass.update_fundef"
	    ("Different types for function "^f)
	end
    with Not_found ->
      Hashtbl.add symbtbl f (VarSymb (C.Global f), C.Fun t, loc);
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

  let rec translate_init t x =
    let res = ref [] in
    let rec translate o t x =
      match (x, t) with
	  (Data (Str str), C.Array _) -> 
	    let seq = seq_of_string str in
	      translate o t (Sequence seq)
		
	| (Data e, _) -> 
	    let e = C.cast (translate_exp e) t in
	      res := (o, t, e)::!res;
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
    let a = (C.char_typ, Some ((String.length str) + 1)) in
    let t = C.Array a in
      if not (Hashtbl.mem glbdecls name) then begin
	let loc = ("", -1, -1) in
	let (t, init) = translate_glb_init t (Some (Data (Str str))) in
	  add_global name loc (t, Some init)
      end;
      (C.AddrOf (C.Shift (C.Global name, C.exp_of_int 0), t), C.Ptr C.char_typ)
  
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
	  let r = C.fields_of_typ compdefs t in
	  let (o, t) = List.assoc f r in
	  let o = C.exp_of_int o in
	    (C.Shift (lv, o), t)

      | Index (lv, e) -> 
	  let (lv', t) = translate_lv lv in begin
	    match t with
		C.Array (t, n) ->
		  let (i, _) = translate_exp e in
		  let n = translate_array lv' n in
		  let sz = C.exp_of_int (C.size_of compdefs t) in
		  let o = C.Unop (C.Belongs_tmp (Int64.zero, n), i) in
		  let o = C.Binop (C.MultI, o, sz) in
		    (C.Shift (lv', o), t)

	      | C.Ptr _ -> translate_lv (Deref (Binop (Plus, lv, e)))
	      | _ -> 
		  Npkcontext.error "Firstpass.translate_lv" 
		    "Array or pointer type expected"
	  end

      | Deref e -> C.deref (translate_exp e)

      | ExpPlusPlus lv ->
	  let (lv, t) = translate_lv lv in
	  let e = C.Lval (lv, t) in
	  let one = (C.exp_of_int 1, C.int_typ) in
	  let (incr_e, _) = translate_binop Plus (e, t) one in
	  let incr = (C.Set (lv, t, incr_e), Npkcontext.get_loc ()) in
	    (C.Post (lv, incr), t)

      | _ -> Npkcontext.error "Firstpass.translate_lv" "Left value expected"

  and translate_exp e = 
    match e with
	Cst i -> translate_cst i

      | Var x -> 
	  let (v, t, _) = find_symb x in
	  let e = 
	    match v with
		Enum i -> i
	      | VarSymb lv -> C.Lval (lv, t)
	  in
	    (e, t)
	      
      | Field _ | Index _ | Deref _ | ExpPlusPlus _ -> 
	  let (lv, t) = translate_lv e in
	    (C.Lval (lv, t), t)
	    
      | AddrOf (Deref _) -> 
	  Npkcontext.error "Firstpass.translate_exp" 
	    ("unecessary creation of a pointer from a dereference:"
	      ^" rewrite the code")

      | AddrOf (Index (lv, Cst (C.CInt i))) 
	  when Int64.compare i Int64.zero = 0 ->
	  let (lv', t) = translate_lv lv in begin
	    match t with
		C.Array (elt_t, _) -> (C.AddrOf (lv', t), C.Ptr elt_t)
	      | C.Ptr _ -> translate_exp (AddrOf (Deref lv))
	      | _ -> 
		  Npkcontext.error "Firstpass.translate_lv" 
		    "Array type expected"
	  end

      | AddrOf (Index (lv, e)) -> 
	  let base = AddrOf (Index (lv, exp_of_int 0)) in
	    translate_exp (Binop (Plus, base, e))
	    
      | AddrOf lv ->
	  let (lv, t) = translate_lv lv in
	    (C.AddrOf (lv, t), C.Ptr t)
	      
      | Unop (op, e) -> 
	  let e = translate_exp e in
	    translate_unop op e

      | Binop (op, e1, e2) -> 
	  let e1 = translate_exp e1 in
	  let e2 = translate_exp e2 in
	    translate_binop op e1 e2

(* TODO: there may be a need for a coercion here ??? int types ?? *)
      | IfExp (c, e1, e2) ->
	  let loc = Npkcontext.get_loc () in

	  let t = C.Int C.int_kind in
	  let (x, decl, v) = gen_tmp loc t in
	  let blk1 = (Exp (Set (Var x, e1)), loc)::[] in
	  let blk2 = (Exp (Set (Var x, e2)), loc)::[] in
	  let set = (If (c, blk1, blk2), loc) in

	  let set = translate_stmt set in
	    remove_symb x;
	    (C.Pref (decl::set, C.Lval (v, t)), t)

      | SizeofE (Str str) ->
	  let sz = String.length str + 1 in
	    (C.exp_of_int sz, C.int_typ)

      | SizeofE e ->
	  let (_, t) = translate_exp e in
	  let sz = (C.size_of compdefs t) / 8 in
	    (C.exp_of_int sz, C.int_typ)

      (* TODO: should check that the size of all declarations is less than max_int *)
      | Sizeof t -> 
	  let t = translate_typ t in
	  let sz = (C.size_of compdefs t) / 8 in
	    (C.exp_of_int sz, C.int_typ)

      | Str str -> add_glb_cstr str

      | Cast (e, t) -> 
	  let e = translate_exp e in
	  let t = translate_typ t in
	  let e = C.cast e t in
	    (e, t)

      | Call (f, args) -> 
	  let loc = Npkcontext.get_loc () in
	  let (f, ft) = C.funexp_of_lv (translate_lv f) in
	  let (args_t, _, ret_t) = ft in
	  let args = 
	      try List.map2 (translate_arg loc) args args_t 
	      with Invalid_argument "List.map2" ->
		Npkcontext.error "Firstpass.translate_exp" 
		  ("Different types at function call")
	  in
	    (C.Call (ft, f, args), ret_t)

      | Set _ -> 
	  Npkcontext.error "Firstpass.translate_exp" 
	    "assignments within expressions forbidden"

  and translate_arg loc e (t, _) = C.cast (translate_exp e) t 

  and translate_typ t =
    match t with
	Void -> C.Void
      | Int k -> C.Int k
      | Fun ft -> C.Fun (translate_ftyp ft)
      | Float n -> C.Float n
      | Ptr t -> C.Ptr (translate_typ t)
      | Array (t, len) -> 
	  let t = translate_typ t in
	  let len = 
	    match len with
		None -> None
	      | Some e -> 
		  let (e, _) = translate_exp e in
		  let i = C.len_of_exp e in
		    Some i
	  in
	    C.Array (t, len)
      | Struct n -> C.Struct n
      | Union n -> C.Union n
      | Bitfield _ -> 
	  Npkcontext.error "Firstpass.translate_typ" 
	    "bitfields not allowed outside of structures"
  
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

  and translate_local_decl (x, t, init) loc =
    Npkcontext.set_loc loc;
    let t = translate_typ t in
    let (init, t) = 
      match init with
	  None -> ([], t)
	| Some init -> translate_init t init
    in
    let (decl, id) = add_var loc (t, x) in
    let v = C.Var id in
    let build_set (o, t, e) =
      let lv = C.Shift (v, C.exp_of_int o) in
	(C.Set (lv, t, e), loc)
    in
    let init = List.map build_set init in
      decl::init

  and translate_enum (x, v) loc =
    let (v, _) = translate_exp v in
      push_enum (x, v) loc

  and translate_blk x = 
    match x with
      | (EDecl (x, v), loc)::body -> 
	  translate_enum (x, v) loc;
	  let body = translate_blk body in
	    remove_symb x;
	    body

      | (VDecl (x, t, static, init), loc)::body when static -> 
	  Npkcontext.set_loc loc;
	  let t = translate_typ t in
	  let (t, init) = translate_glb_init t init in
	    add_static x loc (t, Some init);
	    let body = translate_blk body in
	      remove_symb x;
	      body
	  
      | (VDecl (x, t, _, init), loc)::body -> 
	  let init = translate_local_decl (x, t, init) loc in
	  let body = translate_blk body in
	    remove_symb x;
	    init@body

      | hd::tl -> 
	  let hd = translate_stmt hd in
	  let tl = translate_blk tl in
	    hd@tl

      | [] -> []
  
  and translate_stmt (x, loc) = 
    Npkcontext.set_loc loc;
    match x with
      | Exp (Set (lv, e)) ->
	  let (lv, t) = translate_lv lv in
	  let e = C.cast (translate_exp e) t in
	    (C.Set (lv, t, e), loc)::[]

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

      | If (IfExp (c, e1, e2), blk1, blk2) ->
	  let if_e1_blk = (If (e1, blk1, blk2), loc)::[] in
	  let if_e2_blk = (If (e2, blk1, blk2), loc)::[] in
	    translate_stmt (If (c, if_e1_blk, if_e2_blk), loc)

      | If (Unop (Not, (IfExp _ as e)), blk1, blk2) ->
	  translate_stmt (If (e, blk2, blk1), loc)

      | If (e, blk1, blk2) ->
	  let e = simplify_bexp e in
	  let (e, _) = translate_exp e in
	  let blk1 = translate_blk blk1 in
	  let blk2 = translate_blk blk2 in
	    (C.If (e, blk1, blk2), loc)::[]

      | Block body -> (C.Block (translate_blk body, None), loc)::[]

      | For (init, e, body, suffix) ->
	  let init = (C.Block (translate_blk init, Some cnt_lbl), loc) in
	  let guard = translate_stmt (If (e, [], (Break, loc)::[]), loc) in
	  let body = translate_blk body in
	  let body = (C.Block (guard@body, Some cnt_lbl), loc) in
	  let suffix = translate_blk suffix in
	  let loop = (C.Loop (body::suffix), loc) in
	    (C.Block (init::loop::[], Some brk_lbl), loc)::[]

      | CSwitch (e, choices, default) -> 
	  let (e, _) = translate_exp e in
	  let (last_lbl, switch) = translate_switch choices in
	  let default_action = (C.Goto default_lbl, loc)::[] in
	  let switch = (C.Switch (e, switch, default_action), loc)::[] in
	  let body = translate_cases (last_lbl, switch) choices in
	  let default = translate_blk default in
(* TODO: have Newspeak optimization that removes duplicated lbls
   for instance, here when default (see 019.c) *)
	  let body = (C.Block (body, Some default_lbl), loc)::default in
	    (C.Block (body, Some brk_lbl), loc)::[]

      | VDecl _ | EDecl _ -> 
	  Npkcontext.error "Firstpass.translate_stmt"
	    "unreachable code"

  and translate_switch x =
    match x with
	(e, body, loc)::tl ->
	  let e = translate_exp e in
	  let (lbl, tl) = translate_switch tl in
	  let lbl = if body = [] then lbl else lbl+1 in
	    (lbl, (e, (C.Goto lbl, loc)::[])::tl)
      | [] -> (default_lbl, [])

  and translate_cases (lbl, body) x =
    match x with
	(_, [], _)::tl -> translate_cases (lbl, body) tl
      | (_, case, loc)::tl ->
	  let case = translate_blk case in
	  let body = (C.Block (body, Some lbl), loc)::case in
	    translate_cases (lbl-1, body) tl
      | [] -> body
  in

  let  translate_proto_ftyp f (args, va_list, ret) = 
    if args = [] then begin
      Npkcontext.print_warning "Firstpass.translate_proto_ftyp" 
	("Incomplete prototype for function "^f);
    end;
    translate_ftyp (args, va_list, ret)
  in

  let translate_global (x, loc) =
    Npkcontext.set_loc loc;
    match x with
	FunctionDef (x, t, body) ->
	  let t = translate_typ t in
	  let ft = C.ftyp_of_typ t in
	    update_funtyp x ft loc;
	    current_fun := x;
	    let formalids = add_formals loc ft in
	    let body = translate_blk body in
	    let body = (C.Block (body, Some ret_lbl), loc)::[] in
	      update_funbody x (formalids, body);
	      remove_formals ft;
	      current_fun := ""

      | GlbEDecl d -> translate_enum d loc

(* TODO: put this check in parser ?? *)
      | GlbVDecl ((_, _, _, Some _), is_extern) when is_extern -> 
	  Npkcontext.error "Firstpass.translate_global"
	    "Extern globals can not be initizalized"
 
      | GlbVDecl ((x, t, static, init), is_extern) ->
	  begin match (t, init) with
	      (Fun ft, None) -> 
		let ft = translate_proto_ftyp x ft in
		  update_funtyp x ft loc

	    | (Fun ft, Some _) -> 
		Npkcontext.error "Firstpass.translate_global"
		  ("Unexpected initialization of function "^x)
	    | _ -> 
		let t = translate_typ t in
		let (t, init) = translate_glb_init t init in
		  let init = if is_extern then None else Some init in
		    if static then add_static x loc (t, init)
		    else add_global x loc (t, init)
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
