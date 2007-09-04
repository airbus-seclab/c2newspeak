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

(* TODO: code optimization: get rid of env, and use the globals Hashtbl
   rename it to env ! *)
open Lexing
open Csyntax

module N = Newspeak
module K = Npkil
(* TODO: code cleanup: break this file in several module. 
   do factorisation, when possible. In particular K.size_of  and align *)

(* TODO: code cleanup: think about this!! *)
(* TODO: check that integer don't have a default type (like int) *)
let kind_of_int64 i =
  let sign =
    if Int64.compare i (Int64.of_string "2147483647") > 0 
    then N.Unsigned else N.Signed
  in
    (sign, Config.size_of_int)

type ctyp =
    | CVoid
    | CScalar of cscalar_t
    | CArray of (ctyp * int option)
    | CRegion of (cfield list * int)

and cscalar_t =
    | CInt of N.ikind
    | CFloat of int
    | CPtr of ctyp
    | CFunPtr

and cfield = (string * int * ctyp)

let ret_vname = "!return"

let size_of_cscalar t = 
  match t with
      CInt (_, n) -> n
    | CFloat n -> n
    | CPtr _ -> Config.size_of_ptr
    | CFunPtr -> Config.size_of_ptr

let rec size_of t =
  match t with
      CScalar t -> size_of_cscalar t
    | CArray (t, Some n) -> (size_of t) * n
    | CRegion (_, n) -> n
    | CArray _ -> Npkcontext.error "Compiler.size_of" "unknown size of array"
    | CVoid -> Npkcontext.error "Compiler.size_of" "size of void unknown"

(* TODO: check this for various architecture ? 
   Here align everything on 4 *)
let align t o = 
  if (o mod 4) = 0 then o
  else if size_of t <= (4 - (o mod 4)) then o
  else o + (4 - (o mod 4))

let align_end o = align (CScalar (CInt (N.Signed, 4))) o

let rec offset_of f t =
  match t with
      (x, _, _)::tl when x <> f -> offset_of f tl
    | (_, o, t)::_ -> (o, t)
    | [] -> Npkcontext.error "Compiler.offset_of" ("field "^f^" not defined")
      

let int64_to_int x =
  if Int64.compare x (Int64.of_int max_int) > 0 
  then Npkcontext.error "Compiler.int64_to_int" "integer too big";
  if Int64.compare x (Int64.of_int max_int) > 0 
  then Npkcontext.error "Compiler.int64_to_int" "expecting positive integer";
  Int64.to_int x

let parse fname =
  let cin = open_in fname in
  let lexbuf = Lexing.from_channel cin in
    Lexer.init fname lexbuf;
    try
      let cprog = Parser.cprog Lexer.token lexbuf in
	close_in cin;
	cprog
    with Parsing.Parse_error -> 
      let pos = Lexing.lexeme_start_p lexbuf in
      let line_nb = string_of_int pos.pos_lnum in
      let lexeme = Lexing.lexeme lexbuf in
	Npkcontext.error "Parser.parse_error" 
	  ("syntax error: line "^line_nb^", unexpected token: "^lexeme)

(* TODO: code cleanup: remove this: remove any use of this *)
let dummy_loc = ("", -1, -1)

let rec typ_of_ctyp t =
  match t with
      CVoid -> Npkcontext.error "Compiler.typ_of_ctyp" "void not allowed here"
    | CScalar t -> K.Scalar (scalar_of_cscalar t)
    | CArray (t, sz) -> K.Array (typ_of_ctyp t, sz)
    | CRegion (f, sz) -> K.Region (List.map field_of_cfield f, sz)

and scalar_of_cscalar t = 
  match t with
      CInt i -> N.Int i
    | CFloat n -> N.Float n
    | CPtr _ -> N.Ptr
    | CFunPtr -> N.FunPtr

and field_of_cfield (n, o, t) = (o, typ_of_ctyp t)

let ret_typ_of_ctyp t =
  match t with
      CVoid -> None
    | _ -> Some (typ_of_ctyp t)

let cscalar_of_ctyp t = 
  match t with 
      CScalar t -> t
    | _ -> Npkcontext.error "Compiler.scalar_of_typ" "scalar type expected"

let region_of_ctyp t =
  match t with
      CRegion (fields, _) -> fields
    | _ -> 
	Npkcontext.error "Compiler.region_of_typ" 
	  "struct or union type expected"

let array_of_ctyp lv t =
  match (t, lv) with
      (CArray (t, Some n), _) -> (t, K.Known n)
    | (CArray (t, None), K.Global x) -> (t, K.Length x)
    | _ -> 
	Npkcontext.error "Compiler.array_of_typ" "Array of known size expected"

let deref_ctyp t =
  match t with
      CPtr t -> t
    | _ -> Npkcontext.error "Compiler.deref_ctyp" "pointer type expected"

let translate_sign s =
  match s with
      Signed -> N.Signed
    | Unsigned -> N.Unsigned

let translate_ityp s t =
  match t with
      Char -> CInt (s, Config.size_of_char)
    | Int -> CInt (s, Config.size_of_int)

let rec translate_var_modifier b v =
  match v with
      Variable x -> (b, x)
    | FunctionName x -> 
	Npkcontext.error "Compiler.translate_var_modifier" 
	  "local function definition not allowed"
    | Array (v, n) -> 
	let n = Some (int64_to_int n) in
	  translate_var_modifier (CArray (b, n)) v
    | FunctionProto (Pointer v, _) -> 
	translate_var_modifier (CScalar CFunPtr) v
    | Pointer v -> translate_var_modifier (CScalar (CPtr b)) v
    | FunctionProto _ -> 
	Npkcontext.error "Compiler.translate_var_modifier" 
	  "case not implemented yet"

let rec translate_decl d =
  match d with
      Declaration (b, v, loc) -> 
	let b = translate_base_typ b in
	let (t, x) = translate_var_modifier b v in
	  (t, x, loc)
    | _ -> 
	Npkcontext.error "Compiler.translate_decl" 
	  "Unexpected function definition"

and translate_base_typ t =
  match t with
      Integer (s, t) -> CScalar (translate_ityp (translate_sign s) t)
    | Struct f -> CRegion (translate_struct_fields f)
    | Union f -> CRegion (translate_union_fields f)
    | Void -> CVoid

and translate_struct_fields f =
  let rec translate o f =
    match f with
	d::f -> 
	  let (t, x, _) = translate_decl d in
	  let sz = size_of t in
	  let o = align t o in
	  let (f, n) = translate (o+sz) f in
	    ((x, o, t)::f, n)
      | [] -> 
	  let o = align_end o in
	    ([], o)
  in
    match f with
	d::[] -> 
	  let (t, x, _) = translate_decl d in
	  let sz = size_of t in
	    ((x, 0, t)::[], sz)
      | _ -> translate 0 f

and translate_union_fields f =
  let n = ref 0 in
  let translate d =
    let (t, x, _) = translate_decl d in
    let sz = size_of t in
      if !n < sz then n := sz;
      (x, 0, t)
  in
    (List.map translate f, !n)
	    
let rec translate_fun_decl (b, v) =
  let rec translate b v =
    match v with
	Variable x -> 
	  Npkcontext.error "Compiler.translate_fun_decl" "unreachable code"
      | FunctionName f -> (b, f)
      | Array _ -> 
	  Npkcontext.error "Compiler.translate_fun_decl" 
	    "function can not return array"
      | FunctionProto (Pointer v, _) -> 
	  Npkcontext.error "Compiler.translate_fun_decl" 
	    "function can not return function pointer"
      | Pointer v -> translate (CScalar (CPtr b)) v
      | FunctionProto _ -> 
	  Npkcontext.error "Compiler.translate_var_modifier" 
	    "case not implemented yet"
  in
    translate (translate_base_typ b) v

let get_ret_lbl () = 0

(* TODO: code cleanup: this could be in npkil and also used by cilcompiler ? *)
let cast t e t' =
  match (t, t') with
      _ when t = t' -> e
    | (N.Int _, N.Int k) -> K.make_int_coerce k e
    | (N.Int _, N.Ptr) when e = K.Const (N.CInt64 Int64.zero) -> K.Const N.Nil

    | (N.Ptr, N.Int k) when !Npkcontext.castor_allowed -> 
	Npkcontext.print_warning "Compiler.cast"
	  ("Probable invalid cast "^(K.string_of_cast t t'));
	K.UnOp (K.PtrToInt k, e)
    | _ -> 
	Npkcontext.error "Compiler.cast"
	  ("Invalid cast "^(K.string_of_cast t t'))

let translate_binop op (e1, t1) (e2, t2) =
  match (op, t1, t2) with
      (Plus, CInt k1, CInt k2) when k1 = k2 -> 
	(K.make_int_coerce k1 (K.BinOp (N.PlusI, e1, e2)), t1)
    | (Mult, CInt k1, CInt k2) when k1 = k2 -> 
	(K.make_int_coerce k1 (K.BinOp (N.MultI, e1, e2)), t1)
    | (Plus, CPtr _, CInt _) ->	
	let stride = K.Const (N.CInt64 (Int64.of_int Config.size_of_ptr)) in 
	  (K.BinOp (N.PlusPI, e1, K.BinOp (N.MultI, e2, stride)), t1)
(* TODO: clean bug ? maybe a cast is necessary ? *)
    | (Gt, _, _) -> 
	let t = CInt (N.Signed, Config.size_of_int) in
	  (K.BinOp (N.Gt (scalar_of_cscalar t1), e1, e2), t)
    
    | _ -> 
	Npkcontext.error "Compiler.translate_binop" 
	  "unexpected binary operator and arguments"

let rec append_decls d body =
  match d with
      (x, t, loc)::tl -> 
	let t = typ_of_ctyp t in
	  (K.Decl (x, t, append_decls tl body), loc)::[]
    | [] -> body


let compile fname = 
  let globals = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in
  let global_env = Hashtbl.create 100 in
  let env = Hashtbl.create 100 in
  let vcnt = ref 0 in
  let push x t loc =
    incr vcnt;
    Hashtbl.add env x (!vcnt, t, loc)
  in
  let pop x =
    vcnt := !vcnt - 1;
    Hashtbl.remove env x
  in
  let get_var x =
    try 
      let (n, t, _) = Hashtbl.find env x in
	(K.Local (!vcnt - n), t)
    with Not_found -> 
      try
	let t = Hashtbl.find global_env x in
	  (K.Global x, t)
      with Not_found ->
	Npkcontext.error "Compiler.get_var" ("Variable "^x^" not declared")
  in
  let get_ret_var () = 
    try
      let (n, t, _) = Hashtbl.find env ret_vname in
	(K.Local (!vcnt - n), t)
    with Not_found -> 
	Npkcontext.error "Compiler.get_ret_var" 
	  ("function does not return a value")
  in
  let get_locals () =
    let res = ref [] in
    let get_var x (i, t, loc) = res := (i, (x, t, loc))::!res in
      Hashtbl.iter get_var env;
      Hashtbl.clear env;
      let res = List.sort (fun (i, _) (j, _) -> compare i j) !res in
      let (_, res) = List.split res in
	res
  in
    
  let rec translate_lv lv =
    match lv with
	Var x -> get_var x

      | Field (lv, f) ->
	  let (lv, t) = translate_lv lv in
	  let r = region_of_ctyp t in
	  let (o, t) = offset_of f r in
	  let o = K.Const (N.CInt64 (Int64.of_int o)) in
	    (K.Shift (lv, o), t)

      | Index (lv, e) -> 
	  let (lv, t) = translate_lv lv in
	  let (t, n) = array_of_ctyp lv t in
	  let (i, _) = translate_exp e in
	  let sz = size_of t in
	  let sz = K.Const (N.CInt64 (Int64.of_int sz)) in
	  let o = K.UnOp (K.Belongs_tmp (Int64.zero, K.Decr n), i) in
	  let o = K.BinOp (N.MultI, o, sz) in
	    (K.Shift (lv, o), t)

      | Deref e ->
	  let (e, t) = translate_exp e in
	  let t = deref_ctyp t in
	    (K.Deref (e, size_of t), t)

  and translate_exp e =
    match e with
	Const i -> (K.Const (N.CInt64 i), CInt (kind_of_int64 i))

      | Lval lv -> 
	  let (lv, t) = translate_lv lv in
	  let t = cscalar_of_ctyp t in
	    (K.Lval (lv, scalar_of_cscalar t), t)

      | AddrOf Index (lv, e) ->
	  let (lv, t) = translate_lv lv in
	  let (i, _) = translate_exp e in
	  let (t', n) = array_of_ctyp lv t in
	  let sz = size_of t' in
	  let sz_e = K.Const (N.CInt64 (Int64.of_int sz)) in
	  let o = K.UnOp (K.Belongs_tmp (Int64.zero, n), i) in
	  let o = K.BinOp (N.MultI, o, sz_e) in
	  let n = K.Mult (n, sz) in
	    (K.BinOp (N.PlusPI, K.AddrOf (lv, n), o), CPtr t)

      | AddrOf lv ->
	  let (lv, t) = translate_lv lv in
	  let sz = size_of t in
	    (K.AddrOf (lv, K.Known sz), CPtr t)

      | Binop (op, e1, e2) ->
	  let v1 = translate_exp e1 in
	  let v2 = translate_exp e2 in
	    translate_binop op v1 v2
  in

  let rec translate_blk (decls, body) =
    match decls with
	[] -> translate_stmt_list body
      | d::tl ->
	  let (t, x, loc) = translate_decl d in
	    push x t loc;
	    translate_blk (tl, body)

  and translate_stmt_list x =
    match x with
	hd::tl -> (translate_stmt hd)@(translate_stmt_list tl)
      | [] -> []

  and translate_stmt (x, loc) =
    Npkcontext.set_loc loc;
    match x with
      | Set (lv, e) -> 
	  (* TODO: code cleanup *)
	  let (lv, t) = translate_lv lv in
	  let (e, t') = translate_exp e in
(* TODO: code cleanup: put these two together ?? *)
	  let t = scalar_of_cscalar (cscalar_of_ctyp t) in
	  let t' = scalar_of_cscalar t' in
	  let e = cast t' e t in
	    (K.Set (lv, e, t), loc)::[]

      | If (e, body) ->
	  let (cond1, _) = translate_exp e in
	  let cond2 = K.negate cond1 in
	  let body = translate_blk body in
	    (K.ChooseAssert [([cond1], body); ([cond2], [])], loc)::[]

      | Return e -> 
	  (* TODO: code cleanup *)
	  let (lv, t) = get_ret_var () in
	  let (e, t') = translate_exp e in
	  let t = scalar_of_cscalar (cscalar_of_ctyp t) in
	  let t' = scalar_of_cscalar t' in
	  let e = cast t' e t in
	  let lbl = get_ret_lbl () in
	    (K.Set (lv, e, t), loc)::(K.Goto lbl, loc)::[]
  in

  let translate_global x =
    match x with
	FunctionDef (b, v, loc, body) -> 
	  let (t, f) = translate_fun_decl (b, v) in
	    push ret_vname t loc;
	    let body = translate_blk body in
	      pop ret_vname;
	      let t = ret_typ_of_ctyp t in
	      let body = (K.DoWith (body, get_ret_lbl (), []), loc)::[] in
	      let decls = get_locals () in
	      let body = append_decls decls body in
		Hashtbl.add fundefs f ([], t, Some body) 
	      
      | Declaration _ -> 
	  let (t, x, loc) = translate_decl x in
	    Hashtbl.add global_env x t;
	    Hashtbl.add globals x (typ_of_ctyp t, loc, Some None, true)
  in

  let cprog = parse fname in
    
    List.iter translate_global cprog;
    (fname, globals, fundefs)
