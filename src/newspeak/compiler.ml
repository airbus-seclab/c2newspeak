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

(* TODO: should be in another file *)
open Cir

module N = Newspeak
module K = Npkil

let translate_scalar t =
  match t with
    | Int i -> N.Int i
    | Float n -> N.Float n
    | Ptr (Fun _) -> N.FunPtr
    | Ptr _ -> N.Ptr
    | Void -> Npkcontext.error "Compiler.translate_scalar" 
	"Value void not ignored as it ought to be"
    | _ -> 
	Npkcontext.error "Compiler.translate_scalar" 
	  "Unexpected non scalar type"

let translate_unop op e =
  match op with
      Belongs_tmp r -> K.UnOp (K.Belongs_tmp r, e)
    | Not -> K.negate e
    | BNot k -> K.UnOp (K.BNot (N.domain_of_typ k), e)
    | Cast (t, t') -> 
	let t = translate_scalar t in
	let t' = translate_scalar t' in
	  K.cast t e t'

let translate_arithmop op e1 e2 k = K.make_int_coerce k (K.BinOp (op, e1, e2))

let translate_binop compdefs op e1 e2 =
  match op with
      Mult k -> translate_arithmop N.MultI e1 e2 k
    | Plus k -> translate_arithmop N.PlusI e1 e2 k
    | Minus k -> translate_arithmop N.MinusI e1 e2 k
    | Div k -> translate_arithmop N.DivI e1 e2 k
    | MultF n -> K.BinOp (N.MultF n, e1, e2)
    | PlusF n -> K.BinOp (N.PlusF n, e1, e2)
    | MinusF n -> K.BinOp (N.MinusF n, e1, e2)
    | DivF n -> K.BinOp (N.DivF n, e1, e2)
    | BAnd k -> K.BinOp (N.BAnd (N.domain_of_typ k), e1, e2)
    | BXor k -> K.BinOp (N.BXor (N.domain_of_typ k), e1, e2)
    | BOr k -> K.BinOp (N.BOr (N.domain_of_typ k), e1, e2)
    | Mod -> K.BinOp (N.Mod, e1, e2)
    | Shiftl k -> K.make_int_coerce k (K.BinOp (N.Shiftlt, e1, e2))
    | Shiftr k -> K.make_int_coerce k (K.BinOp (N.Shiftrt, e1, e2))
    | PlusP (Fun _) -> 
	Npkcontext.error "Compiler.translate_binop" 
	  "pointer arithmetic forbidden on function pointers"
    | PlusP t -> 
	let stride = K.exp_of_int (size_of compdefs t) in 
	  K.BinOp (N.PlusPI, e1, K.BinOp (N.MultI, e2, stride))
    | MinusP -> K.make_int_coerce int_kind (K.BinOp (N.MinusPP, e1, e2))
	
    | Gt t -> 
	let t = translate_scalar t in
	  K.BinOp (N.Gt t, e1, e2)
	    
    | Eq t -> 
	let t = translate_scalar t in
	  K.BinOp (N.Eq t, e1, e2)

let translate_cst c =
  match c with
      CInt i -> N.CInt64 i
    | CFloat s -> 
	let c = 
	  try float_of_string s 
	  with Failure "float_of_string" -> 
	    Npkcontext.error "Compiler.translate_cst" "Float not representable"
	in
	  N.CFloat (c, s)

let translate (compdefs, cglbdecls, cfundefs) =
  let glbdecls = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in

  let stack_height = ref 0 in
  let env = Hashtbl.create 50 in
  let push id = 
    incr stack_height;
    Hashtbl.add env id !stack_height
  in
  let pop id = 
    decr stack_height;
    Hashtbl.remove env id
  in

  (* Hashtbl of already translated types, to as to have some sharing *)
  let translated_typ = Hashtbl.create 100 in

  let rec translate_typ t =
    try Hashtbl.find translated_typ t 
    with Not_found ->
      let t' =
	match t with
	    Void -> 
	      Npkcontext.error "Compiler.translate_typ" "Void not allowed here"
	  | Int _ | Float _ | Ptr _ -> K.Scalar (translate_scalar t)
	  | Array (t, sz) -> K.Array (translate_typ t, sz)
	  | Struct n | Union n -> 
	      let (fields, sz, _) = 
		try Hashtbl.find compdefs n 
		with Not_found -> 
		  Npkcontext.error "Compiler.translate_typ"
		    ("Unknown structure or union: "^n)
	      in
	      let translate_field (_, (o, t)) = (o, translate_typ t) in
		K.Region (List.map translate_field fields, sz)
	  | Fun _ -> 
	      Npkcontext.error "Compiler.translate_typ" 
		"Function not allowed here"
      in
	Hashtbl.add translated_typ t t';
	t'
  in

  let translate_ftyp (args, va_list, ret) =
    let va_list = if va_list then [K.Scalar N.Ptr] else [] in
    let args = List.map translate_typ args in
    let args = args@va_list in
    let ret =
      match ret with
	  Void -> None
	| _ -> Some (translate_typ ret)
    in
      (args, ret)
  in

  let rec translate_lv lv =
    match lv with
	Var id -> 
	  let x = Hashtbl.find env id in
	    K.Local (!stack_height - x)
      | Global x -> K.Global x
      | Shift (lv, o) ->
	  let lv = translate_lv lv in
	  let o = translate_exp o in
	    K.Shift (lv, o)

      | Deref (e, t) ->
	  let e = translate_exp e in
	  let sz = size_of compdefs t in
	    K.Deref (e, sz)

      | Post _ ->
	  Npkcontext.error "Compiler.translate_lval"
	    "unexpected side-effect in left value"

  and translate_exp e =
    match e with
	Const i -> K.Const (translate_cst i)
      | Lval (lv, t) -> 
	  let lv = translate_lv lv in
	    K.Lval (lv, translate_scalar t)

      | AddrOf (Global f, Fun _) -> K.AddrOfFun f

      | AddrOf (lv, (Array _ as t)) ->
	  let lv = translate_lv lv in
	  let t = translate_typ t in
	  let sz = K.size_of_array t lv in
	    K.AddrOf (lv, sz)

      | AddrOf (lv, t) ->
	  let lv = translate_lv lv in
	  let sz = size_of compdefs t in
	    K.AddrOf (lv, K.Known sz)

      | Unop (op, e) -> 
	  let e = translate_exp e in
	    translate_unop op e
      | Binop (op, e1, e2) ->
	  let e1 = translate_exp e1 in
	  let e2 = translate_exp e2 in
	    translate_binop compdefs op e1 e2

      | Pref _ -> 
	  Npkcontext.error "Compiler.translate_exp"
	    "unexpected side-effect in expression"

      | Call _ -> 
	  Npkcontext.error "Compiler.translate_exp"
	    "unexpected call in expression"	  
  in

  let rec translate_blk x = 
    match x with
	(Decl (t, x, id), loc)::body ->
	  Npkcontext.set_loc loc;
	  let t = translate_typ t in
	    push id;
	    let body = translate_blk body in
	      pop id;
	      (K.Decl (x, t, body), loc)::[]

      | hd::tl -> 
	  let hd = translate_stmt hd in
	  let tl = translate_blk tl in
	    hd@tl
      | [] -> []
  
  and translate_set (lv, t, e) =
    let lv = translate_lv lv in
      match (t, e) with
	  ((Struct _| Union _), Lval (lv', _)) -> 
	    let lv' = translate_lv lv' in
	    let sz = size_of compdefs t in
	      K.Copy (lv, lv', sz)
      | _ ->
	  let e = translate_exp e in
	  let t = translate_scalar t in
	    K.Set (lv, e, t)

  and translate_stmt (x, loc) =
    Npkcontext.set_loc loc;
    match x with
	Block (body, None) -> translate_blk body
      | Block (body, Some lbl) ->
	  let body = translate_blk body in
	    (K.DoWith (body, lbl, []), loc)::[]

      | Set (lv, _, Call c) ->
	  let call = translate_call loc (Some lv) c in
	    call::[]

      | Set x -> 
	  let set = translate_set x in
	    (set, loc)::[]

      | Goto lbl -> (K.Goto lbl, loc)::[]

      | If (e, body1, body2) ->
	  let cond1 = translate_exp e in
	  let body1 = translate_blk body1 in
	  let body2 = translate_blk body2 in begin
	    match cond1 with
(* TODO: remove this code and have it as a simplification for newspeak rather 
*)
		K.Const N.CInt64 i when Int64.compare i Int64.zero <> 0 -> 
		  body1
	      | K.Const N.CInt64 _ -> body2
	      | _ -> 
		  let cond2 = K.negate cond1 in
		    (K.ChooseAssert [([cond1], body1); 
				     ([cond2], body2)], loc)::[]
	  end

      | Loop body -> (K.InfLoop (translate_blk body), loc)::[]

      | Switch switch ->
	  let switch = translate_switch switch in
	    (switch, loc)::[]

      | Exp (Call c) -> 
	  let call = translate_call loc None c in
	    call::[]

      | Exp _ -> 
	  Npkcontext.error "Compiler.translate_stmt" 
	    "unexpected expression as statement"

      | Decl _ -> 
	  Npkcontext.error "Compiler.translate_stmt" "unreachable code"

  and append_args loc args f =
    let rec append x args =
      match args with
	  (e::args, t::args_t) ->
	    let id = fresh_id () in
	      push id;
	      let lv = Var id in
	      let set = translate_set (lv, t, e) in
	      let t = translate_typ t in
	      let call = append (x+1) (args, args_t) in
	      let arg = "arg"^(string_of_int x) in
		pop id;
		(K.Decl (arg, t, (set, loc)::call::[]), loc)
		  
	| _ -> 
	    let fn = translate_fn f in
	      (K.Call fn, loc)
    in
      append 1 args
      
  and translate_fn fn =
    match fn with
	Fname f -> K.FunId f
      | FunDeref (e, ft) -> 
	  let e = translate_exp e in
	  let ft = translate_ftyp ft in
	    K.FunDeref (e, ft)

  and translate_call loc ret ((args_t, va_list, ret_t), f, args) =
    if va_list then begin
      Npkcontext.error "Compiler.translate_exp"
	"functions with variable argument list not supported"
    end;
    let args = (args, args_t) in
      match ret_t with
	  Void -> append_args loc args f
	| _ ->
	    let t = translate_typ ret_t in
	    let id = fresh_id () in
	      push id;
	      let post = 
		match ret with
		    Some lv -> 
		      let e = Lval (Var id, ret_t) in
		      let set = translate_set (lv, ret_t, e) in
			(set, loc)::[]
		  | None -> []
	      in
	      let fid =
		match f with
		    Fname f -> f
		  | _ -> "fptr_call"
	      in
	      let call = append_args loc args f in
		pop id;
		(K.Decl ("Value_of_"^fid, t, call::post), loc)
		
  and translate_switch (e, cases, default) =
    let e = translate_exp e in
    let default = translate_blk default in
    let rec translate_cases default_cond x =
      match x with
	  ((v, t), body)::tl ->
	    let v = translate_exp v in
	    let t = translate_scalar t in
	    let cond = K.BinOp (Newspeak.Eq t, e, v) in
	    let body = translate_blk body in
	    let default_cond = (K.negate cond)::default_cond in
	    let choices = translate_cases default_cond tl in
	      (cond::[], body)::choices
	| [] -> (default_cond, default)::[]
    in
    let choices = translate_cases [] cases in
      K.ChooseAssert (choices)
  in
	  
  let translate_init (o, t, e) =
    let t = translate_scalar t in
    let e = translate_exp e in
      (o, t, e)
  in

  let translate_glb_init x =
        match x with
	None -> None 
      | Some None -> Some None
      | Some Some init ->
	  let init = List.map translate_init init in
	    Some (Some init)
  in

  let translate_glbdecl x (t, loc, init) =
    let init = translate_glb_init init in
    let t = translate_typ t in
      Hashtbl.add glbdecls x (t, loc, init, true)
  in

  let translate_fundef f ((args, va_list, t), _, body) =
    let body =
      match body with
	  None -> None
	| Some ((ret_id, args_id), body) -> 
	    (* push return value *)
	    push ret_id;
	    (* push arguments *)
	    List.iter push args_id;
	    let body = Cir.normalize body in
	    let body = translate_blk body in
	      List.iter pop args_id;
	      pop ret_id;
	      Some body
    in
    let ft = translate_ftyp (args, va_list, t) in
      Hashtbl.add fundefs f (ft, body)
  in

    Hashtbl.iter translate_glbdecl cglbdecls;
    Hashtbl.iter translate_fundef cfundefs;
    (glbdecls, fundefs)
  


(**********************************************************************)
(* TODO: this portion of code should be in a different file!!*)
open Csyntax

let parse fname =
  let cin = open_in fname in
  let lexbuf = Lexing.from_channel cin in
    Lexer.init fname lexbuf;
    try
      let cprog = Parser.parse Lexer.token lexbuf in
	close_in cin;
	cprog
    with Parsing.Parse_error -> 
      let lexeme = Lexing.lexeme lexbuf in
	Npkcontext.error "Parser.parse_error" 
	  ("syntax error: unexpected token: "^lexeme)


let compile fname =
  Npkcontext.print_debug ("Parsing "^fname^"...");
  let (fnames, prog) = parse fname in
  let fnames = if fnames = [] then fname::[] else fnames in
    Npkcontext.forget_loc ();
    Npkcontext.print_debug "Parsing done.";
    Npkcontext.print_debug "Running first pass...";
    let prog = Firstpass.translate prog in
      Npkcontext.forget_loc ();
      Npkcontext.print_debug "First pass done.";
      Npkcontext.print_debug ("Translating "^fname^"...");
      let (glbdecls, fundefs) = translate prog in
	Npkcontext.forget_loc ();
	(fnames, glbdecls, fundefs)
  
