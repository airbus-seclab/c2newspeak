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

module Nat = Newspeak.Nat

module Set = Set.Make(String)

let translate_scalar t =
  match t with
    | Int i -> N.Int i
    | Float n -> N.Float n
    | FunPtr -> N.FunPtr
    | Ptr -> N.Ptr
    | Void -> Npkcontext.error "Compiler.translate_scalar" 
	"value void not ignored as it ought to be"
    | _ -> 
	Npkcontext.error "Compiler.translate_scalar" 
	  "unexpected non scalar type"

let translate_unop op e =
  match op with
      Belongs_tmp r -> K.UnOp (K.Belongs_tmp r, e)
    | Coerce b -> K.UnOp (K.Coerce b, e)
    | Not -> K.negate e
    | BNot k -> K.UnOp (K.BNot (N.domain_of_typ k), e)
    | Cast (t, t') -> 
	let t = translate_scalar t in
	let t' = translate_scalar t' in
	  K.cast t e t'

let translate_arithmop op e1 e2 k = K.make_int_coerce k (K.BinOp (op, e1, e2))

let translate_binop op e1 e2 =
  match op with
      Mult -> K.BinOp (N.MultI, e1, e2)
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
	(* TODO: push this in firstpass!! *)
	let step = K.exp_of_int (size_of t) in 
	  K.BinOp (N.PlusPI, e1, K.BinOp (N.MultI, e2, step))
    | MinusP t -> 
	let e = K.BinOp (N.MinusPP, e1, e2) in
	(* TODO: push this in firstpass!! *)
	let step = size_of t in
	let e = K.BinOp (N.DivI, e, K.exp_of_int step) in
	  K.make_int_coerce int_kind e
    | Gt t -> 
	let t = translate_scalar t in
	  K.BinOp (N.Gt t, e1, e2)
	    
    | Eq t -> 
	let t = translate_scalar t in
	  K.BinOp (N.Eq t, e1, e2)

let translate_cst c =
  match c with
      CInt i -> N.CInt i
    | CFloat f -> N.CFloat f

let translate (cglbdecls, cfundefs, specs) =
  let glbdecls = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in

  let used_glbs = ref Set.empty in
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
	      Npkcontext.error "Compiler.translate_typ" 
		"type void not allowed here"
	  | Int _ | Float _ | Ptr | FunPtr -> K.Scalar (translate_scalar t)
	  | Array (t, sz) -> K.Array (translate_typ t, sz)
	  | Struct (fields, sz) | Union (fields, sz) -> 
	      let translate_field (_, (o, t)) = (o, translate_typ t) in
		K.Region (List.map translate_field fields, sz)
	  | Fun _ -> 
	      Npkcontext.error "Compiler.translate_typ" 
		"Function not allowed here"
      in
	Hashtbl.add translated_typ t t';
	t'
  in

  let translate_ftyp (args, ret) =
    let args = List.map translate_typ args in
    let args = args in
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
      | Global x -> 
	  used_glbs := Set.add x !used_glbs;
	  K.Global x

      | Shift (lv, o) ->
	  let lv = translate_lv lv in
	  let o = translate_exp o in
	    K.Shift (lv, o)

      | Deref (e, t) ->
	  let e = translate_exp e in
	  let sz = size_of t in
	    K.Deref (e, sz)

      | Stmt_lv _ ->
	  Npkcontext.error "Compiler.translate_lval"
	    "unexpected side-effect in left value"

  and translate_exp e =
    match e with
	Const i -> K.Const (translate_cst i)
      | Lval (lv, t) -> 
	  let lv = translate_lv lv in
	    K.Lval (lv, translate_scalar t)

      | AddrOf (Global f, Fun ft) -> K.AddrOfFun (f, translate_ftyp ft)

      | AddrOf (lv, (Array _ as t)) ->
	  let lv = translate_lv lv in
	  let t = translate_typ t in
	  let sz = K.size_of_array t lv in
	    K.AddrOf (lv, sz)

      | AddrOf (lv, t) ->
	  let lv = translate_lv lv in
	  let sz = size_of t in
	    K.AddrOf (lv, K.Known sz)

      | Unop (op, e) -> 
	  let e = translate_exp e in
	    translate_unop op e
      | Binop (op, e1, e2) ->
	  let e1 = translate_exp e1 in
	  let e2 = translate_exp e2 in
	    translate_binop op e1 e2

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
	    let sz = size_of t in
	      K.Copy (lv, lv', sz)
      | _ ->
	  let e = translate_exp e in
	  let t = translate_scalar t in
	    K.Set (lv, e, t)

  and translate_stmt (x, loc) =
    Npkcontext.set_loc loc;
    match x with
	Block (body, None) -> translate_blk body
      | Block (body, Some (lbl, action)) ->
	  let body = translate_blk body in
	  let action = translate_blk action in
	    (K.DoWith (body, lbl, action), loc)::[]

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
		(* TODO: isn't this redundant with firstpass?? *)
		K.Const N.CInt i when Nat.compare i Nat.zero <> 0 -> 
		  body1
	      | K.Const N.CInt _ -> body2
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

  and append_args loc args fid f =
    let rec append x args =
      match args with
	  (e::args, t::args_t) ->
	    let id = fresh_id () in
	      push id;
	      let lv = Var id in
	      let set = translate_set (lv, t, e) in
	      let t = translate_typ t in
	      let call = append (x+1) (args, args_t) in
	      let arg = fid^".arg"^(string_of_int x) in
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

  and translate_call loc ret ((args_t, ret_t), f, args) =
    let fid =
      match f with
	  Fname f -> f
	| _ -> "fptr_call"
    in
    let args = (args, args_t) in
      match ret_t with
	  Void -> append_args loc args fid f
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
	      let call = append_args loc args fid f in
		pop id;
		(K.Decl ("value_of_"^fid, t, call::post), loc)
		
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
    Npkcontext.set_loc loc;
    let init = translate_glb_init init in
    let t = translate_typ t in
      Hashtbl.add glbdecls x (t, loc, init, false)
  in

  let translate_fundef f ((args, t), _, body) =
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
    let ft = translate_ftyp (args, t) in
      Hashtbl.add fundefs f (ft, body)
  in

  let flag_glb x =
    let (t, loc, init, _) = Hashtbl.find glbdecls x in
      Hashtbl.replace glbdecls x (t, loc, init, true)
  in

    Hashtbl.iter translate_glbdecl cglbdecls;
    Hashtbl.iter translate_fundef cfundefs;
    Set.iter flag_glb !used_glbs;
    (glbdecls, fundefs, specs)
  


(**********************************************************************)
(* TODO: this portion of code should be in a different file!!*)
open Csyntax

let parse fname =
  let cin = open_in fname in
  let lexbuf = Lexing.from_channel cin in
  let specbuf = Buffer.create 800 in
    Lexer.init fname lexbuf;
    Synthack.init_tbls ();
    try
      let cprog = Parser.parse (Lexer.token specbuf) lexbuf in
      let specbuf = Lexing.from_string (Buffer.contents specbuf) in
      let spec = Spec_parser.parse Spec_lexer.token specbuf in
	close_in cin;
	(cprog, spec)
    with Parsing.Parse_error -> 
      let lexeme = Lexing.lexeme lexbuf in
	Npkcontext.error "Parser.parse_error" 
	  ("syntax error: unexpected token: "^lexeme)


let compile fname =
  Npkcontext.print_debug ("Parsing "^fname^"...");
  let ((fnames, prog), spec) = parse fname in
  let fnames = if fnames = [] then fname::[] else fnames in
    Npkcontext.forget_loc ();
    Npkcontext.print_debug "Parsing done.";
    Npkcontext.print_debug "Running first pass...";
    let prog = Firstpass.translate (prog, spec) in
      Npkcontext.forget_loc ();
      Npkcontext.print_debug "First pass done.";
      Npkcontext.print_debug ("Translating "^fname^"...");
      let (glbdecls, fundefs, specs) = translate prog in
	Npkcontext.forget_loc ();
	(fnames, glbdecls, fundefs, specs)
  
