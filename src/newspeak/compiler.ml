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

open Lexing
open Csyntax

(* TODO: code cleanup: break this file in several module. 
   do factorisation (with Cilcompiler too), when possible. 
   In particular K.size_of  and align *)
module N = Newspeak
module K = Npkil

let ret_lbl = 0
let cnt_lbl = 1
let brk_lbl = 2

let translate_array lv (t, n) =
  let n =
    match (n, lv) with
	(Some n, _) -> K.Known n
      | (_, K.Global x) -> K.Length x
      | _ -> Npkcontext.error "Compiler.translate_array" "Unknown array length"
  in
    (t, n)

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
      Not -> K.negate e
    | BNot k -> K.UnOp (K.BNot (N.domain_of_typ k), e)
    | Cast (t, t') -> 
	let t = translate_scalar t in
	let t' = translate_scalar t' in
	  K.cast t e t'

let translate_arithmop op e1 e2 k = K.make_int_coerce k (K.BinOp (op, e1, e2))

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

let translate fname (compdefs, cglbdecls, cfundefs) = 
  let glbdecls = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in
  let stack_height = ref 0 in

  (* Hashtbl of already translated types, to as to have some sharing *)
  let translated_typ = Hashtbl.create 100 in

  let push () = 
    incr stack_height;
    Local !stack_height
  in
  let pop () = decr stack_height in
  let index_of_var x = !stack_height - x in

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
	      let (fields, sz) = 
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

  let translate_ftyp (args, ret) =
    let args = List.map (fun (t, _) -> translate_typ t) args in
    let ret =
      match ret with
	  Void -> None
	| _ -> Some (translate_typ ret)
    in
      (args, ret)
  in

  let translate_binop op e1 e2 =
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
	    "Pointer arithmetic forbidden on function pointers"
      | PlusP t -> 
	  let stride = K.exp_of_int (size_of compdefs t) in 
	    K.BinOp (N.PlusPI, e1, K.BinOp (N.MultI, e2, stride))
      | MinusP -> K.make_int_coerce int_kind (K.BinOp (N.MinusPP, e1, e2))

      (* TODO: clean bug ? maybe a cast is necessary ? *)
      | Gt t -> 
	  let t = translate_scalar t in
	    K.BinOp (N.Gt t, e1, e2)
	      
      | Eq t -> 
	  let t = translate_scalar t in
	    K.BinOp (N.Eq t, e1, e2)
  in

  let rec translate_lv lv =
    match lv with
	Local x -> K.Local (index_of_var x)

      | Global x -> K.Global x

      | Field (lv, f, o) ->
	  let lv = translate_lv lv in
	  let o = K.exp_of_int o in
	    K.Shift (lv, o)

      | Index (lv, a, e) -> 
	  let lv = translate_lv lv in
	  let (t, n) = translate_array lv a in
	  let i = translate_exp e in
	  let sz = K.exp_of_int (size_of compdefs t) in
	  let o = K.UnOp (K.Belongs_tmp (Int64.zero, K.Decr n), i) in
	  let o = K.BinOp (N.MultI, o, sz) in
	    K.Shift (lv, o)

      | Deref (e, t) ->
	  let e = translate_exp e in
	  let sz = size_of compdefs t in
	    K.Deref (e, sz)

  and translate_exp e =
    match e with
	Const i -> K.Const (translate_cst i)

      | Lval (lv, t) -> 
	  let lv = translate_lv lv in
	    K.Lval (lv, translate_scalar t)

      | AddrOf (Global f, Fun t) -> K.AddrOfFun f

      | AddrOf (Index (lv, a, e), _) ->
	  let lv = translate_lv lv in
	  let i = translate_exp e in
	  let (t, n) = translate_array lv a in
	  let sz = size_of compdefs t in
	  let sz_e = K.exp_of_int sz in
	  let o = K.UnOp (K.Belongs_tmp (Int64.zero, n), i) in
	  let o = K.BinOp (N.MultI, o, sz_e) in
	  let n = K.Mult (n, sz) in
	    K.BinOp (N.PlusPI, K.AddrOf (lv, n), o)

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
	    translate_binop op e1 e2
  in

  let rec translate_blk x =
    match x with
	hd::tl -> 
	  let hd = translate_stmt hd in
	  let tl = translate_blk tl in
	    hd@tl
      | [] -> []

  and translate_set loc (lv, t, e) =  
    let lv = translate_lv lv in
      match (t, e) with
	  ((Struct _ | Union _), Lval (lv_src, _)) -> 
	    let lv_src = translate_lv lv_src in
	    let sz = size_of compdefs t in
	      (K.Copy (lv, lv_src, sz), loc)
	| _ ->
	    let e = translate_exp e in
	    let t = translate_scalar t in
	      (K.Set (lv, e, t), loc)

  and translate_init (o, t, e) =
    let t = translate_scalar t in
    let e = translate_exp e in
      (o, t, e)
    
(* TODO: replace inits by sets !!*)
  and translate_local_init loc x init =
    let (o, t, e) = translate_init init in 
    let lv = translate_lv (Local x) in
    let lv = K.Shift (lv, K.exp_of_int o) in
      (K.Set (lv, e, t), loc)

  and translate_stmt (x, loc) =
    Npkcontext.set_loc loc;
    match x with
      | Init (x, init) -> List.map (translate_local_init loc x) init

      | Set set -> (translate_set loc set)::[]

      | If (e, body1, body2) ->
	  let cond1 = translate_exp e in
	  let body1 = translate_blk body1 in
	  let body2 = translate_blk body2 in begin
	    match cond1 with
		K.Const N.CInt64 i when Int64.compare i Int64.zero <> 0 -> 
		  body1
	      | K.Const N.CInt64 _ -> body2
	      | _ -> 
		  let cond2 = K.negate cond1 in
		    (K.ChooseAssert [([cond1], body1); 
				     ([cond2], body2)], loc)::[]
	  end

      | Loop (body1, body2) ->
	  let body1 = translate_blk body1 in
	  let body2 = translate_blk body2 in
	  let body = (K.DoWith (body1, cnt_lbl, []), loc)::body2 in
	  let loop = (K.InfLoop body, loc)::[] in
	    (K.DoWith (loop, brk_lbl, []), loc)::[]

      | Return -> (K.Goto ret_lbl, loc)::[]

      | Call f -> translate_call loc f
 
      | Break -> (K.Goto brk_lbl, loc)::[]

      | Continue -> (K.Goto cnt_lbl, loc)::[]
      
      | Switch (e, cases) -> translate_switch loc e cases

  and translate_decl loc (t, x) =
    let t = translate_typ t in
      (x, t, loc)

  and translate_call loc (r, (f, (args_t, ret_t)), args_exp) =
    let fid = 
      match f with
	  Global f -> f
	| _ -> "fptr_call"
    in
    let (ret_var, ret_decl) =
      match ret_t with
	  Void -> (None, [])
	| _ -> 
	    let v = push () in
	    let d = translate_decl loc (ret_t, "value_of_"^fid) in
	      (Some v, d::[])
    in
    let ret_set = 
      match (r, ret_var) with
	  (None, _) -> []
	| (Some lv, Some v) -> 
	    let ret_e = Lval (v, ret_t) in
	    let set = translate_set loc (lv, ret_t, ret_e) in
	      set::[]
	| _ -> 
	    (* TODO: code cleanup: change error message *)
	    Npkcontext.error "Compiler.translate_call" "No return value";
    in
    let args_var = List.map (fun (t, _) -> (push (), t)) args_t in
    let decls = List.map (translate_decl loc) args_t in
    let translate_arg (lv, t) e = translate_set loc (lv, t, e) in
    let sets = 
      try List.map2 translate_arg args_var args_exp
      with Invalid_argument "List.map2" ->
	(* TODO: code cleanup: change error message *)
	Npkcontext.error "Compiler.translate_call" 
	  ("Different types for function "^fid)
    in
    let fn = 
      match f with
	  Global f -> K.FunId f 
	| Deref (e, Fun ft) ->
	    let e = translate_exp e in
	    let ft = translate_ftyp (args_t, ret_t) in
	      K.FunDeref (e, ft)
	| _ -> 
	    Npkcontext.error "Compiler.translate_call" "Unreachable statement"
    in

      if ret_t <> Void then pop ();
      List.iter (fun _ -> pop ()) args_t;
      
      let call = (K.Call fn, loc)::[] in
      let call = sets@call in
      let call = K.append_decls decls call in
	(* TODO: code optimization: write this so that there is no @ 
	   (by putting ret_set under the scope of local variables too) *)
      let call = call@ret_set in
      let call = K.append_decls ret_decl call in
	call

  and translate_switch loc e cases =
    let switch_exp = translate_exp e in
    let default_lbl = brk_lbl in
    let default_cond = ref [] in
    let default_goto = ref [K.Goto default_lbl, loc] in

    let choices = ref [] in

    let found_case lbl v =
      match v with
	  Some (v, t) ->
	    let v = translate_exp v in
	    let t = translate_scalar t in
	    let cond = K.BinOp (Newspeak.Eq t, switch_exp, v) in
	      default_cond := (K.negate cond)::!default_cond;
	      choices := (cond::[], [K.Goto lbl, loc])::!choices
	| None -> default_goto := [K.Goto lbl, loc]
    in
    let rec translate_cases x =
      match x with
	  (v, [], case_loc)::(v', body, _)::tl ->
	    let (lbl, cases) = translate_cases ((v', body, case_loc)::tl) in
	    found_case (lbl - 1) v;
	      (lbl, cases)
	| (v, body, case_loc)::tl ->
	    let (lbl, tl) = translate_cases tl in
	    let body = translate_blk body in
	      found_case lbl v;
	      (lbl + 1, (lbl, body, case_loc)::tl)
	| [] -> (brk_lbl + 1, [])
    in

    let (_, cases) = translate_cases cases in
    let default_choice = (!default_cond, !default_goto) in
    let switch = [K.ChooseAssert (default_choice::!choices), loc] in

    let rec append_cases x =
      match x with
	  (lbl, body, loc)::tl -> 
	    let tl = append_cases tl in
	      (K.DoWith (tl, lbl, []), loc)::body
	| [] -> switch
    in
      (* TODO: optimize this, do not rev cases, maybe have it as a reference *)
    let switch = append_cases (List.rev cases) in
      [K.DoWith (switch, default_lbl, []), loc]
	
  in

  let translate_init x =
    match x with
	None -> None 
      | Some None -> Some None
      | Some Some init ->
	  let init = List.map translate_init init in
	    Some (Some init)
  in

  let translate_glbdecl x (t, loc, init) =
    let init = translate_init init in
    let t = translate_typ t in
      (* TODO: maybe do this in a first pass, since there may be the need for
	 a variable not encountered yet, in init *)
      Hashtbl.add glbdecls x (t, loc, init, true)
  in

  let translate_local (t, x, loc) = 
    let _ = push () in
      translate_decl loc (t, x)
  in

  let translate_fundef f ((args, t), loc, body) =
    let body =
      match body with
	  None -> None
	| Some (locals, body) ->
	    (* push return value *)
	    let _ = push () in
	      (* push arguments *)
	    let _ = List.map (fun _ -> push ()) args in
	      (* push local variables *)
	    let locals = List.map translate_local locals in
	    let body = translate_blk body in
	      List.iter (fun _ -> pop ()) args;
	      pop ();
	      let body = (K.DoWith (body, ret_lbl, []), loc)::[] in
	      let body = K.append_decls locals body in
		List.iter (fun _ -> pop ()) locals;
		Some body
    in
    let ft = translate_ftyp (args, t) in
      Hashtbl.add fundefs f (ft, body)
  in

    Hashtbl.iter translate_glbdecl cglbdecls;
    Hashtbl.iter translate_fundef cfundefs;

    (fname, glbdecls, fundefs)


let parse fname =
  let cin = open_in fname in
  let lexbuf = Lexing.from_channel cin in
    Lexer.init fname lexbuf;
    try
      let cprog = Parser.parse Lexer.token lexbuf in
	close_in cin;
	cprog
    with Parsing.Parse_error -> 
      let pos = Lexing.lexeme_start_p lexbuf in
      let line_nb = string_of_int pos.pos_lnum in
      let lexeme = Lexing.lexeme lexbuf in
	Npkcontext.error "Parser.parse_error" 
	  ("syntax error: unexpected token: "^lexeme)


let compile fname =
  Npkcontext.print_debug ("Parsing "^fname^"...");
  let prog = parse fname in
    Npkcontext.forget_loc ();
    Npkcontext.print_debug "Parsing done.";
    Npkcontext.print_debug "Running first pass...";
    let prog = Firstpass.translate fname prog in
      Npkcontext.forget_loc ();
      Npkcontext.print_debug "First pass done.";
      Npkcontext.print_debug ("Translating "^fname^"...");
      let prog = translate fname prog in
	Npkcontext.forget_loc ();
	prog
  
