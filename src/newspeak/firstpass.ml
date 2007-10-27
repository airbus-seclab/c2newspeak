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

let int64_to_int x =
  if Int64.compare x (Int64.of_int max_int) > 0 
  then Npkcontext.error "Firstpass.int64_to_int" "integer too big";
  if Int64.compare x (Int64.of_int max_int) > 0 
  then Npkcontext.error "Firstpass.int64_to_int" "expecting positive integer";
  Int64.to_int x

(* TODO: check this for various architecture ? 
   Here align everything on 4 *)
let align o sz = 
  let offset = o mod 4 in
  if offset = 0 then o
  else if offset + sz <= 4 then o
  else (o - offset) + 4

let translate cprog =
  let typedefs = Hashtbl.create 100 in
  let glbdecls = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in

  let rec translate_decl (b, v) =
    let b = translate_base_typ b in
    let (t, x) = translate_var_modifier b v in
      (t, x)
	
  and translate_base_typ t =
    match t with
	Integer (s, t) -> C.Scalar (C.Int (s, size_of_ityp t))
      | Struct f -> C.StructOrUnion (translate_struct_fields f)
      | Union f -> C.StructOrUnion (translate_union_fields f)
      | Void -> C.Void
      | Name x -> 
	  try Hashtbl.find typedefs x
	  with Not_found -> 
	    Npkcontext.error "Firstpass.translate_base_typ" ("Unknown type "^x)

  and translate_struct_fields f =
    let rec translate o f =
      match f with
	  d::f ->
	    let (t, x) = translate_decl d in
	    let sz = C.size_of t in
	    let o = align o sz in
	    let (f, n) = translate (o+sz) f in
	      ((x, (o, t))::f, n)
	| [] -> ([], align o Config.size_of_int)
    in
    let (f, n) = 
      match f with
	  d::[] ->
	    let (t, x) = translate_decl d in
	    let sz = C.size_of t in
	      ((x, (0, t))::[], sz)
	| _ -> translate 0 f 
    in
      (true, f, n)

  and translate_union_fields f =
    let n = ref 0 in
    let translate d =
      let (t, x) = translate_decl d in
      let sz = size_of t in
	if !n < sz then n := sz;
	(x, (0, t))
    in
      (false, List.map translate f, !n)

  and translate_var_modifier b v =
    match v with
	Variable x -> (b, x)
      | Function (Variable f, args) -> 
	  (C.Fun (b, List.map translate_decl args), f)
      | Function (Pointer v, args) -> 
	  let args = List.map translate_decl args in
	    translate_var_modifier (C.Scalar (C.Ptr (C.Fun (b, args)))) v
      | Array (v, n) -> 
	  let n = Some (int64_to_int n) in
	    translate_var_modifier (C.Array (b, n)) v
      | Pointer v -> translate_var_modifier (C.Scalar (C.Ptr b)) v
      | Function _ -> 
	  Npkcontext.error "Firstpass.translate_var_modifier" 
	    "case not implemented yet"
  in
    
  let rec translate_blk body =
    match body with
      | (Decl (x, init), loc)::tl -> 
	  let (t, x) = translate_decl x in
	  let (decls, body) = translate_blk tl in
	  let body =
	    match init with
		None -> body
	      | Some e -> (translate_stmt (Set (Var x, e), loc))::body
	  in
	    (((t, x), loc)::decls, body)

      | _ -> ([], translate_stmt_list body)

  and translate_stmt_list x =
    match x with
	hd::tl -> (translate_stmt hd)::(translate_stmt_list tl)
      | [] -> []

  and translate_stmt (x, loc) =
    match x with
	Set (lv, e) -> (C.Set (lv, e), loc)

      | If branches -> 
	  let translate (e, body, loc) = (e, translate_blk body, loc) in
	  let branches = List.map translate branches in
	    (C.If branches, loc)

      | While (e, body) ->
	  let body = translate_blk body in
	    (C.While (e, body), loc)

      | DoWhile (body, e) -> 
	  let body = translate_blk body in
	    (C.DoWhile (body, e), loc)

      | Return e -> (C.Return e, loc)

      | Exp e -> (C.Exp e, loc)

      | Switch (e, cases) -> 
	  let translate (e, body, loc) = (e, translate_blk body, loc) in
	  let cases = List.map translate cases in
	    (C.Switch (e, cases), loc)

      | Break -> (C.Break, loc)

      | Decl _ -> 
	  Npkcontext.error "Firstpass.translate_stmt"
	    "Variable declaration is allowed at the start of blocks only "
  in

  let translate_global (x, loc) =
    match x with
	FunctionDef (x, body) ->
	  let (t, x) = translate_decl x in
	  let ft = C.ftyp_of_typ t in
	  let body = translate_blk body in
	    Hashtbl.add fundefs x (ft, loc, body)
 
      | GlbDecl (is_extern, d, init) -> 
	  let (t, x) = translate_decl d in
	    Hashtbl.add glbdecls x (t, loc, init, is_extern)
	  
      | Typedef x -> 
	  let (t, x) = translate_decl x in
	    Hashtbl.add typedefs x t
  in
  
  List.iter translate_global cprog;
  (Hashtbl.create 100, glbdecls, fundefs)
