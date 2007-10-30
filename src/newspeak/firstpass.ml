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

let char_typ = C.Int (Newspeak.Signed, Config.size_of_char)

  (* TODO: code cleanup: find a way to factor this with create_cstr
     in Npkil *)
let init_of_string str =
  let len = String.length str in
  let res = ref [(len, char_typ, C.Const Int64.zero)] in
    for i = len - 1 downto 0 do 
      let c = Char.code str.[i] in
	res := (i, char_typ, C.Const (Int64.of_int c))::!res
    done;
    (len + 1, Some !res)

let translate fname cprog =
  let typedefs = Hashtbl.create 100 in
  let glbdecls = Hashtbl.create 100 in
  let fundefs = Hashtbl.create 100 in

  let add_glb_cstr str = 
    (* TODO: code cleanup: find a way to factor this with create_cstr
       in Npkil *)
    let name = "!"^fname^".const_str_"^str in
      if not (Hashtbl.mem glbdecls name) then begin
	let (len, init) = init_of_string str in
	let t = C.Array (char_typ, Some len) in
	let loc = (fname, -1, -1) in
	  Hashtbl.add glbdecls name (t, loc, Some init)
      end;
      name
  in

  let rec translate_decl (b, v) =
    let b = translate_base_typ b in
    let (t, x) = translate_var_modifier b v in
      (t, x)
	
  and translate_base_typ t =
    match t with
	Integer (s, t) -> C.Int (s, size_of_ityp t)
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
	  (C.Fun (List.map translate_decl args, b), f)
      | Function (Pointer v, args) -> 
	  let args = List.map translate_decl args in
	    translate_var_modifier (C.Ptr (C.Fun (args, b))) v
      | Array (v, n) -> 
	  let n = Some (int64_to_int n) in
	    translate_var_modifier (C.Array (b, n)) v
      | Pointer v -> translate_var_modifier (C.Ptr b) v
      | Function _ -> 
	  Npkcontext.error "Firstpass.translate_var_modifier" 
	    "case not implemented yet"
  in
    
  let rec translate_lv x =
    match x with
	Var x -> C.Var x
      | Field (lv, f) -> C.Field (translate_lv lv, f)
      | Index (lv, e) -> C.Index (translate_lv lv, translate_exp e)
      | Deref e -> C.Deref (translate_exp e)

  and translate_exp x =
    match x with
	Const c -> C.Const c
      | Lval lv -> C.Lval (translate_lv lv)
      | AddrOf lv -> C.AddrOf (translate_lv lv) 
      | Unop (op, e) -> C.Unop (op, translate_exp e)
      | Binop (op, e1, e2) -> C.Binop (op, translate_exp e1, translate_exp e2)
      | Call (f, args) -> C.Call (f, List.map translate_exp args)
      | And _ -> 
	  Npkcontext.error "Firstpass.translate_exp" "Unexpected And operator"
  in

  let translate_exp_option e =
    match e with
	None -> None
      | Some e -> Some (translate_exp e)
  in

  let translate_init t x =
    let res = ref [] in
    let o = ref 0 in
    let rec translate t x =
      match (x, t) with
	  (Data e, _) -> res := (!o, t, translate_exp e)::(!res)
	| (Sequence seq, C.Array (t, Some len)) -> 
	    let sz = C.size_of t in
	    let translate_elt x =
	      translate t x;
	      o := !o + sz
	    in
	      List.iter translate_elt seq
	| (CstStr str, Ptr _) -> 
	    let name = add_glb_cstr str in
	    let e = C.AddrOf (C.Index (C.Var name, C.Const Int64.zero)) in
	      res := (!o, t, e)::(!res)
	| _ -> 
	    Npkcontext.error "Firstpass.translate_init"
	      "This type of initialization not implemented yet"
    in
      match x with
	  None -> None
	| Some x -> 
	    translate t x;
	    Some (List.rev !res)
  in

  let rec translate_blk x =
    match x with
      | (Decl (d, init), loc)::tl -> 
	  let (t, x) = translate_decl d in
	  let tl =
	    match init with
		None -> tl
	      | Some Data e -> (Set (Var x, e), loc)::tl
	      | Some _ -> 
		  Npkcontext.error "Firstpass.translate_blk" 
		    ("Initialization of local with multiple values "
		      ^"not implemented")
	  in
	  let tl = translate_blk tl in
	    (C.Decl ((t, x), tl), loc)::[]

      | hd::tl -> (translate_stmt hd)@(translate_blk tl)

      | [] -> []

  and translate_stmt (x, loc) =
    match x with
	Set (lv, e) -> 
	  let lv = translate_lv lv in
	  let e = translate_exp e in
	  (C.Set (lv, e), loc)::[]

      | If ((And (e1, e2), body, loc)::tl) ->
	  let body = (If ((e2, body, loc)::tl), loc)::[] in
	    translate_stmt (If ((e1, body, loc)::tl), loc)

      | If branches -> 
	  let translate_case (e, body, loc) = 
	    (translate_exp e, translate_blk body, loc) 
	  in
	  let branches = List.map translate_case branches in
	    (C.If branches, loc)::[]

      | While (e, body) ->
	  let e = translate_exp e in
	  let body = translate_blk body in
	    (C.While (e, body), loc)::[]

      | DoWhile (body, e) -> 
	  let body = translate_blk body in
	  let e = translate_exp e in
	    (C.DoWhile (body, e), loc)::[]

      | Return e -> (C.Return (translate_exp e), loc)::[]

      | Exp e -> (C.Exp (translate_exp e), loc)::[]

      | Switch (e, cases) -> 
	  let e = translate_exp e in
	  let translate_case (e, body, loc) = 
	    (translate_exp_option e, translate_blk body, loc) 
	  in
	  let cases = List.map translate_case cases in
	    (C.Switch (e, cases), loc)::[]

      | Break -> (C.Break, loc)::[]

      | Block body -> translate_blk body

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

      | GlbDecl (is_extern, _, Some _) when is_extern -> 
	  Npkcontext.error "Firstpass.translate_global"
	    "Extern globals can not be initizalized"
 
      | GlbDecl (is_extern, d, init) -> 
	  let (t, x) = translate_decl d in
	  let init = translate_init t init in
	  let init = if is_extern then None else Some init in
	    Hashtbl.add glbdecls x (t, loc, init)
	  
      | Typedef x -> 
	  let (t, x) = translate_decl x in
	    Hashtbl.add typedefs x t
  in
  
  List.iter translate_global cprog;
  (Hashtbl.create 100, glbdecls, fundefs)
