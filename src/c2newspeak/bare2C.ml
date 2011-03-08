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

open BareSyntax
module T = Csyntax

let apply_attrs attrs t =
  match (attrs, t) with
      ([], _) -> t
    | (new_sz::[], T.Int (sign, _)) -> T.Int (sign, new_sz)
    | (_::[], _) -> 
	Npkcontext.report_error "Parser.apply_attr" 
	  "wrong type, integer expected"
    | _ -> 
	Npkcontext.report_error "Parser.apply_attr" 
	  "more than one attribute not handled yet"

let process_decls (build_sdecl, build_vdecl) (b, m) =
  let (sdecls, b) = Synthack.normalize_base_typ b in
  let build_vdecl ((v, attrs), init) res =
    let b = apply_attrs attrs b in
    let (t, x, loc) = Synthack.normalize_var_modifier b v in
      match x with
	| None -> res
	| Some x -> build_vdecl res (t, x, loc, init)
  in
  let sdecls = List.map build_sdecl sdecls in
  let vdecls = List.fold_right build_vdecl m [] in
    sdecls@vdecls

let build_glbdecl loc (static, extern) d =
  let build_vdecl l (t, x, loc, init) = 
    let d = 
      { T.t = t; is_static = static; is_extern = extern; initialization = init }
    in
    (T.GlbDecl (x, T.VDecl d), loc)::l
  in
  let build_sdecl x = (T.GlbDecl x, loc) in
    process_decls (build_sdecl, build_vdecl) d

let build_fundef static ((b, m), body) =
  let (_, (t, x, loc)) = Synthack.normalize_decl (b, m) in
  let x =
    match x with
      | Some x -> x
      | None -> 
	  (* TODO: code cleanup remove these things !!! *)
	  Npkcontext.report_error "Firstpass.translate_global" 
	    "unknown function name"
  in
  let t = Csyntax.ftyp_of_typ t in
    (T.FunctionDef (x, t, static, body), loc)::[]

let build_glbtypedef loc d =
  let build_vdecl l (t, x, _, _) = 
    Synthack.define_type x t;
    l
  in
  let build_sdecl x = (T.GlbDecl x, loc) in
    process_decls (build_sdecl, build_vdecl) d

(* TODO: move code out of synthack and parser into bare2C => remove synthack?? *)

let process_global loc x =
  match x with
      FunctionDef (static, x) -> build_fundef static x
    | GlbDecl (modifiers, d) -> build_glbdecl loc modifiers d
    | GlbTypedef x -> build_glbtypedef loc x
    | GlbUserSpec x -> (T.GlbUserSpec x, loc)::[]

let process x = 
  let result = ref [] in
  let process_global (x, loc) =
(* TODO: optimization: think about this concatenation, maybe not efficient *)
    result := (!result)@(process_global loc x)
  in
    List.iter process_global x;
    !result
