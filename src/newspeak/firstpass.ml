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

  Olivier Levillain
  email: olivier.levillain@penjili.org
*)


open Cil
open Cilutils
open Npkcontext
open Npkutils

type glb_type = {
  mutable gtype : Cil.typ;
  mutable gloc : Cil.location;
  mutable gdefd : bool;
  mutable ginit : Cil.init option;
}

type fspec_type = {
  mutable prett : Npkil.typ option;
  mutable pargs : ((int * string * Npkil.typ) list) option;
  mutable plocs : ((int * string * Npkil.typ) list) option;
  mutable ploc  : Newspeak.location;
  mutable pbody : Cil.block option;
}

let local_vids = ref Int_set.empty
let code_to_duplicate = Hashtbl.create 100

class visitor_first_pass = 
object (this)
  inherit nopCilVisitor

  val mutable glb_used = Npkil.String_set.empty
  val mutable glb_cstr = Npkil.String_set.empty

  method get_used = (glb_used, glb_cstr)

  method vglob g =
    Npkcontext.set_loc (get_globalLoc g);
    DoChildren


  (* deletes CIL's gotos (this piece of code might be highly dependent
     on CIL and may have to change in the future. The code that should be
     duplicated is stored in a hash table and used when a Goto is
     encountered) *)
  method vstmt s =
    Npkcontext.set_loc (get_stmtLoc s.skind);

    let add_code goto_stmts label =
      match label with
	| Label (s, _, false) as l ->
	    Hashtbl.add code_to_duplicate l goto_stmts
	| _ -> ()
    in

      match s.skind with
	| If (_, {bstmts = ({labels = l1}::_ as goto_stmts1)},
	      {bstmts = ({labels = l2}::_ as goto_stmts2)}, _) ->
	    List.iter (add_code goto_stmts1) l1;
	    List.iter (add_code goto_stmts2) l2;
	    DoChildren
	| If (_, {bstmts = ({labels = l}::_ as goto_stmts)}, _, _)
	| If (_, _, {bstmts = ({labels = l}::_ as goto_stmts)}, _) ->
	    List.iter (add_code goto_stmts) l;
	    DoChildren
	| _ -> DoChildren


  (* removes unused local vars *)
  method vfunc f =
    local_vids := Int_set.empty;
    let del_unused f =
      let rec new_locals locs =
	match locs with
	  | [] -> []
	  | v::r when Int_set.mem v.vid !local_vids -> v::(new_locals r)
	  | _::r -> new_locals r
      in
	if !remove_temp then f.slocals <- new_locals f.slocals;
	f
	  
    in
      ChangeDoChildrenPost (f, del_unused)

  method register_var cil_var =
    match cil_var.vtype with
      | TFun _ -> ()
      | _ -> 
	  let norm_name = Npkenv.glb_uniquename cil_var in
	    (*TODO: clean up String_set name here: *)
	    glb_used <- Npkil.String_set.add norm_name glb_used
	      
  method register_cstr s =
    glb_cstr <- Npkil.String_set.add s glb_cstr

  (* remembers the local variables used *)
  method vlval lv =
    match lv with
      | Var v, _ ->
	  if v.vglob
	  then this#register_var v
	  else local_vids := Int_set.add v.vid !local_vids;
	  DoChildren
      | _ -> DoChildren

  (* simplifies some exps (StartOf, pointer equality) *)
  method vexpr e =
    match e with
      | Const (CStr s) ->
	  this#register_cstr s;
	  SkipChildren

      | StartOf lv  -> 
	  ChangeDoChildrenPost (AddrOf (addOffsetLval (Index (zero, NoOffset)) lv), fun x -> x)

      | BinOp (Ne as o, CastE(TInt _, e1), CastE(TInt _, e2), (TInt _ as t))
      | BinOp (Eq as o, CastE(TInt _, e1), CastE(TInt _, e2), (TInt _ as t))
	  when size_of t = pointer_size && isPtr e1 && isPtr e2 ->
	  ChangeDoChildrenPost (BinOp (o, e1, e2, t), fun x -> x)
	    
      | _ -> DoChildren

end




(* TODO: Is it necessary ? Should I remove or rewrite this code ? *)
let check_main_signature t =
  let (ret, args) = 
    match unrollType t with
	TFun (ret, Some args, _, []) -> (ret, args)
      | _ ->
	  error "Npkfirstpass.check_main_signature"
	    "main, should have a function type"
  in
    if (!verb_morewarns) && (unrollType ret <> TInt (IInt, [])) 
    then print_warning "Npkfirstpass.check_main_signature"
      "return type of 'main' is not 'int'";
    match args with
	[] -> ()
      | (_, arg1, [])::(_, arg2, [])::[] 
	  when (unrollType arg1 = TInt (IInt, []))
	    && (unrollTypeDeep arg2 = 
		TPtr (TPtr (TInt (IChar, []), []), []))
	    -> ()
      | _ ->
	  error "Npkfirstpass.check_main_signature: "
	    ("invalid argument types for main, "
	     ^"authorized forms are main() and"
	     ^" main(int, char**)")


(* Exploration of Cil's "globals" and first pass *)
(* TODO: factor out print_warnings by putting together the warnings and 
   selecting which should be verb_warnings or not *)
let first_pass f =
  let glb_decls = Hashtbl.create 100 in
  let fun_specs = Hashtbl.create 100 in

  let update_fun_def f =
    let name = f.svar.vname in
      
    let rettype = 
      match f.svar.vtype with
	| TFun (TVoid _, _, _, _) -> None
	| TFun (t, _, _, _) -> Some (translate_typ t)
	| _ ->
	    error "Npkenv.update_fun_def"
	      ("invalid type \""^(string_of_type f.svar.vtype)^"\"")
    in
      
    let translate_local v = v.vid, v.vname, translate_typ v.vtype in
    let formals = List.map translate_local f.sformals in
    let locals = List.map translate_local f.slocals in
    let loc = Npkcontext.get_loc () in
      
      (* TODO: remove this Some ?? *)
      Npkenv.update_fun_proto name rettype (Some formals);

      if (Hashtbl.mem fun_specs name) 
      then error "Npkenv.update_fun_def" ("multiple definition for "^name);
      
      Hashtbl.add fun_specs name (locals, formals, f.sbody)
  in

  let update_glob_decl v =
    let name = Npkenv.glb_uniquename v in
      try
	let x = Hashtbl.find glb_decls name in
	  if not (Npkil.compare_typs 
		     (translate_typ x.gtype) 
		     (translate_typ v.vtype))
	    (* TODO: add the respective locations *)
	  then error "Npkenv.update_glob_decl"
	    ("different types for "^name^": '"
	      ^(string_of_type x.gtype)^"' and '"
	      ^(string_of_type v.vtype)^"'")
      with Not_found ->
	Hashtbl.add glb_decls name
	  {gtype = v.vtype; gloc = v.vdecl;
	   gdefd = false; ginit = None;}
  in

(* TODO: factor this code with the one up there *)
  let update_glob_def v i =
    let name = Npkenv.glb_uniquename v in
      try
	let x = Hashtbl.find glb_decls name in
	  if not (Npkil.compare_typs 
		     (translate_typ x.gtype) 
		     (translate_typ v.vtype))
	    (* TODO: add the respective locations *)
	  then error "Npkenv.update_glob_decl"
	    ("different types for "^name^": '"
	      ^(string_of_type x.gtype)^"' and '"
	      ^(string_of_type v.vtype)^"'");
	  if x.gdefd then begin
	    if not !accept_mult_def then
	      error "Npkenv.glb_declare" ("multiple definition for "^name);
	    if (x.ginit <> None) && (i <> None) then
	      error "Npkenv.glb_declare" ("multiple declarations for "^name);
	    print_warning "Npkenv.glb_declare" 
	      ("multiple declarations for "^name)
	  end;
	  x.gtype <- v.vtype;
	  x.gdefd <- true;
	  x.gloc <- v.vdecl;
	  x.ginit <- i
      with Not_found ->
	Hashtbl.add glb_decls name
	  {gtype = v.vtype; gloc = v.vdecl;
	   gdefd = true; ginit = i;}
  in

  let visitor = new visitor_first_pass in
    
  let rec explore g = 
    let loc = get_globalLoc g in
    let new_g = visitCilGlobal (visitor :> Cil.cilVisitor) g in
      Npkcontext.set_loc loc;
      if loc.file <> "<compiler builtins>" then
	match new_g with
	  | [GType (t, _)] ->
	      print_morewarn "Npkfirstpass.first_pass.explore"
		("skipping typedef "^t.tname)
	  | [GEnumTag (info, _)] -> 
	      print_morewarn "Npkfirstpass.first_pass.explore"
		("skipping enum "^info.ename)
	  | [GCompTag (c, _)] -> 
	      print_morewarn "Npkfirstpass.first_pass.explore"
		("skipping composite typedef "^c.cname)
	  | [GCompTagDecl (c, _)] -> 
	      print_morewarn "Npkfirstpass.first_pass.explore"
		("skipping composite declaration "^c.cname)
	  | [GPragma (a, _)] when !ignores_pragmas -> 
	      print_warning "Npkfirstpass.first_pass.explore"
		("ignoring directive: unknown #pragma "^(string_of_attribute a))
		      
	  | [GVarDecl ({vname = name; vtype = TFun (ret,args,_,_)}, _)] ->
	      let ret = Npkutils.translate_ret_typ ret in
	      let args = Npkenv.translate_formals name args in
		Npkenv.update_fun_proto name ret args
		  
	  | [GFun (f, loc)] -> 
	      if (f.svar.vname = "main")
	      then check_main_signature f.svar.vtype;
	      update_fun_def f;
	      
	  | [GVarDecl (v, _)] -> update_glob_decl v
		  
	  | [GVar (v, {init = i}, _)] -> update_glob_def v i
		
	  | _ ->
	      error "Npkfirstpass.first_pass.explore"
		("global "^(string_of_global g)^" not supported")
  in

      Npkcontext.forget_loc ();
      Npkenv.init_env ();
      List.iter explore f.globals;
      let (glb_used, glb_cstr) = visitor#get_used in
	(glb_used, glb_cstr, fun_specs, glb_decls)
