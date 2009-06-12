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

open Npkil

module Nat = Newspeak.Nat

module StrSet = Set.Make(String)

module H = Highspeak

(*--------------*)
(* Linking time *)
(*--------------*)

(* Association table global -> Newspeak.typ *)
let globals = Hashtbl.create 100
let funspecs = Hashtbl.create 100

let get_glob_typ name =
  try
    let (t, _, _) = Hashtbl.find globals name in
      t
  with Not_found ->
    Npkcontext.report_error "Npklink.get_glob_typ" 
      ("type for global variable "^name^" not found")

let rec generate_typ t =
  match t with
      Scalar x -> Newspeak.Scalar x
    | Array (t, Some l) -> Newspeak.Array (generate_typ t, l)
    | Array (_, None) -> 
	Npkcontext.report_error "Link.generate_typ" "unknown array length"
    | Region (fields, sz) -> 
	Newspeak.Region (List.map generate_field fields, sz)

and generate_field (offs, t) = (offs, generate_typ t)

and generate_ftyp (args, ret) =
  let ret = 
    match ret with
	None -> None
      | Some t -> Some (generate_typ t)
  in
    (List.map generate_typ args, ret)

let rec generate_init init =
  match init with
      Some l -> H.Init (List.map generate_init_field l)
    | None when !Npkcontext.global_zero_init -> H.Zero
    | None -> H.Init []

and generate_init_field (sz, sca, e) = 
  let e = generate_exp e in
    (sz, sca, e)

and generate_lv lv =
  match lv with
    | Global name -> H.Global name
    | Deref (e, sz) -> H.Deref (generate_exp e, sz)
    | Shift (lv', e) -> H.Shift (generate_lv lv', generate_exp e)
    | Local v -> H.Local v
	
and generate_exp e =
  match e with
    | Lval (lv, t) -> H.Lval (generate_lv lv, generate_typ t)
    | Const c -> H.Const c 
    | AddrOfFun (fid, ft) -> H.AddrOfFun (fid, generate_ftyp ft)
    | AddrOf (lv, sz) -> 
	let sz = 
	  try Nat.to_int (generate_tmp_nat sz) 
	  with Invalid_argument "Newspeak.Nat.to_int" -> Config.max_sizeof
	in
	  if (sz > Config.max_sizeof) then begin
	    Npkcontext.report_error "Link.generate_exp" 
	      ("size too large: maximum allowed is "
	       ^(string_of_int Config.max_sizeof)^" bits")
	  end;
	  H.AddrOf (generate_lv lv, sz)
    | UnOp (o, e) -> H.UnOp (generate_unop o, generate_exp e)
    | BinOp (o, e1, e2) -> H.BinOp (o, generate_exp e1, generate_exp e2)

(* TODO:
   Avoid redefinition of unop and binop. Use the same for npkil and newspeak
   just add a belongs_tmp to npkil !!! *)
and generate_unop o =
  match o with
      Belongs_tmp (l, u) -> 
	let u = Nat.sub (generate_tmp_nat u) Nat.one in
	  Newspeak.Belongs (l, u)
    | Coerce r -> Newspeak.Coerce r
    | Not -> Newspeak.Not
    | BNot r -> Newspeak.BNot r
    | PtrToInt k -> Newspeak.PtrToInt k
    | IntToPtr k -> Newspeak.IntToPtr k
    | Cast (t1, t2) -> Newspeak.Cast (t1, t2)

and generate_tmp_nat x =
  match x with
      Known i -> i
    | Length name -> begin
	match get_glob_typ name with
	    Newspeak.Array (_, len) -> Nat.of_int len
	  | _ -> 
	      Npkcontext.report_error "Npklink.generate_tmp_nat" 
		"array type expected"
      end
    | Mult (v, n) -> 
	let i = generate_tmp_nat v in
	  Nat.mul_int n i

let generate_global_init name (_, _, init, _) =
  try
    let (t, _, loc) = Hashtbl.find globals name in 
    let i =
      match init with
	| Some i -> i
	| None -> 
	    Npkcontext.report_accept_warning "Link.generate_global" 
	      ("extern global variable "^name) Npkcontext.ExternGlobal;
	    None
    in
      Hashtbl.replace globals name (t, generate_init i, loc)
  with 
      Not_found -> ()

let generate_global name (t, loc, _, used) =
  Npkcontext.set_loc loc;
  if used || (not !Npkcontext.remove_temp) then begin
    let t = generate_typ t in
      Hashtbl.add globals name (t, H.Init [], loc);
      Npkcontext.print_debug ("Global linked: "^name)
  end

let rec generate_stmt (sk, loc) =
  let new_sk = 
    match sk with
	Set (lv, e, t) -> H.Set (generate_lv lv, generate_exp e, generate_typ t)
      | Decl (name, t, b) -> 
	  H.Decl (name, generate_typ t, List.map generate_stmt b)
      | Guard cond -> H.Guard (generate_exp cond)
      | Select (body1, body2) ->
	  H.Select (generate_blk body1, generate_blk body2)
      | InfLoop b -> H.InfLoop (List.map generate_stmt b)
      | Call (args, ft, fn, rets) ->
	  let args = List.map generate_exp args in
	  let ft = generate_ftyp ft in
	  let fn = generate_fn fn in
	  let rets = List.map generate_lv rets in
	  H.Call (args, ft, fn, rets)
      | Goto lbl -> H.Goto lbl
      | DoWith (body, lbl, action) ->
	  let body = List.map generate_stmt body in
	  let action = List.map generate_stmt action in
	    H.DoWith (body, lbl, action)
      | UserSpec x -> H.UserSpec (List.map generate_token x)
  in 
    (new_sk, loc)

and generate_token x =
  match x with
      SymbolToken c -> H.SymbolToken c
    | IdentToken x -> H.IdentToken x
    | LvalToken (lv, t) -> H.LvalToken (generate_lv lv, generate_typ t)
    | CstToken c -> H.CstToken c

and generate_blk x = List.map generate_stmt x
    
and generate_fn fn =
  match fn with
    | FunId f -> H.FunId f
    | FunDeref e -> H.FunDeref (generate_exp e)

and generate_body body = List.map generate_stmt body

let generate_fundecs fundecs =
  let funspecs = Hashtbl.create 100 in
  let add_fundec (name, (rets, args, ftyp, body)) =
    let body = generate_body body in
    let ftyp = generate_ftyp ftyp in
      
      if Hashtbl.mem funspecs name then begin
	Npkcontext.report_error "Npklink.generate_funspecs" 
	  ("function "^name^" declared twice")
      end;
      Hashtbl.add funspecs name (rets, args, ftyp, body);
      Npkcontext.print_debug ("Function linked: "^name)
  in
    List.iter add_fundec fundecs;
    funspecs      

(* TODO: optimization, this is probably not efficient to read the whole
   program and then again a second time!!! reprogram Npkil.read and write *)
let merge npkos =
  let glb_decls = Hashtbl.create 100 in
  let fnames = ref StrSet.empty in
  let specs = ref [] in
  let fundefs = ref [] in

  let add_fname x = fnames := StrSet.add x !fnames in

  let add_fundef f body = fundefs := (f, body)::!fundefs in

  let add_global name (t, loc, init, used) =
    Npkcontext.set_loc loc;
    try
      let (prev_t, prev_loc, prev_init, prev_used) = 
	Hashtbl.find glb_decls name 
      in
	
      let t =
	try
	  if (Npkil.is_mp_typ t prev_t) then t
	  else prev_t
	with Npkil.Uncomparable -> 
	  (* TODO: add the respective locations *)
	  Npkcontext.report_error "Npklink.update_glob_link"
	    ("different types for "^name^": '"
	     ^(Npkil.string_of_typ prev_t)^"' and '"
	     ^(Npkil.string_of_typ t)^"'")
      in
      let used = used || prev_used in
      let init = 
	match init, prev_init with
	    (None, Some _) -> prev_init
	  | (Some _, None) -> init
	  | (None, None) -> prev_init
	  | (Some (Some _), Some (Some _)) -> 
	      Npkcontext.report_error "Npklink.update_glob_link" 
		("multiple declaration of "^name)
	  | _ ->
	      let info = 
		if prev_loc = loc then begin
		  let (file, _, _) = loc in
		    ", in file "^file^" variable "
		    ^name^" should probably be extern"
		end else begin
		  " (previous definition: "
		  ^(Newspeak.string_of_loc prev_loc)^")"
		end
	      in
		Npkcontext.report_accept_warning "Npklink.update_glob_link"
		  ("multiple definitions of global variable "^name^info) 
		  Npkcontext.MultipleDef;	      
		prev_init
      in
	Hashtbl.replace glb_decls name (t, prev_loc, init, used)
	  
    with Not_found -> Hashtbl.add glb_decls name (t, loc, init, used)
  in

  let add_spec x = specs := (List.map generate_token x)::(!specs) in

  let merge npko =
    (* TODO: merge these two operations into one *)
    let prog = Npkil.read npko in
      List.iter add_fname prog.fnames;
      Hashtbl.iter add_global prog.globals;
      List.iter add_spec prog.specs;
      Hashtbl.iter add_fundef prog.fundecs;
      prog.src_lang
  in
    match npkos with
	[] -> Npkcontext.report_error "Linker.merge" "empty file list"
      | hd::tl -> 
	  let src_lang = merge hd in
	  let check_merge x = 
	    let _ = merge x in
	      ()
	  in
	    List.iter check_merge tl;
	    (StrSet.elements !fnames, glb_decls, !fundefs, src_lang, !specs)

let link npkos =
  Npkcontext.forget_loc ();
    
  Npkcontext.print_debug "Linking files...";
  let (filenames, glb_decls, fun_decls, src_lang, specs) = merge npkos in
    
    Npkcontext.print_debug "Globals...";
    Hashtbl.iter generate_global glb_decls;
    Hashtbl.iter generate_global_init glb_decls;
    Npkcontext.forget_loc ();
    
    Npkcontext.print_debug "Functions...";
    let fundecs = generate_fundecs fun_decls in
	
    let prog = { 
      H.fnames = filenames;
      H.globals = globals;
      H.fundecs = fundecs;
      H.specs = specs;
      H.ptr_sz = Config.size_of_ptr;
      H.src_lang = src_lang;
    } 
    in
      
      Npkcontext.print_debug "File linked.";
      Npkcontext.print_debug "Converting to Newspeak...";
      let prog = Hpk2npk.translate prog in
	Npkcontext.print_debug "Conversion done.";
	let prog = 
	  if !Npkcontext.no_opt then prog
	  else Newspeak.simplify !Npkcontext.opt_checks prog
	in
	  
	  Newspeak.write !Npkcontext.output_file prog;
	  if !Npkcontext.verb_newspeak then begin
	    print_endline "Newspeak output";
	    print_endline "---------------";
	    Newspeak.dump prog;
	    print_newline ()
	  end

