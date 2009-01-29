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

(*--------------*)
(* Linking time *)
(*--------------*)

(* Association table global -> Newspeak.typ *)
let globals = Hashtbl.create 100
let funspecs = Hashtbl.create 100

let get_glob_typ name =
  try
    let (t, _) = Hashtbl.find globals name in
      t
  with Not_found ->
    Npkcontext.error "Npklink.get_glob_typ" 
      ("type for global variable "^name^" not found")

let rec generate_typ t =
  match t with
      Npkil.Scalar x -> Newspeak.Scalar x
    | Npkil.Array (t, Some l) -> Newspeak.Array (generate_typ t, l)
    | Npkil.Array (_, None) -> 
	Npkcontext.error "Link.generate_typ" "unknown array length"
    | Npkil.Region (fields, sz) -> 
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
    | None -> 
	if !Npkcontext.global_zero_init then Newspeak.Zero 
	else Newspeak.Init []
    | Some l -> Newspeak.Init (List.map generate_init_field l)

and generate_init_field (sz, sca, e) = 
  let e = generate_exp e in
  let e = 
    if !Npkcontext.no_opt then e 
    else Newspeak.simplify_exp !Npkcontext.opt_checks e 
  in
    (sz, sca, e)

and generate_lv lv =
  match lv with
    | Npkil.Global name -> Newspeak.Global name
    | Npkil.Deref (e, sz) -> Newspeak.Deref (generate_exp e, sz)
    | Npkil.Shift (lv', e) -> Newspeak.Shift (generate_lv lv', generate_exp e)
    | Npkil.Local v -> Newspeak.Local v
	
and generate_exp e =
  match e with
    | Npkil.Lval (lv, sca) -> Newspeak.Lval (generate_lv lv, sca)
    | Npkil.Const c -> Newspeak.Const c 
    | Npkil.AddrOfFun (fid, ft) -> Newspeak.AddrOfFun (fid, generate_ftyp ft)
    | Npkil.AddrOf (lv, sz) -> 
	let sz = 
	  try Nat.to_int (generate_tmp_nat sz) 
	  with Invalid_argument "Newspeak.Nat.to_int" -> Config.max_sizeof
	in
	  if (sz > Config.max_sizeof) 
	  then Npkcontext.error "Link.generate_exp" 
	    ("size too large: maximum allowed is "
	     ^(string_of_int Config.max_sizeof)^" bits");
	  Newspeak.AddrOf (generate_lv lv, sz)
    | Npkil.UnOp (o, e) -> Newspeak.UnOp (generate_unop o, generate_exp e)
    | Npkil.BinOp (o, e1, e2) -> 
	Newspeak.BinOp (o, generate_exp e1, generate_exp e2)

and generate_unop o =
  match o with
      Npkil.Belongs_tmp (l, u) -> 
	let u = Nat.sub (generate_tmp_nat u) Nat.one in
	  Newspeak.Belongs (l, u)
    | Npkil.Coerce r -> Newspeak.Coerce r
    | Npkil.Not -> Newspeak.Not
    | Npkil.BNot r -> Newspeak.BNot r
    | Npkil.PtrToInt k -> Newspeak.PtrToInt k
    | Npkil.IntToPtr k -> Newspeak.IntToPtr k
    | Npkil.Cast (t1, t2) -> Newspeak.Cast (t1, t2)

and generate_tmp_nat x =
  match x with
      Npkil.Known i -> i
    | Npkil.Length name -> begin
	match get_glob_typ name with
	    Newspeak.Array (_, len) -> Nat.of_int len
	  | _ -> 
	      Npkcontext.error "Npklink.generate_tmp_nat" 
		"array type expected"
      end
    | Npkil.Mult (v, n) -> 
	let i = generate_tmp_nat v in
	  Nat.mul_int n i

let generate_global name (t, loc, init, used) =
  Npkcontext.set_loc loc;
  if used || (not !Npkcontext.remove_temp) then begin
    let i =
      match init with
	| Some i -> i
	| None -> 
	    Npkcontext.report_accept_warning "Link.generate_global" 
	      ("extern global variable "^name) Npkcontext.ExternGlobal;
	    None
    in
    let t = generate_typ t in
      Hashtbl.add globals name (t, generate_init i);
      Npkcontext.print_debug ("Global linked: "^name)
  end

let rec generate_stmt (sk, loc) =
  let new_sk = 
    match sk with
      | Npkil.Set (lv, e, sca) -> 
	  Newspeak.Set (generate_lv lv, generate_exp e, sca)
      | Npkil.Copy (lv1, lv2, sz) -> 
	  Newspeak.Copy (generate_lv lv1, generate_lv lv2, sz)
      | Npkil.Decl (name, t, b) -> 
	  Newspeak.Decl (name, generate_typ t, List.map generate_stmt b)
      | Npkil.Guard cond -> Newspeak.Guard (generate_exp cond)
      | Npkil.Select (body1, body2) ->
	  Newspeak.Select (generate_blk body1, generate_blk body2)
      | Npkil.InfLoop b -> Newspeak.InfLoop (List.map generate_stmt b)
      | Npkil.Call fn -> Newspeak.Call (generate_fn fn)
      | Npkil.Goto lbl -> Newspeak.Goto lbl
      | Npkil.DoWith (body, lbl, action) ->
	  let body = List.map generate_stmt body in
	  let action = List.map generate_stmt action in
	    Newspeak.DoWith (body, lbl, action)
      | Npkil.UserSpec x -> Newspeak.UserSpec (List.map generate_token x)
  in 
    (new_sk, loc)

and generate_token x =
  match x with
      Npkil.SymbolToken c -> Newspeak.SymbolToken c
    | Npkil.IdentToken x -> Newspeak.IdentToken x
    | Npkil.LvalToken lv -> Newspeak.LvalToken (generate_lv lv)
    | Npkil.CstToken c -> Newspeak.CstToken c

and generate_blk x = List.map generate_stmt x
    
and generate_fn fn =
  match fn with
    | Npkil.FunId f -> Newspeak.FunId f
    | Npkil.FunDeref (e, t) -> 
	Newspeak.FunDeref (generate_exp e, generate_ftyp t)

and generate_body body = List.map generate_stmt body

let generate_fundecs fundecs =
  let funspecs = Hashtbl.create 100 in
  let add_fundec (name, (ftyp, body)) =
    let body = generate_body body in
    let body = 
      if !Npkcontext.no_opt then body
      else Newspeak.simplify !Npkcontext.opt_checks body
    in
    let ftyp = generate_ftyp ftyp in
      
      if Hashtbl.mem funspecs name then begin
	Npkcontext.error "Npklink.generate_funspecs" 
	  ("function "^name^" declared twice")
      end;
      Hashtbl.add funspecs name (ftyp, body);
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
	  Npkcontext.error "Npklink.update_glob_link"
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
	      Npkcontext.error "Npklink.update_glob_link" 
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
    let (fnames, globals, fundefs, specs) = Npkil.read npko in
      List.iter add_fname fnames;
      Hashtbl.iter add_global globals;
      List.iter add_spec specs;
      Hashtbl.iter add_fundef fundefs
  in
    List.iter merge npkos;
    (StrSet.elements !fnames, glb_decls, !fundefs, !specs)

let link npkos mem_zones output_file =
  Npkcontext.forget_loc ();
    
  Npkcontext.print_debug "Linking files...";
  let (filenames, glb_decls, fun_decls, specs) = merge npkos in
    
    Npkcontext.print_debug "Globals...";
    Hashtbl.iter generate_global glb_decls;
    Npkcontext.forget_loc ();
    
    Npkcontext.print_debug "Functions...";
    let fundecs = generate_fundecs fun_decls in
	
    let npk = { 
      Newspeak.fnames = filenames;
      Newspeak.globals = globals;
      Newspeak.fundecs = fundecs;
      Newspeak.specs = specs;
      Newspeak.ptr_sz = Config.size_of_ptr;
      Newspeak.mem_zones = mem_zones
    } in
      Newspeak.write output_file npk;
	
      Npkcontext.print_debug "File linked.";
      
      if !Npkcontext.verb_newspeak then begin
	print_endline "Newspeak output";
	print_endline "---------------";
	let npk = Newspeak.read output_file in
	  Newspeak.dump npk;
	  print_newline ()
      end
