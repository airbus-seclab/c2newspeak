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

module Str_set = Set.Make(String)

let size_of = Newspeak.size_of Config.size_of_ptr

let filenames = ref Str_set.empty

let glb_decls = Hashtbl.create 100

let glb_used = ref (Str_set.empty)

let specs = ref []

(*--------------*)
(* Linking time *)
(*--------------*)

(* Association table stdname -> Newspeak.typ *)
(* TODO: put these together with glb_decls *)
let globals = Hashtbl.create 100

let add_specs x = specs := x@(!specs)

let get_glob_typ name =
  try
    let (t, _) = Hashtbl.find globals name in
      t
  with
      Not_found ->
	Npkcontext.error "Npklink.get_glob_typ" 
	  ("type for global variable "^name^" not found")

let rec replace_stmt (sk, l) =
  let new_sk = 
    match sk with
      | Npkil.Set (lv, e, sca) -> Newspeak.Set (replace_lv lv, replace_exp e, sca)
      | Npkil.Copy (lv1, lv2, sz) -> Newspeak.Copy (replace_lv lv1, replace_lv lv2, sz)
      | Npkil.Decl (name, t, b) -> 
	  Newspeak.Decl (name, replace_typ t, List.map replace_stmt b)
      | Npkil.ChooseAssert l -> Newspeak.ChooseAssert (List.map replace_chooseitem l)
      | Npkil.InfLoop b -> Newspeak.InfLoop (List.map replace_stmt b)
      | Npkil.Call fn -> Newspeak.Call (replace_fn fn)
      | Npkil.Goto lbl -> Newspeak.Goto lbl
      | Npkil.DoWith (body, lbl, action) ->
	  let body = List.map replace_stmt body in
	  let action = List.map replace_stmt action in
	    Newspeak.DoWith (body, lbl, action)
  in 
    (new_sk, l)
       
and replace_chooseitem (exps, b) =
  (List.map replace_exp exps, List.map replace_stmt b)
    
and replace_lv lv =
  match lv with
    | Npkil.Global name -> Newspeak.Global name
    | Npkil.Deref (e, sz) -> Newspeak.Deref (replace_exp e, sz)
    | Npkil.Shift (lv', e) -> Newspeak.Shift (replace_lv lv', replace_exp e)
    | Npkil.Local v -> Newspeak.Local v
	
and replace_exp e =
  match e with
    | Npkil.Lval (lv, sca) -> Newspeak.Lval (replace_lv lv, sca)
    | Npkil.Const c -> Newspeak.Const c 
    | Npkil.AddrOfFun (fid, ft) -> Newspeak.AddrOfFun (fid, replace_ftyp ft)
    | Npkil.AddrOf (lv, sz) -> 
	let sz = 
	  try Nat.to_int (replace_tmp_nat sz) 
	  with Invalid_argument "Newspeak.Nat.to_int" -> Config.max_sizeof
	in
	  if (sz > Config.max_sizeof) 
	  then Npkcontext.error "Link.replace_exp" 
	    ("size too large: maximum allowed is "
	     ^(string_of_int Config.max_sizeof)^" bits");
	  Newspeak.AddrOf (replace_lv lv, sz)
    | Npkil.UnOp (o, e) -> Newspeak.UnOp (replace_unop o, replace_exp e)
    | Npkil.BinOp (o, e1, e2) -> 
	Newspeak.BinOp (o, replace_exp e1, replace_exp e2)

and replace_unop o =
  match o with
      Npkil.Belongs_tmp (l, u) -> 
	let u = Nat.sub (replace_tmp_nat u) Nat.one in
	  Newspeak.Belongs (l, u)
    | Npkil.Coerce r -> Newspeak.Coerce r
    | Npkil.Not -> Newspeak.Not
    | Npkil.BNot r -> Newspeak.BNot r
    | Npkil.PtrToInt k -> Newspeak.PtrToInt k
    | Npkil.IntToPtr k -> Newspeak.IntToPtr k
    | Npkil.Cast (t1, t2) -> Newspeak.Cast (t1, t2)

and replace_tmp_nat x =
  match x with
      Npkil.Known i -> i
    | Npkil.Length name -> begin
	match get_glob_typ name with
	    Newspeak.Array (_, len) -> Nat.of_int len
	  | _ -> 
	      Npkcontext.error "Npklink.replace_tmp_nat" 
		"array type expected"
      end
    | Npkil.Mult (v, n) -> 
	let i = replace_tmp_nat v in
	  Nat.mul_int n i

and replace_fn fn =
  match fn with
    | Npkil.FunId f -> Newspeak.FunId f
    | Npkil.FunDeref (e, t) -> 
	Newspeak.FunDeref (replace_exp e, replace_ftyp t)

and replace_ftyp (args, ret) =
  let ret = 
    match ret with
	None -> None
      | Some t -> Some (replace_typ t)
  in
    (List.map replace_typ args, ret)

and replace_body body = List.map replace_stmt body

and replace_init init =
  match init with
    | None -> 
	if !Npkcontext.global_zero_init then Newspeak.Zero 
	else Newspeak.Init []
    | Some l -> Newspeak.Init (List.map replace_init_field l)

and replace_init_field (sz, sca, e) = 
  let e = replace_exp e in
  let e = 
    if !Npkcontext.no_opt then e 
    else Newspeak.simplify_exp !Npkcontext.opt_checks e 
  in
    (sz, sca, e)

and replace_typ t =
  match t with
      Npkil.Scalar x -> Newspeak.Scalar x
    | Npkil.Array (t, Some l) -> Newspeak.Array (replace_typ t, l)
    | Npkil.Array (_, None) -> 
	Npkcontext.error "Link.replace_typ" "Unknown array length"
    | Npkil.Region (fields, sz) -> 
	Newspeak.Region (List.map replace_field fields, sz)

and replace_field (offs, t) = (offs, replace_typ t)


(*
  TODO: implement --accept-extern
*)

let update_glob_link name (t, loc, init, used) =
  Npkcontext.set_loc loc;
  try
    let (prev_t, prev_loc, prev_init, prev_used) = 
      Hashtbl.find glb_decls name 
    in
      (* TODO: remove Npkil.compare_typs *)

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
		" (previous definition"^(Npkcontext.string_of_loc prev_loc)^")"
	      end
	    in
	    Npkcontext.report_accept_warning "Npklink.update_glob_link"
	      ("multiple definitions of global variable "^name
	       ^info) 
	      Npkcontext.MultipleDef;	      
	    prev_init
    in
    let loc = prev_loc in
      Hashtbl.replace glb_decls name (t, loc, init, used)
      
  with Not_found -> Hashtbl.add glb_decls name (t, loc, init, used)

let add_filename x = filenames := Str_set.add x !filenames

(* TODO: optimization, this is probably not efficient to read the whole
   program and then again a second time!!! reprogram Npkil.read and write *)
let merge_headers npko =
  let (fnames, globs, specs) = Npkil.read_header npko in
    List.iter add_filename fnames;
    Hashtbl.iter update_glob_link globs;
    add_specs specs

let generate_global name (t, loc, init, used) =
  Npkcontext.set_loc loc;
  if used || (not !Npkcontext.remove_temp) then begin
    let i =
      match init with
	| Some i -> i
	| None when !Npkcontext.accept_extern -> 
	    Npkcontext.print_warning "Npklink.handle_real_glob:" 
	      ("extern not accepted: "^name);
	    None
	| None -> 
	    Npkcontext.error "Npklink.handle_real_glob:" 
	      ("extern not accepted: "^name)
    in
    let t = replace_typ t in
      Hashtbl.add globals name (t, replace_init i)
  end

let write_fun cout f spec =
  Npkcontext.print_debug ("Writing function: "^f);
  Newspeak.write_fun cout f spec

let generate_funspecs cout npkos =
(* TODO: Use a String_set here, clean up sets *)
  let waiting = Hashtbl.create 100 in
  let encountered = Hashtbl.create 100 in  

  let handle_funspec name (ftyp, body) =
    let body =
      match body with
	| None -> None
	| Some b -> 
	    let body = replace_body b in
	    let body = 
	      if !Npkcontext.no_opt then body
	      else Newspeak.simplify !Npkcontext.opt_checks body
	    in
	    let body =
	      if !Npkcontext.no_opt then body
	      else if not !Npkcontext.normalize_loops then body
	      else Newspeak.normalize_loops body
	    in
	      Some body
    in
    let ftyp = replace_ftyp ftyp in
      
      try 
	let prev_ftyp = Hashtbl.find encountered name in
	  if (ftyp <> prev_ftyp) 
	  then Npkcontext.error "Npklink.generate_funspecs" 
	    ("Function "^name^" type does not match");
	  match body with
	      None -> ()
	    | Some body when Hashtbl.mem waiting name -> 
		Hashtbl.remove waiting name;
		write_fun cout name (ftyp, body)
	    | Some _ -> 
		Npkcontext.error "Npklink.generate_funspecs" 
		  ("Function "^name^" declared twice")
		  
      with Not_found -> 
	Hashtbl.add encountered name ftyp;
	match body with
	    None -> Hashtbl.add waiting name (ftyp, None)
	  | Some body -> write_fun cout name (ftyp, body)
  in
    
  let read_all_funspec npko =
    let funs = read_fundefs npko in
      Hashtbl.iter handle_funspec funs
  in
    List.iter read_all_funspec npkos
      

(* TODO: clean up *)
let link npkos output_file =
  let cout = open_out_bin output_file in
    Npkcontext.forget_loc ();
    
    Npkcontext.print_debug "Linking files...";
    List.iter merge_headers npkos;
    Npkcontext.print_debug "Globals...";
    
    Hashtbl.iter generate_global glb_decls;
    Npkcontext.forget_loc ();
    
    let filenames = Str_set.elements !filenames in

      Newspeak.write_hdr cout (filenames, globals, !specs, Config.size_of_ptr);
      
      Npkcontext.print_debug "Functions...";
      generate_funspecs cout npkos;
      
      close_out cout;
      
      Npkcontext.print_debug "File linked.";
      
      if !Npkcontext.verb_newspeak then begin
	print_endline "Newspeak output";
	print_endline "---------------";
	let npk = Newspeak.read output_file in
	  Newspeak.dump npk;
	  print_newline ()
      end
