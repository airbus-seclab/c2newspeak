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
open Npkil

(*-------*)
(* Types *)
(*-------*)

type status = {
  return_var : Newspeak.vid;
  return_lbl : Newspeak.lbl;
  switch_lbls: (Cil.location * Newspeak.lbl) list;
  brk_lbl    : Newspeak.lbl;
}

(*-----------------------*)
(* Compilation variables *)
(*-----------------------*)

let glb_decls = Hashtbl.create 100
let fun_specs = ref (Hashtbl.create 100)
let glb_cstr = ref (String_set.empty)
(* This table to avoid 
   recomputing a different string here to improve sharing *)
let static_glb_names = Hashtbl.create 100

let init_env () =
  Hashtbl.clear glb_decls;
  Hashtbl.clear static_glb_names;
  Hashtbl.clear !fun_specs;
  glb_cstr := String_set.empty

let create_npkil name =
  let globs = 
    { 
      ifilename = name;
      iglobs = Hashtbl.copy glb_decls;
      iusedcstr = !glb_cstr
    }
  in
  let funs = Hashtbl.copy !fun_specs in
    (globs, funs)

(*---------*)
(* Globals *)
(*---------*)
let glb_uniquename v =
  if not v.vglob
  then error "Npkenv.glb_uniquename" "global variable expected";
  try
    if v.vstorage = Static
    then Hashtbl.find static_glb_names v
    else v.vname
  with Not_found ->
    let str = (get_cur_file())^"."^v.vname in
      Hashtbl.add static_glb_names v str;
      str

(*--------*)
(* Locals *)
(*--------*)


(* Counter *)
let loc_cnt = ref 0

(* Association table Cil.vid -> Newspeak.vid *)
let loc_tabl = Hashtbl.create 100

(* List of current declarations: the list grows as loc_declare is
   called, and is emptied when retrieved by get_loc_decls *)
let loc_decls = ref []

(* This reference keeps the old counter when translating a call. The
   programmer must check that only one save can be made at a time*)
let loc_cnt_sav = ref 0

let push_local () = ignore (incr loc_cnt)


(* Functions used in translate_fun *)
(*---------------------------------*)

let loc_declare generate_stmt_decl (cil_vid, n, t) =
  let vid = incr loc_cnt in
    Hashtbl.add loc_tabl cil_vid vid;
    if generate_stmt_decl then loc_decls := (n, t, !cur_loc)::!loc_decls


let get_loc_decls () =
  let res = !loc_decls in
    loc_decls := [];
    loc_cnt := 0;
    Hashtbl.clear loc_tabl;
    res


(* Functions used in translate_call *)
(*----------------------------------*)

let save_loc_cnt () = loc_cnt_sav := !loc_cnt

let restore_loc_cnt () = loc_cnt := !loc_cnt_sav



(*----------*)
(* Function *)
(*----------*)


let extract_ldecl (_, n, t) = (n, t)


let compare_formals name l1 l2 =
  let rec compare_aux l1 l2 =
    match l1, l2 with
	[], [] -> ()
      | (_, _, t1)::r1, (_, _, t2)::r2 when t1 = t2 ->
	  compare_aux r1 r2
      | [], _ | _, [] ->
	  (* TODO: add the respective locations *)
	error "Npkenv.compare_formals"
	  ("different number of args in declarations for function "^name)
	  
      | (_, _, t1)::_, (_, n, t2)::_ ->
	  (* TODO: add the respective locations *)
	  error "Npkenv.compare_formals"
	    ("different types for argument "^n^" in different "
	     ^"declarations of "^name^": '"^(Npkil.string_of_typ t1)
	     ^"' and '"^(Npkil.string_of_typ t2)^"'")
  in
    compare_aux l1 l2

let update_fun_proto name ret args =
  let rettype = 
    match ret with
      | TVoid _ -> None
      | t -> Some (translate_typ t)
  in

  let rec translate_formals i l =
    match l with
	[] -> []
      | (n, t, _)::r ->
	  let name =
	    if n="" then "arg" ^ (string_of_int i) else n
	  in
	    (-1, name, translate_typ t)::(translate_formals (i+1) r)
  in
  let formals = 
    match args with
	None ->
	  print_warning "Npkenv.update_fun_proto"
	    ("missing or incomplete prototype for "^name);
	  None
      | Some l -> Some (translate_formals 0 l)
  in
 
    try
      let x = Hashtbl.find !fun_specs name in

      let _ = 
	match x.prett, rettype with
	    None, None -> ()
	  | Some t1, Some t2 when t1 = t2 -> ()
	  | _ ->
	      (* TODO: add the respective types and locations ? *)
	      error "Npkenv.update_fun_proto"
		("different types for return type of prototype "^name)
      in

      let _ =
	match x.pargs, formals with
	  | _, None -> ()
	  | None, Some _ -> x.pargs <- formals
	  | Some l1, Some l2 -> compare_formals name l1 l2
      in ()
    with Not_found ->
      Hashtbl.add !fun_specs name
	{prett = rettype;
	 pargs = formals; plocs = None;
	 ploc = !cur_loc; pbody = None;
	 pcil_body = None;}






(*-----------------------*)
(* Variable id retrieval *)
(*-----------------------*)

let get_var cil_var =
  (* global variable *)
  if cil_var.vglob then begin
    let norm_name = glb_uniquename cil_var in
      Npkil.Global norm_name
  end else begin
    (* local variables *)
    try
      let vid = Hashtbl.find loc_tabl cil_var.vid in
      let n = !loc_cnt - vid in
	Npkil.Local n
    with Not_found -> 
      error "Npkenv.get_var"
	("unexpected variable "^(string_of_lval (Var cil_var, NoOffset)))
  end

let get_cstr s =
  Npkil.AddrOf (Npkil.Global ("!const_str_"^s), 
		Npkil.Known ((String.length s) + 1))

let get_ret_var status = Npkil.Local (!loc_cnt - status.return_var)



(*------------------------------------------*)
(* Status "Constructors" and label handling *)
(*------------------------------------------*)

(* Counter for labels *)
let lbl_cnt = ref 0

let empty_status () =
  {return_var = -1; return_lbl = incr lbl_cnt;
   switch_lbls = []; brk_lbl = -1;}

let new_ret_status () =
  {return_var = !loc_cnt; return_lbl = incr lbl_cnt;
   switch_lbls = []; brk_lbl = -1;}

let new_brk_status status = {status with brk_lbl = incr lbl_cnt}

let new_label () = incr lbl_cnt

let add_switch_label status loc new_lbl =
  {status with switch_lbls = (loc, new_lbl)::status.switch_lbls}

let retrieve_switch_label status loc =
  List.assoc loc status.switch_lbls

let mem_switch_label status loc =
  List.mem_assoc loc status.switch_lbls







(* TODO: check that we still can accept extern *)
(*
      if glob.gv_defd || !accept_extern then begin
	glob.gv_used <- true;
	v.vtype <- glob.gv_ctyp
      end 
      else error ("Npkenv.glb_uses: global variable "
		  ^v.vname^" is used but is never defined")
*)
