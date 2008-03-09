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

type funinfo = {
  ploc  : Newspeak.location;
  mutable fargs : (string * typ) list option;
  frett : typ option;
  mutable pbody : blk option;
}

(*-----------*)
(* Constants *)
(*-----------*)

let return_lbl = 0

let cnt_lbl = 1

let brk_lbl = 2

(* TODO: continue ?? *)

(*-------*)
(* Types *)
(*-------*)

(* maps every location corresponding to a case statement of a switch
   to a newspeak label 
*)
type status = (Cil.location * Newspeak.lbl) list

(*-----------------------*)
(* Compilation variables *)
(*-----------------------*)

let glb_decls = Hashtbl.create 100
(* TODO: code cleanup: fusion fun_specs and fun_args in one table *)
let fun_specs = Hashtbl.create 100

let get_funspec name =
  assert (Hashtbl.mem fun_specs name);
  let info = Hashtbl.find fun_specs name in
    (info.ploc, info.frett)

let update_funspec name (args, body) =
  assert (Hashtbl.mem fun_specs name);
  let info = Hashtbl.find fun_specs name in
    info.fargs <- args;
    info.pbody <- Some body

(* This table to avoid 
   recomputing a different string here to improve sharing *)
let static_glb_names = Hashtbl.create 100

let init_env () =
  Hashtbl.clear glb_decls;
  Hashtbl.clear static_glb_names;
  Hashtbl.clear fun_specs

(* TODO: code optimization, why copy ? *)
let create_npkil name = 
  let fundefs = Hashtbl.create 100 in
  let filter_arg (_, t) = t in
  let add_funinfo f x = 
    match (x.fargs, x.pbody) with
	(Some args, _) -> 
	  let args = List.map filter_arg args in
	    Hashtbl.add fundefs f ((args, x.frett), x.pbody)

     (* In this case, the function is a prototype which is never called: 
	ignore *)
      | (None, None) -> ()
     (* if the functions arguments are undetermined, then the function
	is necessarily a prototype *)
      | (None, _) -> error "Env.create_npkil" "Code unreachable"
  in
    Hashtbl.iter add_funinfo fun_specs;
    (name::[], Hashtbl.copy glb_decls, fundefs, [])

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
    let str = (Npkcontext.get_fname ())^"."^v.vname in
      Hashtbl.add static_glb_names v str;
      str

(*--------*)
(* Locals *)
(*--------*)


(* TODO: put loc_cnt and loc_tabl inside the status, 
   thus removing some unecessary global variables *)
(* Counter *)
let loc_cnt = ref 0

(* Association table Cil.vid -> Newspeak.vid *)
let loc_tabl = Hashtbl.create 100

(* This reference keeps the old counter when translating a call. The
   programmer must check that only one save can be made at a time*)
let loc_cnt_sav = ref 0

let push_local () = ignore (incr loc_cnt)


(* Functions used in translate_fun *)
(*---------------------------------*)

let loc_declare (cil_vid, _, _, _) =
  let vid = incr loc_cnt in
    Hashtbl.add loc_tabl cil_vid vid

(* Functions used in translate_call *)
(*----------------------------------*)

let save_loc_cnt () = loc_cnt_sav := !loc_cnt

let restore_loc_cnt () = loc_cnt := !loc_cnt_sav



(*----------*)
(* Function *)
(*----------*)


let extract_ldecl (_, n, t) = (n, t)

let compare_formals name l1 l2 =
  let res = ref false in
  let rec compare_aux l1 l2 =
    match l1, l2 with
	[], [] -> ()
      | (str, t1)::r1, (_, t2)::r2 when t1 = t2 ->
	  if (str = "") then res := true;
	  compare_aux r1 r2

      | _ ->
	  (* TODO: add the respective locations *)
	error "Npkenv.compare_formals"
	  ("Different types for function "^name)
  in
    compare_aux l1 l2;
    !res

let update_fun_proto name (args, ret) =
  if (args = None) then begin
    print_warning "Npkenv.translate_formals"
      ("missing or incomplete prototype for "^name)
  end;
(* TODO: code cleanup necessary for cilcompiler, because much 
   effort done multiple times *)
  try
    let x = Hashtbl.find fun_specs name in
      
    let _ = 
      match x.frett, ret with
	  None, None -> ()
	| Some t1, Some t2 when t1 = t2 -> ()
	| _ ->
	    (* TODO: add the respective types and locations ? *)
	    error "Npkenv.update_fun_proto"
	      ("different types for return type of prototype "^name)
    in
      
    let _ =
      match x.fargs, args with
	| _, None -> ()
	| None, Some _ -> x.fargs <- args
	| Some l1, Some l2 -> 
	    if (compare_formals name l1 l2) then x.fargs <- args
    in ()

  with Not_found ->
    Hashtbl.add fun_specs name 
      { 
	frett = ret;
	fargs = args;
	ploc = Npkcontext.get_loc ();
	pbody = None;
      }

let get_args f = 
  let funinfo = Hashtbl.find fun_specs f in
  let get (str, _) = 
    if str = "" then raise Not_found;
    str
  in
    match funinfo.fargs with
	None -> raise Not_found
      | Some args -> List.map get args


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

let get_cstr str =
  let (name, glb) = Npkil.create_cstr str in
    if not (Hashtbl.mem glb_decls name) then Hashtbl.add glb_decls name glb;
    (* TODO: this is a hack, should return a cil expression rather,
       or even better should be done in first pass *)
    Npkil.AddrOf (Npkil.Global name, 
		 Npkil.Known (((String.length str) + 1) * 8))

let get_ret_var () = Npkil.Local (!loc_cnt - 1)

let get_ret_lbl () = return_lbl

let get_cnt_lbl () = cnt_lbl

let get_brk_lbl () = brk_lbl

(*------------------------------------------*)
(* Status "Constructors" and label handling *)
(*------------------------------------------*)

(* Counter for labels *)
let lbl_cnt = ref brk_lbl

let reset_lbl_gen () = lbl_cnt := brk_lbl

let empty_status () = 
  loc_cnt := 0;
  Hashtbl.clear loc_tabl;
  []

let new_lbl () = incr lbl_cnt

let add_switch_lbl status loc lbl = (loc, lbl)::status

let get_switch_lbl status loc = List.assoc loc status

let mem_switch_lbl status loc = List.mem_assoc loc status
