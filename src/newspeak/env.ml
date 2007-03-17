open Cil
open Cilutils
open Npkcontext
open Npkutils


(*----------------------------*)
(* Useful, non exported stuff *)
(*----------------------------*)

(* Counter are always incremented by incr*)
let incr cnt = 
  if !cnt = max_int
  then error "Env.incr: too many objects";
  incr cnt;
  !cnt



(*-------*)
(* Types *)
(*-------*)

type status = {
  return_var : Newspeak.vid;
  return_lbl : Newspeak.lbl;
  switch_lbls: (Cil.location * Newspeak.lbl) list;
  brk_lbl    : Newspeak.lbl;
}

type glb_type = {
  gname : string;
  gtype : Cil.typ;
  gloc  : Cil.location;
  gdefd : bool;
  ginit : Cil.init option;
}

type fspec_type = {
  pname : string;
  mutable prett : Newspeak.typ option;
  mutable pargs : ((string * Newspeak.typ) list) option;
  mutable plocs : ((string * Newspeak.typ) list) option;
  mutable ploc  : Cil.location;
  mutable pbody : Newspeak.fundec option
}

type intermediate = {
  iglobs : (string, glb_type) Hashtbl.t;
  ifuns  : (Newspeak.fid, fspec_type) Hashtbl.t;
  iusedglbs : Npkutils.String_set.t;
  iusedcstr : Npkutils.String_set.t;
  iusedfuns : Npkutils.String_set.t;
}
      


(*-----------------------*)
(* Compilation variables *)
(*-----------------------*)

let glb_decls = Hashtbl.create 100
let fun_defs = ref []
let fun_specs = Hashtbl.create 100
let glb_used = ref (String_set.empty)
let fun_called = ref (String_set.empty)
let glb_cstr = ref (String_set.empty)



(*---------*)
(* Globals *)
(*---------*)
let glb_uniquename v =
  if not v.vglob
  then error "Env.glb_uniquename: global variable expected";
  if v.vstorage = Static
  then (get_cur_file())^"."^v.vname
  else v.vname



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

let loc_declare generate_stmt_decl v =
  let vid = incr loc_cnt in
    Hashtbl.add loc_tabl v.vid vid;
    if generate_stmt_decl then loc_decls :=
      ((translate_typ v.vtype, v.vname, Newspeak.Init []), translate_loc v.vdecl)::!loc_decls


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



let use_fun v =
  fun_called := String_set.add v.vname !fun_called

let extract_type ((t, _, _), _) = t



(*-----------------------*)
(* Variable id retrieval *)
(*-----------------------*)

let get_var cil_var =
  (* global variable *)
  if cil_var.vglob then begin
    let norm_name = glb_uniquename cil_var in
      glb_used := String_set.add norm_name !glb_used;
      Newspeak.Global_tmp norm_name
  end else begin
    (* local variables *)
    try
      let vid = Hashtbl.find loc_tabl cil_var.vid in
      let n = !loc_cnt - vid in
	Newspeak.Local n
    with Not_found -> 
      error ("Env.get_var: unexpected variable "^
	       (string_of_lval (Var cil_var, NoOffset)))
  end

let get_cstr s =
  glb_cstr := String_set.add s !glb_cstr;
  Newspeak.AddrOf (Newspeak.Global_tmp ("!const_str_"^s),
		   (String.length s) + 1)

let get_ret_var status = Newspeak.Local (!loc_cnt - status.return_var)



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


let dump_npko inter = 

  let print_list title list =
    print_endline title;
    String_set.iter print_endline list;
    print_newline ()
  in


    print_list "Global used" inter.iusedglbs;
    print_list "Functions called" inter.iusedfuns;
    print_list "Constant Strings" inter.iusedcstr;

    print_endline "Function definitions";
(* TODO: Rewrite this ! *)
(*    Newspeak.dump ([], [], inter.ifuns); *)
    print_newline ()

    

(*


(*---------*)
(* Globals *)
(*---------*)

(* Types and variables used for globals during the first pass, between
   the exploration of the globals and the generation of the declaration *)

(* The type *)
type glb_t = {
  gv_name         : string;
  gv_cstr         : string;
  mutable gv_ctyp : Cil.typ;
  mutable gv_cinit: Cil.init option;
  mutable gv_defd : bool;
  mutable gv_cloc : Cil.location;
  mutable gv_used : bool;
}

(* Association table (stdname -> glb_t) *)
let glb_tabl = Hashtbl.create 100

(* List of the global variables, by order of appearance *)
let glb_list = ref []
let cstr_list = ref []

(* Use a unique name "filename.vname" only for static variables 
   to differentiate them. *)
let glb_declare (v, defined, init) =
  let dispname =
    if v.vstorage = Static
    then (get_cur_file())^"."^v.vname
    else v.vname
  in
  let name = if !mergecil then string_of_int v.vid else dispname in
    try
      let glob = Hashtbl.find glb_tabl name in
	if not (compare_typs glob.gv_ctyp v.vtype)
	then error ("Env.glb_declare: different types for "^name^": '"^
		      (string_of_type glob.gv_ctyp)^"' and '"^
		      (string_of_type v.vtype)^"'");

	if (glob.gv_defd && defined) then begin
	  if (glob.gv_cinit <> None (* || TODO: or not option*)) 
	  then error ("Env.glb_declare: multiple definition for "^name);
	  print_warning ("Env.glb_declare: multiple declaration for "^name)
	end;

	if defined then begin
	  glob.gv_ctyp <- v.vtype;
	  glob.gv_cinit<- init;
	  glob.gv_cloc <- v.vdecl;
	  glob.gv_defd <- true
	end
    with Not_found ->
      glb_list := name::(!glb_list);
      let glob =
	if defined then
	  {gv_name = dispname;
	   gv_cstr = "";
	   gv_ctyp = v.vtype;
	   gv_cinit= init;
	   gv_defd = true; 
	   gv_cloc = v.vdecl;
	   gv_used = !mergecil || not !remove_temp ;}
	else
	  {gv_name = dispname;
	   gv_cstr = "";
	   gv_ctyp = v.vtype;
	   gv_cinit= None;
	   gv_defd = false; 
	   gv_cloc = locUnknown;
	   gv_used = !mergecil || not !remove_temp;}
      in
	Hashtbl.replace glb_tabl name glob 




(* Functions used in the first pass *)

let glb_uses v =
  if v.vglob && not !mergecil then begin
    let filename = get_cur_file() in
    let glob = 
      try
	try
	  Hashtbl.find glb_tabl (filename^"."^v.vname)
	with Not_found -> Hashtbl.find glb_tabl v.vname
      with Not_found -> 
	error ("Env.glb_uses: unexpected global variable: "
	       ^filename^"."^v.vname)
    in
      glob.gv_used <- true;
      v.vtype <- glob.gv_ctyp
  end
(* TODO: after the merge: check variables that are used but never defined!! 
*)
(* TODO: this code is replaced, because can not be done in the first
   pass anymore.
   Should be put somewhere else anyway. 
*)
(*
      if glob.gv_defd || !accept_extern then begin
	glob.gv_used <- true;
	v.vtype <- glob.gv_ctyp
      end 
      else error ("Env.glb_uses: global variable "
		  ^v.vname^" is used but is never defined")
*)





(* Final association table (stdname -> Newspeak.vid) *)
let glb_tabl_vid = Hashtbl.create 100

(* TODO: This is a hack for assumptions *)
let glb_tabl_typ = Hashtbl.create 100

(* Removes temp and gives each glb a number *)
let get_glb_decls_inits translate_exp =
  let total_nb = ref 0 in
  let processed = ref 0 in
  let glb_cnt = ref 0 in
  let accu = ref [] in
    
  let extract_cstrdecl name =
    let glb_decl = Hashtbl.find glb_tabl name in
    (* glb_decl.gv_used is always true *)
    let vid = incr glb_cnt in
      Hashtbl.add glb_tabl_vid name vid;
      let (len, str) = Newspeak.init_of_string glb_decl.gv_cstr in
      let char_sca = Newspeak.Int (Newspeak.Signed, char_size) in
      let t = Newspeak.Array (Newspeak.Scalar char_sca, len) in
      let i = Newspeak.Init str in
	accu := (t, glb_decl.gv_name, i)::(!accu)
  in

  let translate_init cil_loc cil_t init =
    let glb_inits = ref [] in
      
    let rec expand off i =
      match i with
	  SingleInit e ->
	    let o = offset_of cil_t off in begin
		match translate_typ (typeOf e) with
		    Newspeak.Scalar s -> glb_inits := (o, s, translate_exp e)::(!glb_inits)
		  | _ -> error "Env.translate_init: unexpected type of SingleInit"
	      end;
	| CompoundInit (_, c) -> List.iter (expand_elem off) c
    and expand_elem prefix (off, i) = expand (addOffset off prefix) i in
      
      match init with
	| None ->
	    if !global_zero_init
	    then Newspeak.Zero
	    else Newspeak.Init []
	| Some i -> 
	    update_loc cil_loc;
	    expand NoOffset i;
            Newspeak.Init (List.rev (!glb_inits))
  in

  let extract_glbdecl name =
    let glb_decl = try
      Hashtbl.find glb_tabl name 
    with Not_found -> error ("Env.get_var: unexpected error")
    in
      if glb_decl.gv_used then begin
	update_loc glb_decl.gv_cloc;
	let vid = incr glb_cnt in
	  Hashtbl.add glb_tabl_vid name vid;
	  let t = translate_typ glb_decl.gv_ctyp in
	    (* TODO: this is a hack for assumptions, rethink *)
	    if (!Npkcontext.assumptions <> []) 
	    then begin
	      match t with
		  Newspeak.Scalar t -> Hashtbl.add glb_tabl_typ name t
		| _ -> ()
	    end;
	  let i = 
	    translate_init glb_decl.gv_cloc glb_decl.gv_ctyp glb_decl.gv_cinit 
	  in
	    accu := (t, glb_decl.gv_name, i)::(!accu)
      end;
      if !verb_debug then begin
	processed := !processed + 1;
	let progress = !processed*100/(!total_nb) in
	  prerr_string ("Progress: "^(string_of_int progress)^"%\n")
      end
  in
    List.iter extract_cstrdecl (List.rev !cstr_list);
    print_debug "Processing global variables";
    total_nb := List.length !glb_list;
    print_debug ("Number of global variables: "^(string_of_int !total_nb));
    List.iter extract_glbdecl (List.rev !glb_list);
    print_debug "Global variables processing over";
    Hashtbl.clear glb_tabl;
    glb_list := [];
    cstr_list := [];
    List.rev !accu




(*----------*)
(* Function *)
(*----------*)

let get_fun_spec name =
  try
    Some (Hashtbl.find fun_specs name)
  with Not_found -> None


(* Updates the data about a function *)
let update_fun_spec name ret_type formals locals body fun_loc =
  match (get_fun_spec name) with
    | None ->
	Hashtbl.add fun_specs name {ret_type = ret_type;
				    formals = formals; locals = locals;
				    body = body; fun_loc = fun_loc}
    | Some spec ->
	let update_ret_type () = 
	  match spec.ret_type, ret_type with
	    | _ , None -> ()
	    | None, Some _ -> spec.ret_type <- ret_type
	    | Some t1, Some t2 when t1 = t2 -> ()
	    | _ -> error ("Env.update_fun_spec: multiple declaration for \""
			  ^name^"\" are incompatible")

	and update_formals () =
	  let rec compare_formals f1 f2 =
	    match f1,f2 with
	      | [],[] -> []
	      | ({var_decl = (t1, n1, Newspeak.Init []); var_cil_vid = i1; var_loc = l1;} as decl1)::r1,
		  {var_decl = (t2, n2, Newspeak.Init []); var_cil_vid = i2; var_loc = l2;}::r2
		    when t1 = t2 && (i1 = i2 || i1 = 0 || i2 = 0) ->
		  {decl1 with var_cil_vid = max i1 i2;
		     var_loc = if l2 = Newspeak.locUnknown then l1 else l2}::(compare_formals r1 r2)
		    
	      | _ -> error ("Env.update_fun_spec: multiple declaration for \""
			    ^name^"\" are incompatible")
	  in
	    match spec.formals, formals with
		None, Some f -> spec.formals <- formals
	      | _, None -> ()
	      | Some f1, Some f2 ->
		  spec.formals <- Some (compare_formals f1 f2)

	and update_locals () =
	  match spec.locals, locals with
	    | _, [] -> ()
	    | [], l -> spec.locals <- locals
	    | _ -> error ("Env.update_fun_spec: multiple definition "
			  ^"for \""^name^"\" (two lists of locals)")

	and update_body () =
	  match spec.body, body with
	  | b, [] -> ()
	  | [], b -> spec.body <- b
	  | _ -> error ("Env.update_fun_spec: multiple definition "
			^"for \""^name^"\" (two different bodies)")

	and update_loc () =
	  match spec.fun_loc, fun_loc with
	    | None, Some _ -> spec.fun_loc <- fun_loc

	    (* if we have two locations, we update it if the new spec
	       corresponds to the definition *)
	    | _, Some l when body <> [] -> spec.fun_loc <- fun_loc
	    | _ -> ()

	in
	  update_ret_type ();
	  update_formals ();
	  update_locals ();
	  update_body ();
	  update_loc ()




(* Function declarations *)
(*-----------------------*)

(* Case of a definition *)
let fun_declare f =
  let ret_type = match f.svar.vtype with
    | TFun (TVoid _, _, _, _) -> None
    | TFun (t, _, _, _) -> Some (translate_typ t)
    | _ ->
	error ("Env.fun_declare: invalid type \""
	       ^(string_of_type f.svar.vtype)^"\"")
  in
  let translate_local used f =
    if f.vstorage=Static
    then error ("fun_declare.translate_local: static storage not handled yet")
    else new_decl ((translate_typ f.vtype), f.vname, (Newspeak.Init [])) f.vid !cur_loc used
  in
  let formals = Some (List.map (translate_local true) f.sformals) in
  let locals = List.map (translate_local false) f.slocals in
    update_fun_spec f.svar.vname ret_type formals locals f.sbody.bstmts (Some (!cur_loc))


(* Case of a prototype *)
let fun_declare_prototype (name, ret, args) =
  let ret_type = match ret with
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
	    (new_decl ((translate_typ t), name, (Newspeak.Init [])) 0 Newspeak.locUnknown true)::(translate_formals (i+1) r)
  in
  let formals = match args with
      None -> None
    | Some l -> Some (translate_formals 0 l)
  in
    if (args = None)
    then print_warning ("missing or incomplete prototype for "^name);
    update_fun_spec name ret_type formals [] [] None



*)



(*let get_glb_var vname =
  try 
    (* Static global variables *)
    try
      let kvid = Hashtbl.find glb_tabl_vid (get_cur_file()^"."^vname) in
	Newspeak.Global kvid
    with Not_found ->
      (* Global variables *)
      let kvid = Hashtbl.find glb_tabl_vid vname in
	Newspeak.Global kvid
  with Not_found -> error ("Env.get_glb_var: invalid vid for "^vname)

let get_glb_typ vname =
  try 
    (* Static global variables *)
    try Hashtbl.find glb_tabl_typ (get_cur_file()^"."^vname)
    with Not_found ->
      (* Global variables *)
      Hashtbl.find glb_tabl_typ vname
  with Not_found -> error ("Env.get_glb_typ: invalid vid for "^vname)




let get_ret_var status = Newspeak.Local (!loc_cnt - status.return_var)
*)




