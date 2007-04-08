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
let fun_specs = Hashtbl.create 100
let glb_used = ref (String_set.empty)
let fun_called = ref (String_set.empty)
let glb_cstr = ref (String_set.empty)

let init_env () =
  Hashtbl.clear glb_decls;
  Hashtbl.clear fun_specs;
  glb_used := String_set.empty;
  fun_called := String_set.empty;
  glb_cstr := String_set.empty

let create_npkil name =
  { 
    ifilename = name;
    iglobs = Hashtbl.copy glb_decls;
    ifuns = Hashtbl.copy fun_specs;
    iusedglbs = !glb_used;
    iusedcstr = !glb_cstr;
    iusedfuns = !fun_called
  }

(*---------*)
(* Globals *)
(*---------*)
let glb_uniquename v =
  if not v.vglob
  then error "Npkenv.glb_uniquename" "global variable expected";
  if v.vstorage = Static
  then (get_cur_file())^"."^v.vname
  else v.vname


let update_glob_decl v =
  let name = glb_uniquename v in
    try
      let x = Hashtbl.find glb_decls name in
	if not (compare_typs x.gtype v.vtype)
	  (* TODO: add the respective locations *)
	then error "Npkenv.update_glob_decl"
	  ("different types for "^name^": '"
	  ^(string_of_type x.gtype)^"' and '"
	  ^(string_of_type v.vtype)^"'")
    with Not_found ->
      Hashtbl.add glb_decls name
	{gtype = v.vtype; gloc = translate_loc v.vdecl;
	 gdefd = false; ginit = None;}

let update_glob_def v i =
  let name = glb_uniquename v in
    try
      let x = Hashtbl.find glb_decls name in
	if not (compare_typs x.gtype v.vtype)
	  (* TODO: add the respective locations *)
	then error "Npkenv.update_glob_decl"
	  ("different types for "^name^": '"
	  ^(string_of_type x.gtype)^"' and '"
	  ^(string_of_type v.vtype)^"'");
	if x.gdefd (* Should there be an exception here ? *)
	then error "Npkenv.glb_declare" ("multiple definition for "^name);
	x.gtype <- v.vtype;
	x.gdefd <- true;
	x.gloc <- translate_loc v.vdecl;
	x.ginit <- i
    with Not_found ->
      Hashtbl.add glb_decls name
	{gtype = v.vtype; gloc = translate_loc v.vdecl;
	 gdefd = true; ginit = i;}

  


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



let use_fun v =
  fun_called := String_set.add v.vname !fun_called

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
	     ^"declarations of "^name^": '"^(Newspeak.string_of_typ t1)
	     ^"' and '"^(Newspeak.string_of_typ t2)^"'")
  in
    compare_aux l1 l2

let update_fun_proto name ret args =
  let rettype = match ret with
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
  let formals = match args with
      None ->
	print_warning "Npkenv.update_fun_proto"
	  ("missing or incomplete prototype for "^name);
	None
    | Some l -> Some (translate_formals 0 l)
  in
 
    try
      let x = Hashtbl.find fun_specs name in

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
      Hashtbl.add fun_specs name
	{prett = rettype;
	 pargs = formals; plocs = None;
	 ploc = !cur_loc; pbody = None;
	 pcil_body = None;}


let update_fun_def f =
  let name = f.svar.vname in

  let rettype = match f.svar.vtype with
    | TFun (TVoid _, _, _, _) -> None
    | TFun (t, _, _, _) -> Some (translate_typ t)
    | _ ->
	error "Npkenv.update_fun_def"
	  ("invalid type \""^(string_of_type f.svar.vtype)^"\"")
  in

  let translate_local v = v.vid, v.vname, translate_typ v.vtype in
  let formals = List.map translate_local f.sformals in
  let locals = List.map translate_local f.slocals in

  try
    let x = Hashtbl.find fun_specs name in
      if x.pcil_body <> None
      then error "Npkenv.update_fun_def"
	("multiple definition for "^name);

      let _ = 
	match x.prett, rettype with
	    None, None -> ()
	  | Some t1, Some t2 when t1 = t2 -> ()
	  | _ ->
	      (* TODO: add the respective types and locations ? *)
	      error "Npkenv.update_fun_def"
		("different types for return type of prototype "^name)
      in

      let _ =
	match x.pargs with
	  | None -> ()
	  | Some l ->
	      compare_formals name l formals;

      in
	x.pargs <- Some formals;
	x.plocs <- Some locals;
	x.ploc <- !cur_loc;
	x.pcil_body <- Some f.sbody

    with Not_found ->
      Hashtbl.add fun_specs name
	{prett = rettype;
	 pargs = Some formals; plocs = Some locals;
	 ploc = !cur_loc; pbody = None;
	 pcil_body = Some f.sbody;}




(*-----------------------*)
(* Variable id retrieval *)
(*-----------------------*)

let register_var cil_var =
  match cil_var.vtype with
    | TFun _ -> ()
    | _ -> let norm_name = glb_uniquename cil_var in
	glb_used := String_set.add norm_name !glb_used

let register_cstr s =
  glb_cstr := String_set.add s !glb_cstr

let get_var cil_var =
  (* global variable *)
  if cil_var.vglob then begin
    let norm_name = glb_uniquename cil_var in
      Newspeak.Global_tmp norm_name
  end else begin
    (* local variables *)
    try
      let vid = Hashtbl.find loc_tabl cil_var.vid in
      let n = !loc_cnt - vid in
	Newspeak.Local n
    with Not_found -> 
      error "Npkenv.get_var"
	("unexpected variable "^(string_of_lval (Var cil_var, NoOffset)))
  end

let get_cstr s =
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







(*--------------*)
(* Linking time *)
(*--------------*)

let update_glob_link name g =
  try
    let x = Hashtbl.find glb_decls name in
      if not (compare_typs x.gtype g.gtype)
	(* TODO: add the respective locations *)
      then error "Npkenv.update_glob_link"
	("different types for "^name^": '"
	 ^(string_of_type x.gtype)^"' and '"
	 ^(string_of_type g.gtype)^"'");
      match g, x with
	  {ginit = None}, _ when not g.gdefd && x.gdefd -> ()
	  | _, {ginit = None} when g.gdefd && not x.gdefd ->
	      Hashtbl.replace glb_decls name g
	  | _ when not x.gdefd && not g.gdefd -> ()
	  | _ -> error "Npkenv.update_glob_link"
	      ("multiple definition of "^name);
  with Not_found ->
    Hashtbl.add glb_decls name g

let update_fun_link name f =
  try
    let x = Hashtbl.find fun_specs name in
      
    let _ = 
      match x.prett, f.prett with
	  None, None -> ()
	| Some t1, Some t2 when t1 = t2 -> ()
	| _ ->
	    (* TODO: add the respective types and locations ? *)
	    error "Npkenv.update_fun_link"
	      ("different types for return type of prototype "^name)
    in
      
    let _ =
      match x.pcil_body, f.pcil_body with
	| None, None -> ()
	| _ -> error "Npkenv.update_fun_link" ("unexpected error for "^name)
    in
      
    let _ =
      match x.pargs, f.pargs, x.plocs, f.plocs, x.ploc, f.ploc, x.pbody, f.pbody with
	| _, None, _, None, _, _, _, None -> ()
	| None, Some _, None, _, _, _, None, _ ->
	    x.pargs <- f.pargs;
	    x.plocs <- f.plocs;
	    x.ploc <- f.ploc;
	    x.pbody <- f.pbody
	| Some l1, Some l2, _, None, _, _, _, None ->
	    compare_formals name l1 l2
	| Some l1, Some l2, None, _, _, _, None, _ ->
	    compare_formals name l1 l2;
	    x.pargs <- f.pargs;
	    x.plocs <- f.plocs;
	    x.ploc <- f.ploc;
	    x.pbody <- f.pbody
	      (* TODO: Produce more precise errors *)
	| _ -> error "Npkenv.update_fun_link" ("unexpected error for "^name)
    in ()
	 
  with Not_found ->
    Hashtbl.add fun_specs name f




(* Counter *)
let glb_cnt = ref 0

(* Association table stdname -> Newspeak.vid *)
let glb_tabl_vid = Hashtbl.create 100

(* Association table stdname -> Newspeak.typ *)
let glb_tabl_typ = Hashtbl.create 100

let glist = ref []




(* Useful functions for final output *)
let get_glob_vid name =
  try
    Hashtbl.find glb_tabl_vid name
  with
      Not_found ->
	error "Npkenv.get_glob_vid" ("global variable "^name^" not found")

let get_glob_typ name =
  try
    Hashtbl.find glb_tabl_typ name
  with
      Not_found ->
	error "Npkenv.get_glob_vid" ("type for global variable "^name^" not found")

let rec replace_stmt (sk, l) =
  let new_sk = match sk with
    | Newspeak.Set (lv, e, sca) -> Newspeak.Set (replace_lv lv, replace_exp e, sca)
    | Newspeak.Copy (lv1, lv2, sz) -> Newspeak.Copy (replace_lv lv1, replace_lv lv2, sz)
    | Newspeak.Decl (name, t, b) -> 
	Newspeak.Decl (name, t, List.map replace_stmt b)
    | Newspeak.ChooseAssert l -> Newspeak.ChooseAssert (List.map replace_chooseitem l)
    | Newspeak.InfLoop b -> Newspeak.InfLoop (List.map replace_stmt b)
    | Newspeak.Call fn -> Newspeak.Call (replace_fn fn)
    | Newspeak.Goto _ | Newspeak.Label _ -> sk
  in (new_sk, l)
       
and replace_chooseitem (exps, b) =
  (List.map replace_exp exps, List.map replace_stmt b)
    
and replace_lv lv =
  match lv with
    | Newspeak.Global_tmp name -> Newspeak.Global (get_glob_vid name)
    | Newspeak.Deref (e, sz) -> Newspeak.Deref (replace_exp e, sz)
    | Newspeak.Shift (lv', e) -> Newspeak.Shift (replace_lv lv', replace_exp e)
    | Newspeak.Shift_tmp (name, e) -> begin
	let vid = get_glob_vid name in
	  match get_glob_typ name with
	      Newspeak.Array (t, len) ->
		let sz = Newspeak.size_of t in
		let index_exp =
		  Newspeak.BinOp (Newspeak.MultI,
				 Newspeak.make_belongs len (replace_exp e),
				 Newspeak.exp_of_int sz)
		in
		let v = Newspeak.Global (vid) in
		  Newspeak.Shift (v, index_exp)
	    | _ -> error "Npkenv.replace_lval"
		("type of lval "^(Newspeak.string_of_lval lv)
		  ^" is not defined enough")
      end
    | Newspeak.Local _ | Newspeak.Global _ -> lv
	
and replace_exp e =
  match e with
    | Newspeak.Lval (lv, sca) -> Newspeak.Lval (replace_lv lv, sca)
    | Newspeak.Const _ | Newspeak.AddrOfFun _ -> e
    | Newspeak.AddrOf (lv, sz) -> Newspeak.AddrOf (replace_lv lv, sz)
    | Newspeak.UnOp (o, e) -> Newspeak.UnOp (o, replace_exp e)
    | Newspeak.BinOp (o, e1, e2) -> Newspeak.BinOp (o, replace_exp e1, replace_exp e2)
	
and replace_fn fn =
  match fn with
    | Newspeak.FunId _ -> fn
    | Newspeak.FunDeref (e, t) -> Newspeak.FunDeref (replace_exp e, t)

and replace_body body = List.map replace_stmt body

and replace_inits init =
  match init with
    | Newspeak.Zero -> Newspeak.Zero
    | Newspeak.Init l -> Newspeak.Init (List.map replace_init l)

and replace_init (sz, sca, e) = (sz, sca, replace_exp e)



let handle_real_glob translate_exp g_used name g =

  let translate_init loc t init =
    let glb_inits = ref [] in
      
    let rec expand off i =
      match i with
	  SingleInit e ->
	    let o = offset_of t off in begin
	      try
		match translate_typ (typeOf e) with
		    Newspeak.Scalar s -> glb_inits := (o, s, translate_exp e)::(!glb_inits)
		  | _ -> error "Npkenv.translate_init" "unexpected type of SingleInit"
	      with LenOfArray ->
		error "Npkenv.translate_init"
		  ("unspecified length for global array "^name)
	      end;
	| CompoundInit (_, c) -> List.iter (expand_elem off) c
    and expand_elem prefix (off, i) = expand (addOffset off prefix) i in
      
      match init with
	| None ->
	    if !global_zero_init
	    then Newspeak.Zero
	    else Newspeak.Init []
	| Some i -> 
	    cur_loc := loc;
	    expand NoOffset i;
            Newspeak.Init (List.rev (!glb_inits))
  in

  if (String_set.mem name g_used) || not !remove_temp then begin
    try
      let t = translate_typ g.gtype in
	glist := (name, t, translate_init g.gloc g.gtype g.ginit)::(!glist);
	let vid = incr glb_cnt in
	  Hashtbl.add glb_tabl_vid name vid;
	  Hashtbl.add glb_tabl_typ name t
    with LenOfArray ->
      error "Npkenv.handle_real_glob"
	("unspecified length for global array "^name)
  end
    
let handle_cstr str =
  let name = ("!const_str_"^str) in 
  let (len, str) = Newspeak.init_of_string str in
  let char_sca = Newspeak.Int (Newspeak.Signed, char_size) in
  let t = Newspeak.Array (Newspeak.Scalar char_sca, len) in
  let i = Newspeak.Init str in
    glist := (name, t, i)::(!glist);
    let vid = incr glb_cnt in
      Hashtbl.add glb_tabl_vid name vid
   
(* There is no need to reset everything here because it only happens
   once *)
let get_glob_decls () =
  let rec aux accu l =
    match l with
      | [] -> accu
      | (n, t, i)::r -> aux ((n, t, replace_inits i)::accu) r
  in aux [] !glist






let final_specs = Hashtbl.create 100

let extract_typ (_, _, t) = t




let handle_funspec f_called name f =
  (* TODO: Should we have here the !remove_temp ? *)
  if (String_set.mem name f_called) (*|| not !remove_temp*) then begin
    let args = match f.pargs with
      | None -> error "Npkenv.handle_funspec" "unexpected error"
      | Some l -> List.map extract_typ l
    in
    let body =
      match f.pbody with
	| None -> None
	| Some b -> Some (replace_body b)
    in
      Hashtbl.add final_specs name ((args, f.prett), body)
  end  


let get_funspecs () = final_specs;


    



(* TODO: check that we still can accept extern *)
(*
      if glob.gv_defd || !accept_extern then begin
	glob.gv_used <- true;
	v.vtype <- glob.gv_ctyp
      end 
      else error ("Npkenv.glb_uses: global variable "
		  ^v.vname^" is used but is never defined")
*)
