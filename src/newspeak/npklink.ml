open Cil
open Npkutils
open Npkcontext
open Npkil

let filenames = ref []


let glb_decls = Hashtbl.create 100
let fun_specs = Hashtbl.create 100

let glb_used = ref (String_set.empty)
let fun_called = ref (String_set.empty)
let glb_cstr = ref (String_set.empty)

(*--------------*)
(* Linking time *)
(*--------------*)
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
	error "Npklink.get_glob_vid" ("global variable "^name^" not found")

let get_glob_typ name =
  try
    Hashtbl.find glb_tabl_typ name
  with
      Not_found ->
	error "Npklink.get_glob_vid" ("type for global variable "^name^" not found")

let rec replace_stmt (sk, l) =
  let new_sk = 
    match sk with
      | Npkil.Set (lv, e, sca) -> Newspeak.Set (replace_lv lv, replace_exp e, sca)
      | Npkil.Copy (lv1, lv2, sz) -> Newspeak.Copy (replace_lv lv1, replace_lv lv2, sz)
      | Npkil.Decl (name, t, b) -> 
	  Newspeak.Decl (name, t, List.map replace_stmt b)
      | Npkil.ChooseAssert l -> Newspeak.ChooseAssert (List.map replace_chooseitem l)
      | Npkil.InfLoop b -> Newspeak.InfLoop (List.map replace_stmt b)
      | Npkil.Call fn -> Newspeak.Call (replace_fn fn)
      | Npkil.Goto lbl -> Newspeak.Goto lbl 
      | Npkil.Label lbl -> Newspeak.Label lbl
  in 
    (new_sk, l)
       
and replace_chooseitem (exps, b) =
  (List.map replace_exp exps, List.map replace_stmt b)
    
and replace_lv lv =
  match lv with
    | Npkil.Global_tmp name -> Newspeak.Global (get_glob_vid name)
    | Npkil.Deref (e, sz) -> Newspeak.Deref (replace_exp e, sz)
    | Npkil.Shift (lv', e) -> Newspeak.Shift (replace_lv lv', replace_exp e)
    | Npkil.Shift_tmp (name, e) -> begin
	match get_glob_typ name with
	    Newspeak.Array (t, len) ->
	      let sz = Newspeak.size_of t in
	      let index_exp =
		Npkil.BinOp (Newspeak.MultI,
			    Npkil.make_belongs len e,
			    Npkil.exp_of_int sz)
	      in
	      let v = Npkil.Global_tmp name in
	      let lv = Npkil.Shift (v, index_exp) in
		replace_lv lv
	  | _ -> error "Npklink.replace_lval"
	      ("type of lval "(*^(Npkil.string_of_lval lv)*)
		^" is not defined enough")
      end
    | Npkil.Local v -> Newspeak.Local v
	
and replace_exp e =
  match e with
    | Npkil.Lval (lv, sca) -> Newspeak.Lval (replace_lv lv, sca)
    | Npkil.Const c -> Newspeak.Const c 
    | Npkil.AddrOfFun f -> Newspeak.AddrOfFun f
    | Npkil.AddrOf (lv, sz) -> Newspeak.AddrOf (replace_lv lv, sz)
    | Npkil.UnOp (o, e) -> Newspeak.UnOp (o, replace_exp e)
    | Npkil.BinOp (o, e1, e2) -> Newspeak.BinOp (o, replace_exp e1, replace_exp e2)
	
and replace_fn fn =
  match fn with
    | Npkil.FunId f -> Newspeak.FunId f
    | Npkil.FunDeref (e, t) -> Newspeak.FunDeref (replace_exp e, t)

and replace_body body = List.map replace_stmt body

and replace_inits init =
  match init with
    | Npkil.Zero -> Newspeak.Zero
    | Npkil.Init l -> Newspeak.Init (List.map replace_init l)

and replace_init (sz, sca, e) = (sz, sca, replace_exp e)

(* TODO: should never use Npkcompile here, how can I simplify this 
   to remove it ? *)
let handle_real_glob g_used name g =

  let translate_init loc t init =
    let glb_inits = ref [] in
      
    let rec expand off i =
      match i with
	  SingleInit e ->
	    let o = Cilutils.offset_of t off in begin
	      try
		match translate_typ (typeOf e) with
		    Newspeak.Scalar s -> 
		      glb_inits := (o, s, replace_exp (Npkcompile.translate_exp e))::(!glb_inits)
		  | _ -> error "Npklink.translate_init" "unexpected type of SingleInit"
	      with LenOfArray ->
		error "Npklink.translate_init"
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
      error "Npklink.handle_real_glob"
	("unspecified length for global array "^name)
  end
    
let handle_cstr str =
  let name = ("!const_str_"^str) in 
  let (len, str) = Newspeak.init_of_string str in
  let char_sca = Newspeak.Int (Newspeak.Signed, Cilutils.char_size) in
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
      | (n, t, i)::r -> aux ((n, t, (*TODO:replace_inits*) i)::accu) r
  in 
    aux [] !glist




let update_glob_link name g =
  try
    let x = Hashtbl.find glb_decls name in
      if not (compare_typs x.gtype g.gtype)
	(* TODO: add the respective locations *)
      then error "Npklink.update_glob_link"
	("different types for "^name^": '"
	 ^(Cilutils.string_of_type x.gtype)^"' and '"
	 ^(Cilutils.string_of_type g.gtype)^"'");
      match g, x with
	  {ginit = None}, _ when not g.gdefd && x.gdefd -> ()
	  | _, {ginit = None} when g.gdefd && not x.gdefd ->
	      Hashtbl.replace glb_decls name g
	  | _ when not x.gdefd && not g.gdefd -> ()
	  | _ -> error "Npklink.update_glob_link"
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
	    error "Npklink.update_fun_link"
	      ("different types for return type of prototype "^name)
    in
      
    let _ =
      match x.pcil_body, f.pcil_body with
	| None, None -> ()
	| _ -> error "Npklink.update_fun_link" ("unexpected error for "^name)
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
	    Npkenv.compare_formals name l1 l2
	| Some l1, Some l2, None, _, _, _, None, _ ->
	    Npkenv.compare_formals name l1 l2;
	    x.pargs <- f.pargs;
	    x.plocs <- f.plocs;
	    x.ploc <- f.ploc;
	    x.pbody <- f.pbody
	      (* TODO: Produce more precise errors *)
	| _ -> error "Npklink.update_fun_link" ("unexpected error for "^name)
    in ()
	 
  with Not_found ->
    Hashtbl.add fun_specs name f


let handle_file npko =
  (* TODO: Print debug... Handling file... *)
  filenames := npko.ifilename::(!filenames);
  glb_used := String_set.union !glb_used npko.iusedglbs;
  fun_called := String_set.union !fun_called npko.iusedfuns;
  glb_cstr := String_set.union !glb_cstr npko.iusedcstr;
  Hashtbl.iter update_glob_link npko.iglobs;
  Hashtbl.iter update_fun_link npko.ifuns

let generate_globals globs =
  String_set.iter handle_cstr !glb_cstr;
(* TODO: translate_exp should not be exported ? *)
  Hashtbl.iter (handle_real_glob !glb_used) 
    globs;
  get_glob_decls ()

let extract_typ (_, _, t) = t

let generate_funspecs funs =
  let final_specs = Hashtbl.create 100 in
  let handle_funspec f_called name f =
    (* TODO: Should we have here the !remove_temp ? *)
    if (String_set.mem name f_called) (*|| not !remove_temp*) then begin
      let args = match f.pargs with
	| None -> error "Npklink.handle_funspec" "unexpected error"
	| Some l -> List.map extract_typ l
      in
      let body =
	match f.pbody with
	  | None -> None
	  | Some b -> Some (Newspeak.simplify (replace_body b))
      in
	Hashtbl.add final_specs name ((args, f.prett), body)
    end
  in
    Hashtbl.iter (handle_funspec !fun_called) funs;
    final_specs

let link npkos =
  (* TODO: Think about it *)
  update_loc Cil.locUnknown;

  print_debug "Linking files...";
  List.iter handle_file npkos;
  let decls = generate_globals glb_decls in
  let funs = generate_funspecs fun_specs in
  let kernel = ([], decls, funs) in
  print_debug "File linked.";

  if !verb_newspeak then begin
    print_endline "Newspeak output";
    print_endline "---------------";
    Newspeak.dump kernel;
    print_newline ()
  end;

  (!filenames, kernel)
