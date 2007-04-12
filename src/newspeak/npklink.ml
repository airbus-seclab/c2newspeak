open Cil
open Npkutils
open Npkcontext
open Npkil

let (size_of_scalar, size_of) = Newspeak.create_size_of Cilutils.pointer_size

let filenames = ref []

let glb_decls = Hashtbl.create 100

let glb_used = ref (String_set.empty)
let glb_cstr = ref (String_set.empty)

(*--------------*)
(* Linking time *)
(*--------------*)

(* Association table stdname -> Newspeak.typ *)
(* TODO: put these together with glb_decls *)
let glb_tabl_typ = Hashtbl.create 100
let glb_tabl_name = Hashtbl.create 100

let read_header fname = 
  let ch_in = open_in_bin fname in
    print_debug ("Importing "^fname^"...");
    let str = Marshal.from_channel ch_in in
      if str <> "NPKO" then begin 
	close_in ch_in;
	error "C2newspeak.extract_npko"
	  (fname^" is an invalid .npko file");
      end;
      let (res, _) = Marshal.from_channel ch_in in
	print_debug ("Importing done.");
	close_in ch_in;
	res



let read_fun fname = 
  let ch_in = open_in_bin fname in
    print_debug ("Importing funs from "^fname^"...");
    let _ = Marshal.from_channel ch_in in
    let (_, funs) = Marshal.from_channel ch_in in
      print_debug ("Funs import done.");
      close_in ch_in;
      funs

let get_glob_typ name =
  try
    Hashtbl.find glb_tabl_typ name
  with
      Not_found ->
	error "Npklink.get_glob_typ" ("type for global variable "^name^" not found")

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
      | Npkil.Label lbl -> Newspeak.Label lbl
  in 
    (new_sk, l)
       
and replace_chooseitem (exps, b) =
  (List.map replace_exp exps, List.map replace_stmt b)
    
and replace_lv lv =
  match lv with
    | Npkil.Global name -> Newspeak.Global (Hashtbl.find glb_tabl_name name)
    | Npkil.Deref (e, sz) -> Newspeak.Deref (replace_exp e, sz)
    | Npkil.Shift (lv', e) -> Newspeak.Shift (replace_lv lv', replace_exp e)
    | Npkil.Local v -> Newspeak.Local v
	
and replace_exp e =
  match e with
    | Npkil.Lval (lv, sca) -> Newspeak.Lval (replace_lv lv, sca)
    | Npkil.Const c -> Newspeak.Const c 
    | Npkil.AddrOfFun f -> Newspeak.AddrOfFun f
    | Npkil.AddrOf (lv, sz) -> 
	Newspeak.AddrOf (replace_lv lv, replace_tmp_int sz)
    | Npkil.UnOp (o, e) -> Newspeak.UnOp (replace_unop o, replace_exp e)
    | Npkil.BinOp (o, e1, e2) -> 
	Newspeak.BinOp (o, replace_exp e1, replace_exp e2)

and replace_unop o =
  match o with
      Npkil.Belongs_tmp (l, u) -> 
	let u = Int64.of_int (replace_tmp_int u - 1) in
	  Newspeak.Belongs (l, u)
    | Npkil.Coerce r -> Newspeak.Coerce r
    | Npkil.Not -> Newspeak.Not
    | Npkil.BNot r -> Newspeak.BNot r
    | Npkil.PtrToInt k -> Newspeak.PtrToInt k
    | Npkil.Cast (t1, t2) -> Newspeak.Cast (t1, t2)

and replace_tmp_int x =
  match x with
      Npkil.Known i -> i
    | Npkil.Length name -> begin
	match get_glob_typ name with
	    Newspeak.Array (_, len) -> len
	  | _ -> error "Npklink.replace_tmp_int" "array type expected"
      end
    | Npkil.SizeOf name -> size_of (get_glob_typ name)

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
    | None -> if !global_zero_init then Newspeak.Zero else Newspeak.Init []
    | Some l -> Newspeak.Init (List.map replace_init_field l)

and replace_init_field (sz, sca, e) = (sz, sca, replace_exp e)

and replace_typ t =
  match t with
      Npkil.Scalar x -> Newspeak.Scalar x
    | Npkil.Array (t, Some l) -> Newspeak.Array (replace_typ t, l)
    | Npkil.Array (_, None) -> raise LenOfArray
    | Npkil.Region (fields, sz) -> 
	Newspeak.Region (List.map replace_field fields, sz)

and replace_field (offs, t) = (offs, replace_typ t)


(*
  TODO: implement --accept-extern
*)

let init_of_string str = 
  let len = String.length str in
  let char_typ = Newspeak.Int (Newspeak.Signed, Cilutils.char_size) in
  let res = ref [(len, char_typ, Newspeak.exp_of_int 0)] in
    for i = len - 1 downto 0 do 
      let c = Char.code (String.get str i) in
	res := (i, char_typ, Newspeak.exp_of_int c)::!res
    done;
    (len + 1, !res)


let update_glob_link name g =
  try
    let x = Hashtbl.find glb_decls name in
      (* TODO: remove Npkil.compare_typs *)

      begin try
	if (Npkil.is_mp_typ g.gtype x.gtype) then 
	  x.gtype <- g.gtype;
      with Npkil.Uncomparable -> 
	(* TODO: add the respective locations *)
	error "Npklink.update_glob_link"
	  ("different types for "^name^": '"
	   ^(Npkil.string_of_typ x.gtype)^"' and '"
	   ^(Npkil.string_of_typ g.gtype)^"'")
      end;
      
      if g.gused then x.gused <- true;
      match g.ginit, x.ginit with
	  (None, Some _) -> ()
	| (Some _, None) -> x.ginit <- g.ginit
	| (None, None) -> ()
	| (Some (Some _), Some (Some _)) -> 
	    error "Npklink.update_glob_link" ("multiple declaration of "^name)
	| _ when !accept_mult_def -> 
	    print_warning "Npklink.update_glob_link" 
	      ("multiple definition of "^name)
	| _ -> 
	    error "Npklink.update_glob_link" ("multiple definition of "^name)
  with Not_found -> Hashtbl.add glb_decls name g

let merge_headers npko =
  let npko = read_header npko in

    filenames := npko.ifilename::(!filenames);
    glb_cstr := String_set.union !glb_cstr npko.iusedcstr;
    Hashtbl.iter update_glob_link npko.iglobs


(*
let update_fun_link fun_specs name f =
  try
    let x = Hashtbl.find fun_specs name in

      if (x.prett <> f.prett) then begin
	(* TODO: add the respective types and locations ? *)
	error "Npklink.update_fun_link"
	  ("different types for return type of prototype "^name)
      end;

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
	 
  with Not_found -> Hashtbl.add fun_specs name f
*)

let generate_globals globs =
  let glist = ref [] in


  let handle_cstr str =
    let name = ("!const_str_"^str) in 
    let (len, str) = init_of_string str in
    let char_sca = Newspeak.Int (Newspeak.Signed, Cilutils.char_size) in
    let t = Newspeak.Array (Newspeak.Scalar char_sca, len) in
    let i = Newspeak.Init str in
      glist := (name, t, i)::(!glist);
      Hashtbl.add glb_tabl_name name name
  in

  let handle_real_glob name g =
    let x = Hashtbl.find glb_decls name in
      if x.gused || (not !remove_temp) then begin
	let i =
	  match g.ginit with
	    | Some i -> i
	    | None when !accept_extern -> None
	    | None -> 
		invalid_arg "Npklink.handle_real_glob: extern not accepted"
	in
	  try
	    let t = replace_typ g.gtype in
	      glist := (name, t, replace_init i)::(!glist);
	      Hashtbl.add glb_tabl_typ name t;
	      Hashtbl.add glb_tabl_name name name
	  with LenOfArray -> 
	    error "Npklink.handle_real_glob" 
	      ("unspecified length for global array "^name)
      end
  in

    String_set.iter handle_cstr !glb_cstr;
    Hashtbl.iter handle_real_glob globs;
    !glist

let extract_typ (_, _, t) = t

let write_fun cout f spec =
  print_debug ("Writing function: "^f);
  Newspeak.write_fun cout f spec

let generate_funspecs cout npkos =
(* TODO: Use a String_set here, clean up sets *)
  let waiting = Hashtbl.create 100 in
  let encountered = Hashtbl.create 100 in  

  let handle_funspec name f =
    (* TODO: Should we have here the !remove_temp ? *)
    let args = 
      match f.pargs with
	| None -> error "Npklink.handle_funspec" "unexpected error"
	| Some l -> List.map extract_typ l
    in
    let body =
      match f.pbody with
	| None -> None
	| Some b -> Some (Newspeak.simplify (replace_body b))
    in
    let ftyp = replace_ftyp (args, f.prett) in
      
      try 
	let prev_ftyp = Hashtbl.find encountered name in
	  if (ftyp <> prev_ftyp) 
	  then error "Npklink.generate_funspecs" 
	    ("Function "^name^" type does not match");
	  match body with
	      None -> ()
	    | Some _ when Hashtbl.mem waiting name -> 
		Hashtbl.remove waiting name;
		write_fun cout name (ftyp, body)
	    | Some _ -> error "Npklink.generate_funspecs" 
		("Function "^name^" declared twice")
		  
      with Not_found -> 
	Hashtbl.add encountered name ftyp;
	match body with
	    None -> Hashtbl.add waiting name (ftyp, None)
	  | Some _ -> write_fun cout name (ftyp, body)
  in
    
  let read_all_funspec npko =
    let funs = read_fun npko in
      Hashtbl.iter handle_funspec funs
      (* TODO: Force garbage collection here *)
  in
    List.iter read_all_funspec npkos;
    Hashtbl.iter (Newspeak.write_fun cout) waiting
      

(* TODO: clean up *)
let link npkos output_file =
  let cout = open_out_bin output_file in
    (* TODO: Think about it *)
    update_loc Cil.locUnknown;
    
    print_debug "Linking files...";
    List.iter merge_headers npkos;
    print_debug "Globals...";
    let decls = generate_globals glb_decls in
      Newspeak.write_hdr cout (!filenames, decls, Cilutils.pointer_size);
      
      print_debug "Functions...";
      generate_funspecs cout npkos;

      close_out cout;

      print_debug "File linked.";
      
      if !verb_newspeak then begin
	print_endline "Newspeak output";
	print_endline "---------------";
	let (_, kernel, _) = Newspeak.read output_file in
	  Newspeak.dump kernel;
	  print_newline ()
      end
      


(*	Newspeak.write output_file (!filenames, kernel, Cilutils.pointer_size)*)
(*	(!filenames, kernel, Cilutils.pointer_size)*)

(*
  let prog = link npkos in
  print_debug ("Writing output to "^(!output_file)^"...");
  Newspeak.write !output_file prog;
  print_debug (!output_file^" written.")
*)
