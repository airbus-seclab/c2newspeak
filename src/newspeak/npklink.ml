open Cil
open Npkutils
open Npkcontext
open Npkil

let filenames = ref []

let fun_specs = Hashtbl.create 100

let glb_used = ref (String_set.empty)
let fun_called = ref (String_set.empty)
let glb_cstr = ref (String_set.empty)

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
	    Npkenv.compare_formals name l1 l2
	| Some l1, Some l2, None, _, _, _, None, _ ->
	    Npkenv.compare_formals name l1 l2;
	    x.pargs <- f.pargs;
	    x.plocs <- f.plocs;
	    x.ploc <- f.ploc;
	    x.pbody <- f.pbody
	      (* TODO: Produce more precise errors *)
	| _ -> error "Npkenv.update_fun_link" ("unexpected error for "^name)
    in ()
	 
  with Not_found ->
    Hashtbl.add fun_specs name f


let handle_file npko =
  (* TODO: Print debug... Handling file... *)
  filenames := npko.ifilename::(!filenames);
  glb_used := String_set.union !glb_used npko.iusedglbs;
  fun_called := String_set.union !fun_called npko.iusedfuns;
  glb_cstr := String_set.union !glb_cstr npko.iusedcstr;
  Hashtbl.iter Npkenv.update_glob_link npko.iglobs;
  Hashtbl.iter update_fun_link npko.ifuns

let generate_globals globs =
  String_set.iter Npkenv.handle_cstr !glb_cstr;
(* TODO: translate_exp should not be exported ? *)
  Hashtbl.iter (Npkenv.handle_real_glob Npkcompile.translate_exp !glb_used) 
    globs;
  Npkenv.get_glob_decls ()

let extract_typ (_, _, t) = t

let generate_funspecs funs =
  let final_specs = Hashtbl.create 100 in
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
	  | Some b -> Some (Npkenv.replace_body b)
      in
	Hashtbl.add final_specs name ((args, f.prett), body)
    end
  in
    Hashtbl.iter (handle_funspec !fun_called) funs;
    final_specs

let link npkos =
  (* TODO: Think about it *)
  update_loc Cil.locUnknown;

  Npkenv.init_env ();

  print_debug "Linking files...";
  List.iter handle_file npkos;
  let decls = generate_globals Npkenv.glb_decls in
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
