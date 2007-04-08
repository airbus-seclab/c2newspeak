open Cil
open Npkutils
open Npkcontext
open Npkil

let filenames = ref []

let glb_used = ref (String_set.empty)

let handle_file npko =
  (* TODO: Print debug... Handling file... *)
  filenames := npko.ifilename::(!filenames);
  glb_used := String_set.union !glb_used npko.iusedglbs;
  Npkenv.fun_called := String_set.union !Npkenv.fun_called npko.iusedfuns;
  Npkenv.glb_cstr := String_set.union !Npkenv.glb_cstr npko.iusedcstr;
  Hashtbl.iter Npkenv.update_glob_link npko.iglobs;
  Hashtbl.iter Npkenv.update_fun_link npko.ifuns

let generate_globals globs cstrs =
  String_set.iter Npkenv.handle_cstr cstrs;
(* TODO: translate_exp should not be exported ? *)
  Hashtbl.iter (Npkenv.handle_real_glob Npkcompile.translate_exp !glb_used) 
    globs;
  Npkenv.get_glob_decls ()

let generate_funspecs funs f_called =
  Hashtbl.iter (Npkenv.handle_funspec f_called) funs;
  Npkenv.get_funspecs ()

let link npkos =
  (* TODO: Think about it *)
  update_loc Cil.locUnknown;

  Npkenv.init_env ();

  print_debug "Linking files...";
  List.iter handle_file npkos;
  let decls = generate_globals Npkenv.glb_decls !Npkenv.glb_cstr in
  let funs = generate_funspecs Npkenv.fun_specs !Npkenv.fun_called in
  let kernel = ([], decls, funs) in
  print_debug "File linked.";

  if !verb_newspeak then begin
    print_endline "Newspeak output";
    print_endline "---------------";
    Newspeak.dump kernel;
    print_newline ()
  end;

  (!filenames, kernel)
