open Cil
open Npkutils
open Npkcontext
open Npkenv
open Npkil


let filenames = ref []

let handle_file npko =
  (* TODO: Print debug... Handling file... *)
  filenames := npko.ifilename::(!filenames);
  glb_used := String_set.union !glb_used npko.iusedglbs;
  fun_called := String_set.union !fun_called npko.iusedfuns;
  glb_cstr := String_set.union !glb_cstr npko.iusedcstr;
  Hashtbl.iter update_glob_link npko.iglobs;
  Hashtbl.iter update_fun_link npko.ifuns






let generate_globals globs g_used cstrs =
  String_set.iter handle_cstr cstrs;
(* TODO: translate_exp should not be exported ? *)
  Hashtbl.iter (handle_real_glob Npkcompile.translate_exp g_used) globs;
  get_glob_decls ()  



	
let generate_funspecs funs f_called =
  Hashtbl.iter (handle_funspec f_called) funs;
  get_funspecs ()



let link npkos =
  (* TODO: Think about it *)
  update_loc Cil.locUnknown;

  init_env ();

  print_debug "Linking files...";
  List.iter handle_file npkos;
  let decls = generate_globals glb_decls !glb_used !glb_cstr in
  let funs = generate_funspecs fun_specs !fun_called in
  let kernel = ([], decls, funs) in
  print_debug "File linked.";

  if !verb_newspeak then begin
    print_endline "Newspeak output";
    print_endline "---------------";
    Newspeak.dump kernel;
    print_newline ()
  end;

  (!filenames, kernel)
