open Npkutils
open Npkcontext
open Env
open Npkil


let filenames = ref []

(* table gérant les correspondances nomGlobTemp -> vid *)
let glob_corresp = Hashtbl.create 100

let handle_file npko =
  (* TODO: Print debug... Handling file... *)
  filenames := npko.ifilename::(!filenames);
  glb_used := String_set.union !glb_used npko.iusedglbs;
  fun_called := String_set.union !fun_called npko.iusedfuns;
  glb_cstr := String_set.union !glb_cstr npko.iusedcstr;
  Hashtbl.iter update_glob_link npko.iglobs;
  Hashtbl.iter update_fun_link npko.ifuns


let link npkos out_name =
  (* TODO: Think about it *)
  update_loc Cil.locUnknown;
  init_env ();

  print_debug "Linking files...";

  (* Créer les globales *)
  (* Parcourir les specs.body *)

  print_debug "File linked.";

  if !verb_newspeak then begin
    print_endline "Newspeak output";
    print_endline "---------------";

    dump_npko {ifilename = "";
	       iglobs = glb_decls; ifuns = fun_specs;
	       iusedglbs = !glb_used; iusedcstr = !glb_cstr;
	       iusedfuns = !fun_called;};

    (* TODO: Dump something correct *)
    (* K.dump kernel; *)


    print_newline ()
  end;

  print_debug ("Writing output to "^out_name^"...");
  let ch_out = open_out_bin out_name in
    Marshal.to_channel ch_out "NPK!" [];
    (* TODO: Write something ;-) *)
    (* Marshal.to_channel ch_out (fnames, kernel) []; *)
    close_out ch_out;
    print_debug (out_name^" written.")
