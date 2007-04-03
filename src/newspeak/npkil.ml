open Cilutils
open Npkutils
open Npkenv

type intermediate = {
  ifilename : string;
  iglobs : (string, glb_type) Hashtbl.t;
  ifuns  : (Newspeak.fid, fspec_type) Hashtbl.t;
  iusedglbs : Npkutils.String_set.t;
  iusedcstr : Npkutils.String_set.t;
  iusedfuns : Npkutils.String_set.t;
}


let dump_npko inter = 

  let print_list title list =
    print_endline title;
    String_set.iter print_endline list;
    print_newline ()
  in

  let print_glob n g =
    if not g.gdefd then print_string "extern ";
    print_string ((string_of_type g.gtype)^" "^n);
    match g.ginit with
	None -> print_endline ";"
      | Some i -> print_endline (" = "^(string_of_init i)^";")
  in

  let print_fundef n f =
    Newspeak.dump_fundec n (([], None), f.pbody);
    if f.pbody <> None then print_newline ()
  in
    print_endline inter.ifilename;

    print_list "Global used" inter.iusedglbs;
    print_list "Functions called" inter.iusedfuns;
    print_list "Constant Strings" inter.iusedcstr;

    print_endline "Global variables";
    Hashtbl.iter print_glob inter.iglobs;
    print_newline ();

    print_endline "Function definitions";
    Hashtbl.iter print_fundef inter.ifuns;

