
let display_help _ =
  print_endline ("Usage : "^Sys.argv.(0)^" file.npk")

let display_version _ =
  print_endline "Version : pre-alpha";
  print_endline "License : LGPLv2";
  print_endline "Author  : Etienne Millon";
  print_endline "Contact : etienne DOT millon AT eads DOT net"

let handle_file fname =
  let npk = Newspeak.read fname in
  print_endline (String.concat "," npk.Newspeak.fnames);
  let prg = Pcomp.compile npk in
  ignore prg

let main _ =
  let optlist = [ 'h', "help"    , Some display_help,    None
                ; 'v', "version" , Some display_version, None
                ] in
  if (Array.length Sys.argv = 1) then
    display_help ()
  else
    Getopt.parse_cmdline optlist handle_file

let _ =
  main ()
