
let display_help _ =
  prerr_endline ("Usage : "^Sys.argv.(0)^" file.npk")

let display_version _ =
  prerr_endline "V. pre-alpha"

let handle_file fname =
  let npk = Newspeak.read fname in
  prerr_endline (String.concat "," npk.Newspeak.fnames)

let main _ =
  let optlist = [ 'h', "help"    , Some display_help,    None
                ; 'v', "version" , Some display_version, None
                ] in
  Getopt.parse_cmdline optlist handle_file

let _ =
  main ()
