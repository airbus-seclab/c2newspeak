
let display_help _ =
  print_endline ("Usage : " ^ Sys.argv.(0) ^ " file.npk")

let display_version _ =
  print_endline "Version : pre-alpha";
  print_endline "License : LGPLv2";
  print_endline "Author  : Etienne Millon";
  print_endline "Contact : etienne DOT millon AT eads DOT net"

let handle_file_npk fname =
  let npk = Newspeak.read fname in
  let prg = Pcomp.compile npk in
  let cfg = Mkcfg.process prg in
  print_endline (Mkcfg.dump cfg)

let fname_suffix str =
  let dot = String.rindex str '.' in
  let len = String.length str in
  String.sub str (dot + 1) (len - dot - 1)

let c2newspeak fname =
  let tmpnam = "/tmp/solver_a.npk" in
  let ret = Sys.command ("c2newspeak -o " ^ tmpnam ^ " " ^ fname) in
  if ret <> 0 then failwith "c2newspeak error"
  else
    tmpnam

let handle_file fname =
  let sfx = fname_suffix fname in
  if (String.compare sfx "npk" = 0) then
    handle_file_npk fname
  else if (String.compare sfx "c"   = 0) then
    let tmpname = c2newspeak fname in
    handle_file_npk tmpname
  else
    failwith "I don't know what to do with this file"

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
