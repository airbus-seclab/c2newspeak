
let display_version _ =
  print_endline "Version : pre-alpha";
  print_endline "License : LGPLv2";
  print_endline "Author  : Etienne Millon";
  print_endline "Contact : etienne DOT millon AT eads DOT net"

let handle_file_npk fname =
  let npk = Newspeak.read fname in
  let prg = Pcomp.compile npk in
  let cfg = Mkcfg.process prg in
  if (Options.get Options.Cfg_only) then
    print_endline (Mkcfg.dump cfg)
  else
    begin
      print_endline "---";
      Array.iteri (fun i r ->
          print_endline ("  - {id: "^ string_of_int i ^
          ", value: \"" ^ Range.to_string r^"\"}")
      ) (Fixpoint.solve cfg);
    end

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


  let ops = [ 'h', "help"    , "this help message",   None
            ; 'V', "version" , "show version number", Some display_version
            ; 'g', "cfg"     , "dump (YAML) control flow graph and exit"
                                     , Some (Options.set Options.Cfg_only)
            ; 'v', "verbose" , "output more information"
                                     , Some (Options.set Options.Verbose)
            ] in
  let display_help _ =
    print_endline ("Usage : " ^ Sys.argv.(0) ^ " file.npk");
    List.iter (fun (s, l, h, _) ->
      print_endline ("-"^(String.make 1 s)^" / --"^l^" : "^h)
    ) ops in
  let optlist = List.map (fun (s, l, _, fo) ->
      (s, l, (match fo with None -> Some display_help | Some f -> Some f), None)
  ) ops in
  if (Array.length Sys.argv = 1) then
    display_help ()
  else
    Getopt.parse_cmdline optlist handle_file

let _ =
  main ()
