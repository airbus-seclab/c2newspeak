
let display_version _ =
  print_endline "Version : pre-alpha";
  print_endline "License : LGPLv2";
  print_endline "Author  : Etienne Millon";
  print_endline "Contact : etienne DOT millon AT eads DOT net"

let output_graphviz cfg =
  let file = open_out "cfg.dot" in
  output_string file (Mkcfg.dump_dot cfg);
  close_out file

let handle_file_npk fname =
  let npk = Newspeak.read fname in
  let prg = Pcomp.compile npk in
  let cfg = Mkcfg.process prg in
  if (Options.get_graphviz ()) then
    output_graphviz cfg;
  if (Options.get_cfg_only ()) then
    print_endline (Mkcfg.dump_yaml cfg)
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

let run_selftests =
  function
  | "range" -> Test.range ()
  | _       -> invalid_arg "Bad test suite"

let main _ =
  let ops =
  [ 'h', "help"    , "this help message",   Options.Help
  ; 'V', "version" , "show version number", Options.Call display_version
  ; 'g', "cfg"     , "dump (YAML) control flow graph and exit"
                      , Options.Call Options.set_cfg_only
  ; 'v', "verbose" , "output more information"
                      , Options.Call Options.set_verbose
  ; 'd', "dot", "output in to cfg.dot (graphviz)"
                      , Options.Call Options.set_graphviz
  ; 't', "selftest", "run unit tests (output TAP)", Options.Carg run_selftests
  ; 'a', "algorithm", "select a fixpoint algorithm "
                     ^"(rr : roundrobin, wl : worklist (default))",
                     Options.Carg Options.set_fp_algo
  ] in
  Options.parse_cmdline ops handle_file Sys.argv

let _ =
  main ()
