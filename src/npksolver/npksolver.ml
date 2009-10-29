(*
  This file is part of npksolver, a solver for Newspeak,
  a minimal language framework well suited for static analysis.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *)

(** @author Etienne Millon <etienne.millon@eads.net> *)

let display_version _ =
  print_endline ("Version : " ^ Version.version ^ " -  rev " ^ Version.revision);
  print_endline "License : LGPLv2";
  print_endline "Author  : Etienne Millon";
  print_endline "Contact : etienne DOT millon AT eads DOT net"

let output_graphviz ?results cfg =
  let file = open_out "cfg.dot" in
  output_string file (Mkcfg.dump_dot ?results cfg);
  close_out file

let handle_file_npk dom fname =
  let npk = Newspeak.read fname in
  let (prg, vars) = Pcomp.compile npk in
  let (cfg, watchpoints) = Mkcfg.process prg vars dom in
  if (Options.get_cfg_only ()) then
    begin
    if (Options.get_graphviz ()) then
    begin
      output_graphviz cfg
    end;
    print_endline (Mkcfg.dump_yaml cfg)
    end
  else
    begin
      let solve_results = Fixpoint.solve dom cfg in
      if Options.get_solver () then begin
        print_endline "---";
        Array.iteri (fun i r ->
            print_endline ("  - {id: "^ string_of_int i ^
            ", "^ Box.yaml_dump dom r^"}")
        ) solve_results end;
      begin
      if (Options.get_graphviz ()) then
        let r = Array.map (Box.to_string dom) solve_results in
        output_graphviz ~results:r cfg
      end;
      Warnings.compute watchpoints dom solve_results
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
  let handle = 
    begin
      match Options.get_domain () with
      | Options.Const -> handle_file_npk Const.dom
      | Options.Range -> handle_file_npk Range.dom
    end
  in
  if (String.compare sfx "npk" = 0) then
    handle fname
  else if (String.compare sfx "c"   = 0) then
    let tmpname = c2newspeak fname in
    handle tmpname
  else
    failwith "I don't know what to do with this file"

let run_selftests =
  function
  | "range" -> Test.range ()
  | "box"   -> Test.box   ()
  | "const" -> Test.const ()
  | _       -> invalid_arg "Bad test suite"

let main _ =
  let ops =
  [ 'a', "algorithm", "select a fixpoint algorithm "
                     ^"(rr : roundrobin, wl : worklist (default))",
                     Options.Carg Options.set_fp_algo
  ; 'd', "domain", "select a domain "
                  ^ "(c : constants, r : ranges (default))",
                    Options.Carg Options.set_domain
  ; 'h', "help"    , "this help message",   Options.Help
  ; 'g', "cfg"     , "dump (YAML) control flow graph and exit"
                      , Options.Call Options.set_cfg_only
  ; 'r', "graphviz", "output in to cfg.dot (graphviz)"
                      , Options.Call Options.set_graphviz
  ; 's', "solver" , "display solver output", Options.Call Options.set_solver
  ; 't', "selftest", "run unit tests (output TAP)", Options.Carg run_selftests
  ; 'v', "verbose" , "output more information"
                      , Options.Call Options.set_verbose
  ; 'V', "version" , "show version number", Options.Call display_version
  ] in
  Options.parse_cmdline ops handle_file Sys.argv

let _ =
  main ()
