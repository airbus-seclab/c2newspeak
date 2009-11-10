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

let output_graphviz dom ?results cfg =
  if (Options.get_graphviz ()) then
  let file = open_out "cfg.dot" in
  let results = match results with
  | None   -> None
  | Some r -> Some (Array.map (Box.to_string dom) r) in
  output_string file (Mkcfg.dump_dot ?results cfg);
  close_out file

let handle_file_npk fname =
  let npk = Newspeak.read fname in
  let (prg, vars) = Pcomp.compile npk in
  let (cfg, wpts) = Mkcfg.process prg vars in
  { Domain.bind = fun dom ->
    if (Options.get_cfg_only ()) then
      begin
      output_graphviz dom cfg;
      print_endline (Mkcfg.dump_yaml cfg)
      end
    else
      begin
      let results = Fixpoint.solve wpts dom cfg in
      output_graphviz dom ~results cfg;
      end
  }

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
  let dom = 
    match Options.get_domain () with
    | Options.Const -> Domain.pack Const.dom
    | Options.Range -> Domain.pack Range.dom
  in
  let handle fname =
    Domain.with_dom dom (handle_file_npk fname)
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
                  ^ "(c : constants, r : ranges, at : array_top (default))",
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
