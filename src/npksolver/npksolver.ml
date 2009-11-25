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

let domain_str = function
  | "c" -> Domain.pack Const.dom
  | "r" -> Domain.pack Range.dom
  | "p" -> Domain.pack Parity.dom
  | "pr" -> Domain.pack (Pair.make Parity.dom Range.dom)
  | s    -> invalid_arg ("no such domain : "^s)

let       domain = ref (Domain.pack Range.dom)
let set_domain x = domain := domain_str x

let display_version _ =
  print_endline ("Version : " ^ Version.version ^ " -  rev " ^ Version.revision);
  print_endline "License : LGPLv2";
  print_endline "Author  : Etienne Millon";
  print_endline "Contact : etienne DOT millon AT eads DOT net"

let output_graphviz ?results cfg =
  if (Options.get Options.graphviz) then
  let file = open_out "cfg.dot" in
  output_string file (Mkcfg.dump_dot ?results cfg);
  close_out file

let (-->) (f:'a Domain.scope) (g:'a -> 'b) :'b Domain.scope =
  { Domain.bind =
      fun x ->
        g (f.Domain.bind x)
  }

let handle_file_npk fname =
  let npk = Newspeak.read fname in
  let (prg, vars) = Pcomp.compile npk in
  let (cfg, wpts) = Mkcfg.process prg vars in
  let graph_results results = output_graphviz ~results cfg in
    if (Options.get Options.cfg_only) then
      begin
      output_graphviz cfg;
      print_endline (Mkcfg.dump_yaml cfg);
      Domain.do_nothing
      end
    else
      Fixpoint.solve wpts cfg
      --> graph_results

type fname = C | Npk | Unknown

let fname_suffix str =
  let dot = String.rindex str '.' in
  let len = String.length str in
  match String.sub str (dot + 1) (len - dot - 1) with
  | "c"   -> C
  | "npk" -> Npk
  | _     -> Unknown

let c2newspeak fname =
  let tmpnam = "/tmp/solver_a.npk" in
  let ret = Sys.command ("c2newspeak -o " ^ tmpnam ^ " " ^ fname) in
  if ret <> 0 then failwith "c2newspeak error"
  else
    tmpnam

let handle_file fname =
  let handle fname = Domain.with_dom !domain (handle_file_npk fname) in
  match fname_suffix fname with
  | Npk     -> handle fname
  | C       -> handle (c2newspeak fname)
  | Unknown -> failwith "I don't know what to do with this file"

let run_selftests =
  function
  | "range" -> Test.range ()
  | "box"   -> Test.box   ()
  | "const" -> Test.const ()
  | _       -> invalid_arg "Bad test suite"

let main args =
  let ops =
  [ 'd' , "domain"
  , "select a domain ("
    ^   "c : constants"
    ^ ", r : ranges (default)"
    ^ ", p : parity"
    ^ ", rp : range + parity"
    ^ ")"
  , Options.Carg set_domain
  ; 'h', "help"
  , "this help message"
  , Options.Help
  ; 'g', "cfg"
  , "dump (YAML) control flow graph and exit"
  , Options.Set Options.cfg_only
  ; 'r', "graphviz"
  , "output in to cfg.dot (graphviz)"
  , Options.Set Options.graphviz
  ; 's', "solver"
  , "display solver output"
  , Options.Set Options.solver
  ; 't', "selftest"
  , "run unit tests (output TAP)"
  , Options.Carg run_selftests
  ; 'v', "verbose"
  , "output more information"
  , Options.Set Options.verbose
  ; 'V', "version"
  , "show version number"
  , Options.Call display_version
  ] in
  Options.parse_cmdline ops handle_file args

let _ =
  main Sys.argv
