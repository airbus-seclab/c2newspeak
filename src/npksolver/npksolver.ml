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

  Copyright 2009, 2010 Etienne Millon <etienne.millon@eads.net>

 *)

(** @author Etienne Millon <etienne.millon@eads.net> *)

let domains = Hashtbl.create 0

let domain_str s =
  try
    fst (Hashtbl.find domains s)
  with Not_found -> invalid_arg ("no such domain : "^s)

let register_domains =
  let register_domain (k, desc, dom) =
    Hashtbl.add domains k (dom, desc)
  in
  List.iter register_domain

let       domain = ref (Domain.pack Ptr_plus_range.dom)
let set_domain x = domain := domain_str x

let display_version _ =
  print_endline ("Version : " ^ Version.version ^ " - rev " ^ Version.revision);
  print_endline "License : LGPLv2";
  print_endline "Author  : Etienne Millon";
  print_endline "Contact : etienne DOT millon AT eads DOT net"

let output_graphviz ?results cfg =
  if (Options.get Options.graphviz) then
  let file = open_out "cfg.dot" in
  output_string file (Mkcfg.dump_dot ?results cfg);
  close_out file

let handle_file_npk fname =
  let npk = Npk2lpk.translate (Newspeak.read fname) in
  let prg = Pcomp.compile npk in
  List.iter (function
    | Prog.Widening -> Options.set Options.widening true
    | Prog.Domain d -> domain := domain_str d
  ) prg.Prog.anns;
  let cfg = Mkcfg.process prg in
  let graph_results results = output_graphviz ~results cfg in
    if (Options.get Options.cfg_only) then
      begin
      output_graphviz cfg;
      print_endline (Yaml.render (Mkcfg.dump_yaml cfg))
      end
    else
      graph_results (Fixpoint.solve !domain cfg)

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
  match fname_suffix fname with
  | Npk     -> handle_file_npk fname
  | C       -> handle_file_npk (c2newspeak fname)
  | Unknown -> failwith "I don't know what to do with this file"

let run_selftests =
  function
  | "range" -> Test.range ()
  | "box"   -> Test.box   ()
  | "const" -> Test.const ()
  | _       -> invalid_arg "Bad test suite"

let main args =
  register_domains
    [ ( "const"
      , "constants"
      , Domain.pack Const.dom
      )
    ; ("range"
      , "ranges (default)"
      , Domain.pack Range.dom
      )
    ; ( "ptr_plus_range"
      , "ptr_offset and range (improved)"
      , Domain.pack Ptr_plus_range.dom
      )
    ];
  let domain_descrs = Hashtbl.fold (fun k (_, descr) s ->
    k^" : "^descr^" "^ s
  ) domains "" in
  let ops =
  [ 'd', "domain"   , "select a domain ("^domain_descrs^")" , Options.Carg set_domain
  ; 'h', "help"     , "this help message"                   , Options.Help
  ; 'g', "cfg"      , "dump (YAML) control flow graph "     , Options.Set Options.cfg_only
  ; 'r', "graphviz" , "output in to cfg.dot (graphviz)"     , Options.Set Options.graphviz
  ; 's', "solver"   , "display solver output"               , Options.Set Options.solver
  ; 't', "selftest" , "run unit tests (output TAP)"         , Options.Carg run_selftests
  ; 'v', "verbose"  , "output more information"             , Options.Set Options.verbose
  ; 'V', "version"  , "show version number"                 , Options.Call display_version
  ; 'w', "widening" , "use widening"                        , Options.Set Options.widening
  ] in
  Options.parse_cmdline ops handle_file args

let _ =
(* TODO: try to use StandardMain instead of adhoc argument treatment *)
  main Sys.argv
