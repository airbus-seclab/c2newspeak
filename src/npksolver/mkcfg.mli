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

(**
 * Build a control-flow graph from a program.
 * The second argument is a list of global variable and their sizes.
 *)
val process : Prog.t -> Cfg.t

(**
 * Pretty-print a CFG.
 *)
val to_string : Cfg.t -> string

val print_stmt : Cfg.stmt -> string

(**
 * Dump a CFG as a YAML string.
 *)
val dump_yaml : Cfg.t -> Yaml.t

(**
 * Dump a CFG as a DOT (graphviz) string.
 * If 'results' is passed, it will be used to decorate nodes.
 * For example, if the result of Fixpoint.solve is passed,
 * The generated DOT string will represent the abstract values at
 * every node.
 *)
val dump_dot : ?results:(string Resultmap.t) -> Cfg.t -> string
