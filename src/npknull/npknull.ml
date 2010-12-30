(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans
  
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

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org
*)
(* TODO: factor launcher and error treatment for the several newspeak 
   utilities *)

let stats = ref false

let speclist = 
  [
    ("--stats", Arg.Set stats, "prints analysis stats");
    (Context.string_of_option Context.UseStubs, 
     Arg.Unit (Context.set_option Context.UseStubs), 
     "skips warning messages when using stubs");
    (Context.string_of_option Context.PrintGraph, 
     Arg.Unit (Context.set_option Context.PrintGraph), 
     "prints infos to display call graph during analysis");
    (Context.string_of_option Context.Verbose, 
     Arg.Unit (Context.set_option Context.Verbose), 
     "prints more details")
  ]

let process input = 
  let prog = Npk2lpk.translate (Newspeak.read input) in
  let glb_tbl = GlbCollect.process true prog in
  let results = Solver.process glb_tbl prog in
    if !stats then Stats.print prog results

let _ =
  StandardApplication.launch_process_with_npk_argument "npknull" speclist 
    process
