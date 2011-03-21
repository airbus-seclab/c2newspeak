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

let experimental = ref false

let precision_level = ref 2

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
     "prints more details");
    ("--experimental", Arg.Set experimental, "experimental version");
    ("--precision", Arg.Int (fun x -> precision_level := x), "precision level")
  ]

module TopValue(Subst: Transport.T) =
struct
  include TopValue
end

let run0 = 
  let module State = State2Bottom.Make(State2.Make(TopValue)) in
  let module Analysis = Modular.Make(Subst2)(State) in
    Analysis.process 

let run1 = 
  let module State = State2Bottom.Make(State2.Make(NotZeroValue.Make)) in
  let module Analysis = Modular.Make(Subst2)(State) in
    Analysis.process 

let run2 = 
  let module State = State2Bottom.Make(State2.Make(NotZeroValue.Make)) in
  let module Analysis = Modular.Make(Subst3)(State) in
    Analysis.process 

let process input = 
  let prog = Newspeak.read input in
    if !experimental then begin
      let prog = Npk2lpk.translate prog in
      let glb_tbl = GlbCollect.process true prog in
      let results = Solver.process glb_tbl prog in
	if !stats then Stats.print prog results
    end else begin
      let entry_point = "main" in
      let global_tbl = UsedGlobals.compute [entry_point] prog in
      let prog = Preprocessor.prepare prog in
      let analysis = 
	if !precision_level = 0 then run0 
	else if !precision_level = 1 then run1
	else run2
      in
	analysis (global_tbl, prog) entry_point
    end

let _ =
  StandardApplication.launch_process_with_npk_argument "npknull" speclist 
    process
