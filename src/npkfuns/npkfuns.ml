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

let exec_name = "npkfuns"

let input = ref ""

let stats = ref false

let speclist = 
  [
    ("--stats", Arg.Set stats, "prints stats instead of function signatures")
  ]

let anon_fun file = 
  if !input <> "" then invalid_arg "you can only analyse one file at a time";
  input := file

let usage_msg = exec_name^" [options] [-help|--help] file.npk"

let process () =
    if !input = "" then StandardMain.report_missing_file ();

    let prog = Npk2lpk.translate (Newspeak.read !input) in
    let glbs = GlbCollect.process false prog in
      if !stats then GlbCollect.print_stats glbs
      else GlbCollect.print glbs

let _ =
  StandardMain.launch speclist anon_fun usage_msg process
