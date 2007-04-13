(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans, Olivier Levillain
  
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

  Olivier Levillain
  email: olivier.levillain@penjili.org
*)


let fname = ref ""

let anon_fun file =
  if !fname = ""
  then fname := file
  else invalid_arg "You can only get statistics on one file at a time."

let usage_msg = Sys.argv.(0)^" [options] [-help|--help] file.npk"

let speclist = 
  Npkstats.args
(*  @[("-v", Arg.Unit (Npkcontext.verbose true),
     "verbose mode: turn all verbose options on");
    
    ("-q", Arg.Unit (Npkcontext.verbose false), 
     "quiet mode: turn display off");
    
    ("--exit-code", Arg.Set Npkcontext.exit_code, "");		  
    ("-c", Arg.Set Npkcontext.exit_code, "returns 1 if an error occured")] *)
    
let _ = 
  Arg.parse speclist anon_fun usage_msg;

  if !fname = "" then begin
    print_endline ("Fatal error: no file specified. Try "
		   ^Sys.argv.(0)^" --help");
    exit 0;
  end;

  try
    let (_, npk, _) = Newspeak.read !fname in
      Collector.count npk;
      print_endline (Npkstats.to_string ())
  with Invalid_argument s -> 
    print_endline ("Fatal error: "^s);
    exit 0
      
