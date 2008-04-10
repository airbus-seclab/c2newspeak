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

open Newspeak

let input = ref []

let speclist = []

let anon_fun file = input := file::!input

let usage_msg = Sys.argv.(0)^" [options] [-help|--help] file.npk"


class scanner =
object (this)
  inherit Newspeak.visitor
    
  method process_exp e =
    let _ = 
      match e with
	  UnOp (Cast (Float _, Int _), Const _) -> 
	    this#print_warning 
	      "useless float constant: immediate cast to an int"
	| UnOp (Coerce b, Const CInt x) when not (Newspeak.belongs x b) ->
	    this#print_warning 
	      "invalid type for integer constant: integer overflow" 
	| _ -> ()
    in
      true

end

let scan f =
  let (_, prog, _) = Newspeak.read f in
  let scanner = (new scanner :> Newspeak.visitor) in
    Newspeak.visit scanner prog

let _ = 
  try
    Arg.parse speclist anon_fun usage_msg;
    List.iter scan !input
      
  with Invalid_argument s -> 
    print_endline ("Fatal error: "^s);
    exit 0
