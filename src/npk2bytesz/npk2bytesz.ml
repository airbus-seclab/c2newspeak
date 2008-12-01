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
*)
open Newspeak

let input = ref ""
let output = ref "a.npk"
let print = ref false

let speclist = 
  [("--newspeak", Arg.Set print, "Print output");
   ("-o", Arg.Set_string output, "changes name of output, default is 'a.npk'")
  ]

let anon_fun file =
  if !input = ""
  then input := file
  else invalid_arg "You can only get statistics on one file at a time."

let usage_msg = Sys.argv.(0)^" [options] [-help|--help] file.npk"
  
let eight = Nat.of_int 8
  
let rec div e =
  match e with
      Const CInt i -> Const (CInt (Nat.div i eight))
    | BinOp (MultI, e, Const CInt i) when Nat.compare i eight = 0 -> e
    | BinOp (MultI, e, (Const CInt _ as i)) -> BinOp (MultI, e, div i)
    | _ -> BinOp (DivI, e, Newspeak.exp_of_int 8)
	
class to_byte_sz =
object (self)
  inherit Newspeak.builder
    
  method process_size_t sz =
    if (sz mod 8) <> 0 
    then begin
      let msg = 
	if self#curloc = Newspeak.unknown_loc then ""
	else (Newspeak.string_of_loc self#curloc)^": "
      in
      let msg = msg^"size not multiple of 8 bits" in
	invalid_arg msg
    end;
    sz / 8

  method process_offset = self#process_size_t

  method process_lval lv =
    match lv with
	Shift (lv, e) -> Shift (lv, div e)
      | _ -> lv

  method process_exp e =
    match e with
	BinOp (PlusPI, p, i) -> BinOp (PlusPI, p, div i)
      | _ -> e
end

let _ = 
  try
    Arg.parse speclist anon_fun usage_msg;

    if !input = "" 
    then invalid_arg ("no file specified. Try "^Sys.argv.(0)^" --help");

    let prog = Newspeak.read !input in
    let builder = new to_byte_sz in
    let prog = Newspeak.build builder prog in
      if !print then Newspeak.dump prog;
      Newspeak.write !output prog

  with Invalid_argument s -> 
    print_endline ("Fatal error: "^s);
    exit 0
      
