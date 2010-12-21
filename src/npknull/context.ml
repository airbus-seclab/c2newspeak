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

type option =
    Verbose
  | UseStubs
  | PrintGraph

module OptionSet = Set.Make(struct type t = option let compare = compare end)

let options = ref OptionSet.empty

let current_loc = ref (Newspeak.dummy_loc "initialization")

let errors = ref StrSet.empty

let string_of_option option =
  match option with
      Verbose -> "--verbose"
    | UseStubs -> "--use-stubs"
    | PrintGraph -> "--graph"

let set_current_loc loc = current_loc := loc

let get_current_loc () = Newspeak.string_of_loc !current_loc

let set_option option () = options := OptionSet.add option !options

let option_is_set option = OptionSet.mem option !options

let print_verbose msg = if option_is_set Verbose then print_endline msg

let print_err msg = 
  let msg = Newspeak.string_of_loc !current_loc^": "^msg in
    if not (StrSet.mem msg !errors) then begin
      errors := StrSet.add msg !errors;
      prerr_endline msg
    end
      
let print_err_with_advice missing_option message = 
  if not (option_is_set missing_option) 
  then begin
    let message = 
      message^ ", use option "^string_of_option missing_option
      ^" to skip this message"
    in
      print_err message
 end
  
let print_graph str = 
  if option_is_set PrintGraph then print_endline ("[G] "^str)
