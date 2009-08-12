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

let current_loc = ref (Newspeak.dummy_loc "initialization")

let errors = ref StrSet.empty

let verbose = ref false

let use_stubs = ref false

let warn_cnt = ref 0

(* TODO: factor set_verbose and set_use_stubs *)
let set_verbose () = verbose := true

let set_use_stubs () = use_stubs := true

let print_verbose msg = if !verbose then print_endline msg
  
(* TODO: factor print_err and report_stub_used *)
let print_err msg = 
  let msg = Newspeak.string_of_loc !current_loc^": "^msg in
    if not (StrSet.mem msg !errors) then begin
      errors := StrSet.add msg !errors;
      prerr_endline msg
    end
      
let report_stub_used msg = if not !use_stubs then print_err msg
  
let get_current_loc () = Newspeak.string_of_loc !current_loc

let set_current_loc loc = current_loc := loc
