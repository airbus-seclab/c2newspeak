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

open Newspeak

let debug = ref false

let set_debug () = debug := true

let is_debug () = !debug

let call_stack = ref []

let print_current_call () =
  if !debug && !call_stack <> [] then begin
    let len = List.length !call_stack - 1 in
    let margin = String.make (2*len) ' ' in
    let f = List.hd !call_stack in
      print_endline (margin^"Analyzing "^f^"...")
  end

let execute_call_and_print f call =
  call_stack := f::!call_stack;
  print_current_call ();
  let result = call () in
    call_stack := List.tl !call_stack;
    print_current_call ();
    result
