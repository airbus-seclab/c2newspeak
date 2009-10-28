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
 *)

(** @author Etienne Millon <etienne.millon@eads.net> *)
open Prog

let compute watchpoints results =
  begin
  if Options.get_verbose() then
    Printf.fprintf stderr "Watchpoint list : %s\n"
      (String.concat "," (List.map (fun (_,x,_) -> string_of_int x)
      watchpoints))
  end;
  List.iter (function
  | loc, l, AFalse ->
      if results.(l) <> Box.bottom then
        print_endline (Newspeak.string_of_loc loc ^ ": Assert false")
  | loc, l, ABound (v, inf, sup) ->
      let r = Box.get_var v results.(l) in
      if not (Range.dom.Domain.incl r (Range.from_bounds inf sup)) then
        print_endline (Newspeak.string_of_loc loc ^ ": Bound check")
  | loc, l, AEq (v, i) ->
      let r = Box.get_var v results.(l) in
      begin
      if Options.get_verbose() then
      Printf.fprintf stderr "eq check : r = %s, bound = {%d}\n" 
        (Range.dom.Domain.to_string r) i
      end;
      if not (Range.dom.Domain.incl r (Range.from_bounds i i)) then
        print_endline (Newspeak.string_of_loc loc ^ ": Value is different")
  ) watchpoints
