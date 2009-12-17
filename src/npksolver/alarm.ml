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

type t =
  | Array_OOB                   (** Array index out of bounds    *)
  | Ptr_OOB                     (** Pointer offset out of bounds *)
  | Assertion_failed of string  (** Null pointer dereference     *)

let string_of_alarm = function
  | Array_OOB          -> "Array index out of bounds"
  | Ptr_OOB            -> "Pointer offset out of bounds"
  | Assertion_failed s -> "Assertion failed : " ^ s

module Alarm_set = Set.Make(String)

let alarms = ref (Alarm_set.empty)

let emit ?reason loc s =
  let str_loc =
    if loc = Newspeak.unknown_loc then
      "(no loc)"
    else Newspeak.string_of_loc loc
  in
  let alarm_text = str_loc ^ ": " ^ string_of_alarm s in
  if (not (Alarm_set.mem alarm_text !alarms)) then
    begin
      alarms := Alarm_set.add alarm_text !alarms;
      print_endline alarm_text;
      match reason with
      | None -> ()
      | Some r ->
        if (Options.get Options.verbose) then
          print_endline ("Reason : " ^ r)
    end
