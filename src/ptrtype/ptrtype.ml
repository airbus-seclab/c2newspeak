(*
 * ptrtype: do finer typechecks on C pointers
 * Copyright (C) 2011-2012 Etienne Millon
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * Etienne Millon <etienne.millon@eads.net>
 * EADS Innovation Works - SE/IT
 * 12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
 *)

let do_checks = ref true
let show_steps = ref false

let process_npk npk =
  let tpk = Npk2tpk.convert_unit npk in
  let s = Simple.infer (!show_steps) tpk in
  begin
    if !do_checks then
      Simple_check.check s
  end;
  Tyspeak.dump Simple_types.string_of_simple s

let exit_error msg =
  print_endline msg;
  print_endline "Exiting.";
  exit 1

let with_npk f fname =
  try
    f (Newspeak.read fname)
  with Sys_error e -> exit_error e

let process_file =
  with_npk process_npk

let main () =
  let speclist = [ ( "--no-check"
                   , Arg.Unit (fun () -> do_checks := false)
                   , "Don't run the typechecking phase (only inference)"
                   )
                 ; ( "--show-steps"
                   , Arg.Unit (fun () -> show_steps := true)
                   , "Show the type inference phase in slow motion"
                   )
                 ] in
  let inputs = ref [] in
  let add_file file =
    inputs := file::!inputs
  in
  let usage_msg = "ptrtype file.npk" in
  let process () =
    let files = List.rev (!inputs) in
    List.iter process_file files
  in
  Printexc.record_backtrace true;
  StandardApplication.launch speclist add_file usage_msg process

let _ = main ()
