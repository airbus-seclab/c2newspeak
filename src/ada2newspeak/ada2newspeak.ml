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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA

  Etienne Millon
  email : etienne . millon AT gmail . com

  Jasmine Duchon
  email : jasmine . duchon AT free . fr

  Charles Hymans
  email : charles . hymans AT eads . net
*)

(**
 * Parse a file, compile it and translate it into a Npkil representation.
 *)
let compile(fname:string):Npkil.t =
  if not (Filename.check_suffix fname Params.ada_suffix) then begin
    Npkcontext.report_error "Ada2newspeak.compile"
      (fname^" is not a .adb file")
  end;
  let base_name = Filename.basename fname in
  let dir_name = Filename.dirname fname in
  let current_dir = Sys.getcwd () in
    if dir_name <> "." then begin
      Npkcontext.print_debug ("Changing directory : "^dir_name);
      Sys.chdir dir_name
    end;
    Npkcontext.print_debug ("Parsing "^fname^"...");
    let t_0 = Unix.gettimeofday () in
    let (ast:Syntax_ada.compilation_unit) = File_parse.parse base_name in
      let t_1 = Unix.gettimeofday () in
      if (!Npkcontext.verb_ast) then begin
        print_endline "Abstract Syntax Tree";
        print_endline "----------------------";
        Print_syntax_ada.print_ast [ast];
        print_newline ();
      end;
      Npkcontext.forget_loc ();
      Npkcontext.print_debug "Parsing done.";
      Npkcontext.print_debug "Running first pass...";
      let prog = Firstpass.translate ast in
        let t_2 = Unix.gettimeofday () in
        Npkcontext.forget_loc ();
        Npkcontext.print_debug "First pass done.";
        Npkcontext.print_debug ("Translating "^fname^"...");
        let tr_prog = Cir2npkil.translate Newspeak.ADA prog [fname] in
          let t_3 = Unix.gettimeofday () in
          Npkcontext.print_debug
           ("   Time spent\n"
           ^"----------------\n"
           ^"Lexing/Parsing : "^string_of_float (1000.0 *. (t_1-.t_0))^"\n"
           ^"Translating    : "^string_of_float (1000.0 *. (t_2-.t_1))^"\n"
           ^"Post-1st pass  : "^string_of_float (1000.0 *. (t_3-.t_2))^"\n");
          Npkcontext.forget_loc ();
          if dir_name <> "." then begin
            Npkcontext.print_debug ("Changing directory : "^current_dir);
            Sys.chdir current_dir
          end;
          if (!Npkcontext.verb_npko) then begin
            print_endline "Newspeak Object output";
            print_endline "----------------------";
            Npkil.dump_npko tr_prog;
            print_newline ();
          end;
          tr_prog

let create_no name = (Filename.chop_extension name) ^ Params.npko_suffix

let _ =
  try
    Npkcontext.handle_cmdline_options
      Params.version_string Params.comment_string;
    let extract_no fname =
      if Filename.check_suffix fname Params.npko_suffix then fname
      else begin
        let no = create_no fname in
        let prog = compile fname in
          Npkil.write no prog;
          no
      end
    in

      (* TODO: this code should be factored with c2newspeak!!! *)
      match !Npkcontext.input_files with
          file::[]
            when !Npkcontext.compile_only && (!Npkcontext.output_file <> "") ->
              let prog = compile file in
                Npkil.write !Npkcontext.output_file prog

        | files ->
            let nos = List.map extract_no files in
              if not !Npkcontext.compile_only then Linker.link nos []

  with Invalid_argument msg -> Npkcontext.exit_on_error msg

