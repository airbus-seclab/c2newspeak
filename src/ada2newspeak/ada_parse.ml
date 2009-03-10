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

  Jasmine Duchon
  email : jasmine . duchon AT free . fr

*)

let rec read_from_channel inchannel buffer =
  try
    begin
      Buffer.add_string buffer ((input_line inchannel)^"\n");
      read_from_channel inchannel buffer
    end
  with
    | End_of_file -> ()

let parse (fname:string) :Syntax_ada.compilation_unit =
  let cin = open_in fname in
  let buffer = Buffer.create 1000 in
  let lexbuf =
    read_from_channel cin buffer;
    Lexing.from_string (Buffer.contents buffer) in
    Lexer.init fname lexbuf;
    try
      let prog = Parser.s Lexer.token lexbuf
      in
        close_in cin;
        if Ada_utils.check_compil_unit_name prog fname
        then prog
        else Npkcontext.report_error "Ada_parse.parse"
          "file name does not match unit name"
    with Parsing.Parse_error ->
      let start_pos = Lexing.lexeme_start_p lexbuf
      and end_pos = Lexing.lexeme_end_p lexbuf
      in
      let line = string_of_int start_pos.Lexing.pos_lnum
      and lexeme = Lexing.lexeme lexbuf
      and start_col = start_pos.Lexing.pos_cnum
        - start_pos.Lexing.pos_bol
      and end_col = end_pos.Lexing.pos_cnum
        - end_pos.Lexing.pos_bol
      in
      let pos = "line "^line^", col "^(string_of_int start_col)
        ^(if start_col = end_col
          then ""
          else ("-"^(string_of_int end_col)))
      in
      let err_msg = "syntax error: "^pos^", unexpected token: "^lexeme in
        Npkcontext.report_error "Ada_parse.parse" err_msg
