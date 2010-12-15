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
open Csyntax

let parse fname =
  let cin = open_in fname in
  let lexbuf = Lexing.from_channel cin in
    Lexer.init fname lexbuf;
    Synthack.init_tbls ();
    try
      let (fnames, globals) = Parser.parse Lexer.token lexbuf in
	close_in cin;
	(fnames, globals)
    with Parsing.Parse_error -> 
      let src_file = "Compiler.parse" in
      let lexeme = Lexing.lexeme lexbuf in
      let msg = "syntax error: unexpected token: "^lexeme in
      let advice = ", rewrite your code" in
      let pos = Lexing.lexeme_start_p lexbuf in
      let loc = 
	(pos.Lexing.pos_fname, pos.Lexing.pos_lnum, 
	 pos.Lexing.pos_cnum-pos.Lexing.pos_bol) 
      in
	Npkcontext.set_loc loc;
	if (not !Npkcontext.accept_gnuc)
	then Npkcontext.report_accept_warning src_file msg Npkcontext.GnuC;
	Npkcontext.report_error src_file (msg^advice)

(* TODO: try to do this using function parse ? factor code with previous 
   function *)
let append_gnu_symbols globals =
  let lexbuf = Lexing.from_string Gnuc.builtins in
    Lexer.init "__gnuc_builtin_symbols" lexbuf;
    Synthack.init_tbls ();
    try 
      let (_, gnuc_symbols) = Parser.parse Lexer.token lexbuf in
	gnuc_symbols@globals
    with Parsing.Parse_error -> 
      Npkcontext.report_error "Compiler.append_gnu_symbols" 
	"unexpected error while parsing GNU C symbols"

let compile fname =
  Npkcontext.print_debug ("Parsing "^fname^"...");
  let (src_fnames, prog) = parse fname in
    Npkcontext.forget_loc ();
    Npkcontext.print_size (Csyntax.size_of prog);
    let prog = 
      if !Npkcontext.accept_gnuc then append_gnu_symbols prog else prog
    in
    let src_fnames = if src_fnames = [] then fname::[] else src_fnames in
      Npkcontext.forget_loc ();
      Npkcontext.print_debug "Parsing done.";
      if !Npkcontext.verb_ast then Csyntax.print prog;
      let prog = 
	if !Npkcontext.accept_goto then begin
	  Npkcontext.print_debug "Running goto elimination...";
	  let g = GotoElimination.run prog in
	    Npkcontext.print_debug "Goto elimination done.";
	    Npkcontext.print_size (Csyntax.size_of g);
	    g
	end else prog
      in
	Npkcontext.print_debug "Typing...";
	let prog = Csyntax2TypedC.process fname prog in
	  Npkcontext.print_debug "Running first pass...";
	  let prog = TypedC2Cir.translate fname prog in
	    Npkcontext.forget_loc ();
	    Npkcontext.print_debug "First pass done.";
	    Npkcontext.print_size (Cir.size_of prog);
	    if !Npkcontext.verb_cir then Cir.print prog;
	    Npkcontext.print_debug ("Translating "^fname^"...");
	    let tr_prog = Cir2npkil.translate Newspeak.C prog src_fnames in
	      Npkcontext.forget_loc ();
	      Npkcontext.print_debug ("Translation done.");
	      Npkcontext.forget_loc ();
	      tr_prog

let eval_exp x =
  match x with
      Cst (Cir.CInt n, _) -> n
    | _ -> 
	Npkcontext.report_error "Compiler.compile_config" 
	  "constant address expected"
  
