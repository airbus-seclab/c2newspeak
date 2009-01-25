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
  let specs = ref [] in
    Lexer.init fname lexbuf;
    Synthack.init_tbls ();
    try
      let (fnames, globals) = Parser.parse (Lexer.token specs) lexbuf in
	close_in cin;
	(fnames, globals, List.rev !specs)
    with Parsing.Parse_error -> 
      let loc = "Compiler.parse" in
      let lexeme = Lexing.lexeme lexbuf in
      let msg = "syntax error: unexpected token: "^lexeme in
      let advice = ", rewrite your code" in
	if (not !Npkcontext.accept_gnuc)
	then Npkcontext.report_accept_warning loc msg Npkcontext.GnuC;
	Npkcontext.error loc (msg^advice)

(* TODO: try to do this using function parse ? factor code with previous 
   function *)
let append_gnu_symbols globals =
  let lexbuf = Lexing.from_string Gnuc.builtins in
    Lexer.init "__gnuc_builtin_symbols" lexbuf;
    Synthack.init_tbls ();
    try 
      let (_, gnuc_symbols) = Parser.parse (Lexer.token (ref [])) lexbuf in
	gnuc_symbols@globals
    with Parsing.Parse_error -> 
      Npkcontext.error "Compiler.append_gnu_symbols" 
	"unexpected error while parsing GNU C symbols"

let compile fname =
  Npkcontext.print_debug ("Parsing "^fname^"...");
  let (fnames, globals, spec) = parse fname in
  let globals = 
    if !Npkcontext.accept_gnuc then append_gnu_symbols globals else globals
  in
  let fnames = if fnames = [] then fname::[] else fnames in
    Npkcontext.forget_loc ();
    Npkcontext.print_debug "Parsing done.";
    Npkcontext.print_debug "Running first pass...";
    let prog = Firstpass.translate (globals, spec) in
      Npkcontext.forget_loc ();
      Npkcontext.print_debug "First pass done.";
      if !Npkcontext.verb_cir then Cir.print prog;
      Npkcontext.print_debug ("Translating "^fname^"...");
      let tr_prog = Cir2npkil.translate prog fnames in
      Npkcontext.print_debug ("Translation done.");
	Npkcontext.forget_loc ();
	tr_prog

let eval_exp x =
  match x with
      Cst (Cir.CInt n, _) -> n
    | _ -> 
	Npkcontext.error "Compiler.compile_config" "constant address expected"
  
let compile_config fname =
  let cin = open_in fname in
  let lexbuf = Lexing.from_channel cin in    
    Lexer.init fname lexbuf;
    try 
(* TODO: use another function than Lexer.token here, the ref [] is not useful
*)
      let mem_zones = Parser.config (Lexer.token (ref [])) lexbuf in
      let translate (addr, sz) =
	let addr = eval_exp addr in
	let sz = Nat.mul (eval_exp sz) (Nat.of_int 8) in
	let sz = 
	  try Nat.to_int sz
	  with _ -> 
	    Npkcontext.error "Compiler.config" 
	      "size of memory zone too large"
	in
	  (addr, sz)
      in
	List.map translate mem_zones
    with Parsing.Parse_error -> 
      let loc = "Compiler.compile_config" in
      let lexeme = Lexing.lexeme lexbuf in
	Npkcontext.error loc ("syntax error: unexpected token: "^lexeme)
