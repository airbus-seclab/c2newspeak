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

open Ada_utils

let parse fname =
  Npkcontext.print_debug ("Parsing " ^ fname ^ "...");
  let (ast:AdaSyntax.compilation_unit) = File_parse.parse fname in
    Npkcontext.forget_loc ();
    if (!Npkcontext.verb_ast) then begin
      print_endline "Abstract Syntax Tree";
      print_endline "----------------------";
      Print_syntax_ada.print_ast [ast];
      print_newline ();
    end;
    Npkcontext.print_debug ("Done parsing " ^ fname);
    ast

let normalization fname ast =
  let norm_tree = Normalize.normalization ast in
    Npkcontext.print_debug ("Translating " ^ fname ^ "...");
    norm_tree

let firstpass_translate fname norm_tree =
  Npkcontext.print_debug "Translating to CIR...";
  let prog = Firstpass.translate norm_tree in
    Npkcontext.print_debug ("Done translating " ^ fname);
    Npkcontext.forget_loc ();
    prog

let translate prog =
  let tr_prog = Cir2npkil.translate Newspeak.ADA prog in
    Npkcontext.forget_loc ();
    tr_prog

(**
 * Parse a file, compile it and translate it into a Npkil representation.
 *)
let compile (fname: string): Npkil.t =
  if not (Filename.check_suffix fname Params.ada_suffix) then begin
    Npkcontext.report_error "Ada2newspeak.compile"
      (fname ^ " is not a .adb file")
  end;
  let base_name = Filename.basename fname in
  let dir_name = Filename.dirname fname in
  let current_dir = Sys.getcwd () in
    if dir_name <> "." then begin
      Npkcontext.print_debug ("Changing directory : " ^ dir_name);
      Sys.chdir dir_name
    end;
    let ast = parse base_name in
    (* Find a cleaner way to Go into the spec in order 
       to add 'with' clauses
    *)
    let spec_name = (Filename.chop_suffix  base_name  
		       Params.ada_suffix)^Params.ada_spec_suffix 
    in
    let spec_clauses = 
      if (Sys.file_exists spec_name)
      then 
	let (spec_cls, _, _) = parse spec_name in
	  spec_cls
      else []
    in

    let (clauses, libs, locat) = ast in
      
    let not_in_clauses cls y = 
      let eq_clause cl y = 
	match cl, y with 
	    AdaSyntax.With(s1, _), AdaSyntax.With(s2, _) 
	  | AdaSyntax.UseContext s1 , AdaSyntax.UseContext s2 -> 
	      compare s1 s2 = 0
	  | _ -> false 
      in
	List.for_all (fun x -> not (eq_clause x y)) cls
    in  
    let added_clauses = 
      List.fold_left (
	fun x y -> if ( not_in_clauses clauses y) then  y::x else x
      ) [] spec_clauses  
    in 
    let updated_clauses =   
      List.append clauses (List.rev added_clauses ) in
    let n_ast = ( updated_clauses, libs, locat ) in
    let norm_tree = normalization fname n_ast in
    let prog = firstpass_translate fname norm_tree in
    let tr_prog = translate prog in
      if dir_name <> "." then begin
	Npkcontext.print_debug ("Changing directory : " ^ current_dir);
	Sys.chdir current_dir
      end;
      if (!Npkcontext.verb_npko) then begin
	print_endline "Newspeak Object output";
	print_endline "----------------------";
	Npkil.dump tr_prog;
	print_newline ();
      end;
      tr_prog


let create_no name = (Filename.chop_extension name) ^ Params.npko_suffix

let extract_no fname =
  if Filename.check_suffix fname Params.npko_suffix then fname
  else begin
    let no = create_no fname in
    let prog = compile fname in
      Npkil.write no prog;
      no
  end
    
let execute () =
    (* TODO: this code should be factored with c2newspeak!!! into x2newspeak *)
  Normalize.init_bodies !Npkcontext.input_files;

  let nos = List.map extract_no !Npkcontext.input_files in
    
    let bods = Normalize.bodies_to_add() in

    let bods_less_files = List.filter (fun x -> not (
	 List.mem x !Npkcontext.input_files)) bods 
    in
	let bodies_nos = List.map extract_no (
	List.rev bods_less_files) 
	in 
      if not !Npkcontext.compile_only then begin
	Linker.link (List.append bodies_nos nos)
      end
	
	
let _ =
  X2newspeak.process Params.version_string Params.comment_string execute
    
