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

  Olivier Levillain
  email: olivier.levillain@penjili.org
*)

(* TODO: 
   instead of having all parameters here and having all files depend
   on it, 
   Should have npkcontex at the end that sets the parameters of each file.
   Not necessarily better ! Since some options are the same across several
   different files.
*)

open Cil

(*----------------------*)
(* Command line options *)
(*----------------------*)

(* Translation options *)
let ignores_asm = ref false
let ignores_pack = ref false
let ignores_volatile = ref false
let accept_gnuc = ref false
let opt_checks = ref true

let forward_goto = ref false

let accept_dirty_syntax = ref false
let strict_syntax = ref false

let global_zero_init = ref true
let castor_allowed = ref false
let ignores_pragmas = ref false
let remove_temp = ref true
let accept_extern = ref false
let accept_flex_array = ref false

let no_opt = ref false
let normalize_loops = ref false

let accept_mult_def = ref false

(* TODO: Handle assumptions correctly *)
(* let assumptions = ref [] *)


(* Verbose options *)

let verb_debug = ref false
let verb_ast = ref false
let verb_npko = ref false
let verb_newspeak = ref false
let pretty_print = ref false
let accept_transparent_union = ref false

let verbose boolean () =
  verb_ast := boolean;
  verb_debug := boolean;
  verb_newspeak := boolean

(* File options *)

let input_files = ref []
let anon_fun file = input_files := file::!input_files
let compile_only = ref false
let output_file = ref ""
let missing_ftyp = ref false


let use_cil = ref false
let cil_printer = ref "default"

type error =
    Asm
  | Pragma
  | Pack
  | Volatile
  | DirtyCast
  | DirtySyntax
  | PartialFunTyp
  | ForwardGoto
  | StrictSyntax
  | ExternGlobal
  | FlexArray
  | MultipleDef
  | GnuC
  | DisableInit
  | DisableOpt
  | DisableCheckOpt
  | TransparentUnion

let flag_of_error err =
  match err with
      Asm -> ignores_asm
    | Pragma -> ignores_pragmas
    | Pack -> ignores_pack
    | Volatile -> ignores_volatile
    | DirtyCast -> castor_allowed
    | DirtySyntax -> accept_dirty_syntax
    | PartialFunTyp -> missing_ftyp
    | ForwardGoto -> forward_goto
    | StrictSyntax -> strict_syntax
    | ExternGlobal -> accept_extern
    | FlexArray -> accept_flex_array
    | MultipleDef -> accept_mult_def
    | GnuC -> accept_gnuc
    | DisableInit -> global_zero_init
    | DisableOpt -> no_opt
    | DisableCheckOpt -> opt_checks
    | TransparentUnion -> accept_transparent_union
 
let opt_of_error err =
  match err with
      Asm -> "--ignore-asm"
    | Pragma -> "--ignore-pragma"
    | Pack -> "--ignore-pack"
    | Volatile -> "--ignore-volatile"
    | DirtyCast -> "--castor"
    | DirtySyntax -> "--accept-dirty-syntax"
    | PartialFunTyp -> "--missing-funtyp"
    | ForwardGoto -> "--accept-forward-goto"
    | StrictSyntax -> "--strict"
    | ExternGlobal -> "--accept-extern"
    | FlexArray -> "--accept-flexible-array"
    | MultipleDef -> "--accept-mult-def"
    | GnuC -> "--accept-gnuc"
    | DisableInit -> "--disable-init"
    | DisableOpt -> "--disable-opt"
    | DisableCheckOpt -> "--disable-checks-opt"
    | TransparentUnion -> "--accept-transparent-union"

(* Version *)

let version = ref false

let argslist = [
  (opt_of_error DirtyCast, Arg.Set (flag_of_error DirtyCast),
   "allows horrible casts to be translated");

  (opt_of_error DirtySyntax, Arg.Set (flag_of_error DirtySyntax),
   "allows dirty syntax");

  (opt_of_error PartialFunTyp, Arg.Set (flag_of_error PartialFunTyp),
   "allows call to function whose argument type is unknown");

  (opt_of_error ForwardGoto, Arg.Set (flag_of_error ForwardGoto),
   "accepts forward goto statements");

  (opt_of_error StrictSyntax, Arg.Set (flag_of_error StrictSyntax),
   "sets strict syntax");
  
  (opt_of_error Pragma, Arg.Set (flag_of_error Pragma),
   "ignores any #pragma directive");

  (opt_of_error Asm, Arg.Set (flag_of_error Asm),
   "ignores any asm directive");

  (opt_of_error Pack, Arg.Set (flag_of_error Pack),
   "ignores any packed attribute");

  (opt_of_error Volatile, Arg.Set (flag_of_error Volatile),
   "ignores 'volatile' type qualifier");

  (opt_of_error TransparentUnion, Arg.Set (flag_of_error TransparentUnion),
   "ignores any transparent union");

  ("--keep-unused-vars", Arg.Clear remove_temp,
   "does not remove unused variables");

  (opt_of_error ExternGlobal, Arg.Set (flag_of_error ExternGlobal),
   "do not raise an error on variables declared but not defined\n");

  (opt_of_error FlexArray, Arg.Set (flag_of_error FlexArray),
   "accept flexible array members");

  (opt_of_error MultipleDef, Arg.Set (flag_of_error MultipleDef),
   "do not raise an error multiple definitions of the same variables\n");

  ("--cil", Arg.Set use_cil, 
   "use CIL lexer and parser instead of our own");

  (opt_of_error GnuC, Arg.Set (flag_of_error GnuC), "allow GNU C extensions");
  
  ("--cil-printer", Arg.Set_string cil_printer,
   "verbose options: uses \"default\" or \"plain\" Cil output");
  
  ("--debug", Arg.Set verb_debug,
   "verbose options: displays more debugging info");
  
  ("--ast", Arg.Set verb_ast,
   "verbose option: displays Abstract Syntax Tree output");

  ("--npko", Arg.Set verb_npko,
   "verbose option: displays NewsPeak Object intermediate output");

  ("--newspeak", Arg.Set verb_newspeak,
   "verbose option: displays Newspeak output");

  ("--pretty", Arg.Set pretty_print,
   "verbose options: uses var names for Newspeak display");

  ("-v", Arg.Unit (verbose true),
   "verbose mode: turn all verbose options on");
    
  ("-q", Arg.Unit (verbose false),
   "quiet mode: turn display off");
    
  ("-c", Arg.Set compile_only,
  "compiles only into a .no file");
  
  ("-o", Arg.Set_string output_file, 
   "gives the name of Newspeak output\n");

  ("--version", Arg.Set version,
   "prints the version of the software");

  (opt_of_error DisableInit, Arg.Clear (flag_of_error DisableInit),
   "turn initialisation of globals to zero off");

  (opt_of_error DisableOpt, Arg.Set (flag_of_error DisableOpt), 
   "turn all code simplifications off");

  (opt_of_error DisableCheckOpt, Arg.Clear (flag_of_error DisableCheckOpt),
   "turn code simplifications that remove checks off");

  ("--one-loop", Arg.Set normalize_loops, "normalize loops");
]





(*-------------------*)
(* Location handling *)
(*-------------------*)

let cur_loc = ref Newspeak.unknown_loc

let set_loc loc = cur_loc := loc
  
let forget_loc () = cur_loc := Newspeak.unknown_loc

let string_of_loc loc = 
  let (file, line, _) = loc in
    if loc = Newspeak.unknown_loc then ""
    else file^":"^(string_of_int line)^": "

let get_fname () =
  let (file, _, _) = !cur_loc in 
    file

let get_loc () = !cur_loc



(*----------------------------------------*)
(* Warnings/errors generation and display *)
(*----------------------------------------*)


(* TODO: Watch this ! *)
module String_set = 
  Set.Make (struct type t = string let compare = Pervasives.compare end)

let old_warnings = ref (String_set.empty)

let string_of_err kind where msg =
  let warn = kind^(string_of_loc !cur_loc)^msg in
    if (!verb_debug && where <> "") then warn^" ("^where^")" else warn

let print_warning where msg =
  let disp = string_of_err "Warning: " where msg in
(* TODO: optimize this away?? *)
    if not (String_set.mem disp !old_warnings)
    then begin
      prerr_endline disp;
      old_warnings := String_set.add disp !old_warnings
    end

let string_of_error = string_of_err ""

let print_debug msg =
  if !verb_debug then 
    prerr_endline ("Debug: "^(string_of_loc !cur_loc)^msg)

let error where msg = invalid_arg (string_of_error where msg)

let exit_on_error msg =
  prerr_endline ("Fatal error: "^msg);
  exit 1


let handle_cmdline_options version_string comment_string = 
  let usage_msg =
    version_string ^ "\nUsage: "^
      Sys.argv.(0)^" [options] [-help|--help] [file...]\n" in
    
    Arg.parse argslist anon_fun usage_msg;
    
    if !version then begin
      print_endline version_string;
      print_endline comment_string;
      exit 0
    end;
    
    if !input_files = [] then begin
      error "C2Newspeak.handle_cmdline_options"
	("no file specified. Try "^Sys.argv.(0)^" --help")
    end;
    
    if (List.length !input_files > 1) && !compile_only 
      && (!output_file <> "") then begin
	error "C2Newspeak.handle_cmdline_options" 
	  ("You cannot specify the output filename (-o) for multiple "
	   ^"files when only compiling (-c)")
      end;
    
    if (not !compile_only) && (!output_file = "") then output_file := "a.npk"
      
(* TODO: do a report_accept_warning *)
let report_ignore_warning loc msg err_typ =
  if not !(flag_of_error err_typ) then begin
    let advice = ", rewrite your code or try option "^(opt_of_error err_typ) in
      error loc (msg^" not supported yet"^advice)
  end;
  print_warning loc (msg^" ignored")
    
let report_accept_warning loc msg err_typ =
  if not !(flag_of_error err_typ) then begin
    let advice = ", rewrite your code or try option "^(opt_of_error err_typ) in
      error loc (msg^advice)
  end;
  print_warning loc (msg^" accepted")

let report_strict_warning msg err =
  if !strict_syntax then print_warning msg err
