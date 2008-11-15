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


(** The module Npkcontext allows cil2newspeak to keep track of the
    current location in the C files and to report warnings and errors
    to the user. It also regroups every command line option of
    cil2newspeak *)

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

(** {1 Comand line options } *)

(* TODO: remove as many bool refs as possible *)
(** When global_zero_init is set, cil2newspeak adds init code for all
    globals *)
val global_zero_init : bool ref

val gnuc : bool ref
val forward_goto : bool ref
val missing_ftyp : bool ref
val strict_syntax : bool ref

(** When remove_temp is set, only used variables are kept in newspeak
    code *)
val remove_temp : bool ref

(** With accept_extern set, we can have global variables declared and
    not defined *)
val accept_extern : bool ref
val accept_flex_array: bool ref

(** If no_opt is set, then no code simplification is performed *)
val no_opt : bool ref

val normalize_loops : bool ref

val verb_debug : bool ref
val verb_ast : bool ref
val verb_npko : bool ref
val verb_newspeak : bool ref

val opt_checks: bool ref

(** when the pretty_print boolean is set, locals and globals are
    displayed in a prettier way if possible (with their names) *)
val pretty_print : bool ref


(** Names of the files that are to be compiled / link. The first
    string is the name of the file that need to be read, the second is the
    initial name of the .c file; they differ when the files are
    preprocessed. *)
val input_files : string list ref

(** TODO: document *)
val compile_only : bool ref

(** Name of the result file of the process *)
val output_file : string ref


(** TODO: document that *)
val handle_cmdline_options : string -> string -> unit



(** {1 Location handling } *)

(** [set_loc cil_loc] translates a Cil.location cil_loc into a
    Newspeak.location and stores it to track the position in the file *)
val set_loc : Newspeak.location -> unit

val forget_loc : unit -> unit

val get_loc : unit -> Newspeak.location

val get_fname : unit -> string

(** {1 Warnings/errors generation and display } *)

(* rename to report_warning *)
(* TODO: unify these functions!!! into one, with a level!!! *)
(* TODO: remove this function?? or rename? *)
val print_warning : string -> string -> unit
(* TODO: remove this function *)
(* TODO: clean up/simplify npkcontext interface *)
val report_strict_warning: string -> string -> unit

val report_ignore_warning: string -> string -> error -> unit
(** [report_accept_warning file_function message error_type] *)
val report_accept_warning: string -> string -> error -> unit
(** Throws an Invalid_argument exception with a message *)
(* TODO: rename this function into report_error *)
val error : string -> string -> 'a

(** Displays a message to the user *)
val print_debug : string -> unit

(** Displays a message as a fatal error and exits the program *)
val exit_on_error : string -> 'a

(** Whether to use CIL lexer and parser *)
val use_cil: bool ref

(** Name of the printer to use to output CIL syntactic elements *)
val cil_printer: string ref

val string_of_loc: Newspeak.location -> string
