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



(** {1 Comand line options } *)

(** When global_zero_init is set, cil2newspeak adds init code for all
    globals *)
val global_zero_init : bool ref

(** This option allows horrible casts (int <-> ptr) to be translated *)
val castor_allowed : bool ref

(** This option allows pragmas and ignores them *)
val ignores_pragmas : bool ref

(** When remove_temp is set, only used variables are kept in newspeak
    code *)
val remove_temp : bool ref

(** With accept_extern set, we can have global variables declared and
    not defined *)
val accept_extern : bool ref

val accept_mult_def : bool ref

(** If no_opt is set, then no code simplification is performed *)
val no_opt : bool ref

val normalize_loops : bool ref

(* val assumptions : string list ref *)

val verb_morewarns : bool ref
val verb_debug : bool ref
val verb_cil : bool ref
val verb_npko : bool ref
val verb_newspeak : bool ref
val verb_c : bool ref

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
val handle_cmdline_options : unit -> unit



(** {1 Location handling } *)

(** [set_loc cil_loc] translates a Cil.location cil_loc into a
    Newspeak.location and stores it to track the position in the file *)
val set_loc : Newspeak.location -> unit

val forget_loc : unit -> unit

val get_loc : unit -> Newspeak.location

val get_fname : unit -> string

(** {1 Warnings/errors generation and display } *)

val print_warning : string -> string -> unit
val print_morewarn : string -> string -> unit
val print_debug : string -> unit
(** Displays a message to the user *)

(** Throws an Invalid_argument exception with a message *)
val error : string -> string -> 'a

(** Displays a message as a fatal error and exits the program *)
val print_error : string -> 'a
