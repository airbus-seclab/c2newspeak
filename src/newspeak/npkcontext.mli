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

(* val assumptions : string list ref *)

val verb_morewarns : bool ref
val verb_debug : bool ref
val verb_cil : bool ref
val verb_npko : bool ref
val verb_newspeak : bool ref


(** Names of the files that are to be compiled / link. The first
    string is the name of the file that need to be read, the second is the
    initial name of the .c file; they differ when the files are
    preprocessed. *)
val input_files : (string * string) list ref

(** TODO: document *)
val compile_only : bool ref

(** Name of the result file of the process *)
val output_file : string ref


(** TODO: document that *)
val handle_cmdline_options : unit -> unit



(** {1 Location handling } *)

(** This reference always keeps trac of the current location in the
    file *)
val cur_loc : Newspeak.location ref



(** {1 Warnings/errors generation and display } *)

val print_warning : string -> string -> unit
val print_morewarn : string -> string -> unit
val print_debug : string -> unit
(** Displays a message to the user *)

(** Throws an Invalid_argument exception with a message *)
val error : string -> string -> 'a

(** Displays a message as a fatal error and exits the program *)
val print_error : string -> 'a
