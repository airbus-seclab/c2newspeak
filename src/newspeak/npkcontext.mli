(** The module Npkcontext allows cil2newspeak to keep track of the
    current location in the C files and to report warnings and errors
    to the user. It also regroups every command line option of
    cil2newspeak *)


(** {1 Comand line options } *)

(** When global_zero_init is set, cil2newspeak adds init code for all
    globals *)
val global_zero_init : bool ref

(** When remove_temp is set, only used variables are kept in newspeak
    code *)
val remove_temp : bool ref

(** With accept_extern set, we can have global variables declared and
    not defined *)
val accept_extern : bool ref

(** This option allows horrible casts (int <-> ptr) to be translated *)
val castor_allowed : bool ref

(** This option allows pragmas and ignores them *)
val ignores_pragmas : bool ref

val ignores_cil_merge_errors : bool ref

val mergecil : bool ref


val verb_warnings : bool ref
val verb_debug : bool ref
val verb_cil : bool ref
val verb_newspeak : bool ref

(** If exit_code is set, print_error will exit the program with a non
    zero code *)
val exit_code : bool ref

val compile_only : bool ref

val newspeak_output : string ref

(** Global function to enable or disable every cil2newspeak verbose flag *)
val verbose : bool -> unit -> unit

val argslist : (Arg.key * Arg.spec * Arg.doc) list
val verb_argslist : (Arg.key * Arg.spec * Arg.doc) list
  (** argslist and verb_argsist are the lists of the arguments available
      for newspeak. The distinction allows a pretty printing for penjili
      usage *)

(** {1 Location handling } *)

(** This reference always keeps trac of the current location in the
    file *)
val cur_loc : Newspeak.location ref



(** {1 Warnings/errors generation and display } *)

val print_warning : string -> unit
val print_debug : string -> unit
(** Displays a message to the user *)

(** Throws an Invalid_argument exception with a message *)
val error : string -> 'a

(** Displays a message as a fatal error and exits the program *)
val print_error : string -> 'a

val assumptions : string list ref



