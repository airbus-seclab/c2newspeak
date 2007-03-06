(** NpkCompile module *)

(** The [compile] function takes the name of a .c file and returns the
    Newspeak tree corresponding to the translation of the .c file *)
val compile : string -> Newspeak.t
