(** NpkCompile module *)

(** The [compile] function takes the name of a .c file, the name an
    output file (an empty string meaning no output file) and do the
    following: creates the Newspeak tree corresponding to the
    translation of the .c file, writes the results to the optional
    output file and returns it to the caller *)
val compile : string -> string -> unit
