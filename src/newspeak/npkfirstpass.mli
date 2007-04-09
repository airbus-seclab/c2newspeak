(** NpkFirstPass contains every useful function run before the actual
    translation. Before translating a file, Npkfirstpass.translate
    makes the following: 
      - deleting CIL's gotos
      - simplifying some exps (StartOf, pointer equality)
      - collecting const string
      - remove unused local vars *)

val code_to_duplicate : (Cil.label, Cil.stmt list) Hashtbl.t


(** TODO: document that *)
val first_pass : 
  Cil.file -> 
  (Npkil.String_set.t * Npkil.String_set.t 
    * (Newspeak.fid, Npkil.fspec_type) Hashtbl.t * Npkil.String_set.t
    * (string, Npkil.glb_type) Hashtbl.t)
