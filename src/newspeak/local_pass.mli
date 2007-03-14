(** NpkFirstPass contains every useful function run before the actual
    translation. Before translating a file, Npkfirstpass.translate
    makes the following: 
      - deleting CIL's gotos
      - simplifying some exps (StartOf, pointer equality)
      - collecting const string
      - remove unused local vars *)

val code_to_duplicate : (Cil.label, Cil.stmt list) Hashtbl.t


(** TODO: document that *)
val translate : Cil.file -> 
  ((Cil.varinfo * bool * Cil.init option) * Cil.location) list *
    Cil.fundec list *
  ((string * Cil.typ * (string * Cil.typ * Cil.attributes) list option) * Cil.location) list *
    string list * string list * string list
