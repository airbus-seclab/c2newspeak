(** Cilutils regroups useful functions that could have been provided
    by Cil *)

(** {1 C types } *)

val size_of : Cil.typ -> int

(** [sizeof_sub_type t] returns the size of the type pointed by t and
    raises an exception if t is not a pointer *)
val size_of_subtyp : Cil.typ -> int

val offset_of : Cil.typ -> Cil.offset -> int

(** Thanks to size_of, we store in the following variables the sizes
    in bytes of the types used. As these sizes are architecture
    dependant, they are computed by Cil when Cilutils is loaded *)

val char_size : int
val short_size : int
val int_size : int
val long_size : int
val pointer_size : int
val float_size : int
val double_size : int


(** {1 Display functions } *)

(** setCilPrinter allows to choose between the "plain" CilPrinter and
    the "default" CilPrinter for display functions *)
val setCilPrinter : string -> unit

val string_of_type : Cil.typ -> string
val string_of_exp : Cil.exp -> string
val string_of_global : Cil.global -> string
val string_of_lval : Cil.lval -> string
val string_of_instr : Cil.instr -> string
val string_of_attribute : Cil.attribute -> string

val dump : Pervasives.out_channel -> Cil.file -> unit



(** {1 Miscellaneous } *)

val stmt_of_stmtkind : Cil.stmtkind -> Cil.stmt

val null : Cil.exp
