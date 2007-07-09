type t

val universe: int -> t

val forget: t -> t

val assign: t -> Newspeak.lval -> Newspeak.exp -> t

val push: t -> t

val pop: t -> t

val exp_of_local: t -> (Newspeak.lval * Newspeak.scalar_t) -> Newspeak.exp

val to_string: t -> string
