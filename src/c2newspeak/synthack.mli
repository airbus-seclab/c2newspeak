open Newspeak

val normalize_base_typ: BareSyntax.base_typ -> Csyntax.typ

val normalize_var_modifier: Csyntax.typ -> BareSyntax.var_modifier -> string option

val normalize_decl: BareSyntax.decl -> string option

val define_type: string -> unit

val is_type: string -> bool

val init_tbls: unit -> unit
