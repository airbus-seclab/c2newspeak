open Newspeak

type vdecl = (Csyntax.typ * string option * Newspeak.location)
(* TODO: do not group these together *)
type sdecls = (string * Csyntax.decl) list

val normalize_base_typ: BareSyntax.base_typ -> (sdecls * Csyntax.typ)

val normalize_var_modifier: Csyntax.typ -> BareSyntax.var_modifier -> vdecl

val normalize_decl: BareSyntax.decl -> (sdecls * vdecl)

val define_type: string -> Csyntax.typ -> unit

val is_type: string -> bool

val init_tbls: unit -> unit
