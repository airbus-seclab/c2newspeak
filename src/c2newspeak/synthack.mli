open Newspeak

type base_typ =
    | Void 
    | Integer of ikind
    | Float of int
    | Struct of (string * field list option)
    | Union of (string * field list option)
    | Name of string
    | Enum of ((string * Csyntax.exp option) list) option
    | Va_arg
    | Typeof of string

and var_modifier = (int * modifier)

and modifier =
    | Abstract
    | Variable of (string * location)
    | Function of (var_modifier * decl list)
    | Array of (var_modifier * Csyntax.exp option)

and decl = (base_typ * var_modifier)

and field = (base_typ * var_modifier * Csyntax.exp option)

type vdecl = (Csyntax.typ * string option * Newspeak.location)
(* TODO: do not group these together *)
type sdecls = (string * Csyntax.decl) list

val normalize_base_typ: base_typ -> (sdecls * Csyntax.typ)

val normalize_var_modifier: Csyntax.typ -> var_modifier -> vdecl

val normalize_decl: decl -> (sdecls * vdecl)

val define_type: string -> Csyntax.typ -> unit

val is_type: string -> bool

val init_tbls: unit -> unit
