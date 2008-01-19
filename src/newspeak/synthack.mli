open Newspeak

type base_typ =
    | Void 
    | Integer of ikind
    | Float of int
    | Struct of (string * field list option)
    | Union of (string * field list option)
    | Name of string
    | Enum of ((string * Int64.t option) list * Newspeak.location) option

and var_modifier = 
    | Abstract
    | Variable of (string * location)
(* true is variable argument list *)
    | Function of (var_modifier * decl list * bool)
    | Array of (var_modifier * Bare_csyntax.exp option)
    | Pointer of var_modifier

and decl = (base_typ * var_modifier)

and field = (base_typ * var_modifier * Int64.t option)

type vdecl = (Bare_csyntax.typ * string * location)
type edecl = (Bare_csyntax.enumdecl * Newspeak.location)

val normalize_base_typ: base_typ -> (edecl list * Bare_csyntax.typ)

val normalize_var_modifier: Bare_csyntax.typ -> var_modifier -> vdecl

val normalize_decl: decl -> (edecl list * vdecl)

val get_compdefs: unit -> Bare_csyntax.compdefs

val define_type: string -> Bare_csyntax.typ -> unit

val is_type: string -> bool
