open Newspeak

type base_typ =
    | Void 
    | Integer of ikind
    | Float of int
    | Struct of (string * field list option)
    | Union of (string * field list option)
    | Name of string
    | Enum of ((string * Csyntax.exp option) list * Newspeak.location) option

and var_modifier = 
    | Abstract
    | Variable of (string * location)
    | Function of (var_modifier * decl list)
    | Array of (var_modifier * Csyntax.exp option)
    | Pointer of var_modifier

and decl = (base_typ * var_modifier)

and field = (base_typ * var_modifier * Csyntax.exp option)

type vdecl = (Csyntax.typ * string option * location)
type edecl = (Csyntax.enumdecl * Newspeak.location)

val normalize_base_typ: base_typ -> (edecl list * Csyntax.typ)

val normalize_var_modifier: Csyntax.typ -> var_modifier -> vdecl

val normalize_decl: decl -> (edecl list * vdecl)

val define_type: string -> Csyntax.typ -> unit

val is_type: string -> bool

val get_fnames: unit -> string list

val add_fname: string -> unit
