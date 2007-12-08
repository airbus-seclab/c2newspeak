open Newspeak

type base_typ =
    | Void 
    | Integer of (sign_t * ityp)    
    | Struct of (string * decl list option)
    | Union of (string * decl list option)
    | Name of string

and var_modifier = 
    | Abstract
    | Variable of (string * location)
    | Function of (var_modifier * decl list)
    | Array of (var_modifier * Int64.t option)
    | Pointer of var_modifier

and ityp = 
    | Char 
    | Short
    | Int
    | Long
    | LongLong

and decl = (base_typ * var_modifier)

val normalize_base_typ: base_typ -> Csyntax.typ

val normalize_var_modifier: 
  Csyntax.typ -> var_modifier -> (Csyntax.typ * string * location)

val normalize_decl: decl -> (Csyntax.typ * string * location)

val clean: unit -> unit

val define_type: string -> Csyntax.typ -> unit

val is_type: string -> bool

