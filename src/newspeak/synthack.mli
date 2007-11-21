open Newspeak

type base_typ =
    | Void 
    | Integer of (sign_t * ityp)    
    | Struct of decl list
    | Union of decl list
    | Name of string

and var_modifier = 
    | Abstract
    | Variable of string
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

val normalize_decl: decl -> (Csyntax.typ * string)

val clean: unit -> unit

val define_type: string -> Csyntax.typ -> unit

val is_type: string -> bool
