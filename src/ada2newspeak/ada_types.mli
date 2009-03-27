(* FIXME + license *)

(**
 * The Ada95 type system.
 * @author Etienne Millon
 *)

(** The abstract OCaml type for Ada types.  *)
type t

(**********
 * Values *
 **********)
(** {3 Values } *)

(** A typed piece of data. *)
type value

(** Create typed data from an int.  *)
val from_int : t -> int -> value

(**
 * Gather int data from a given type.
 * May return [None] if the wrong type is provided.
 *)
val to_int : value -> int option

(** Checked equality on values.   (hard comparison) *)
val (@=) : value -> value -> bool

(** Unchecked equality on values. (soft comparison) *)
val (@=?) : value -> value -> bool

(*****************
 * Symbol tables *
 *****************)
(** {3 Symbol tables} *)

(** The type for symbol tables.  *)
type table

(**
 * Create a new symbol table.
 * The parameter is a size hint.
 *)
val create_table  : int -> table

(** Add a type symbol to a table. *)
val add_type      : table -> string -> t     -> unit

(** Add a variable symbol to a table. *)
val add_variable  : table -> string -> value -> unit

(** Get a type from a symbol table. *)
val find_type     : table -> string -> t

(** Get a variable from a symbol table. *)
val find_variable : table -> string -> value

(** Pretty-print a symbol table to the standard output. *)
val print_table   : table -> unit

(**********
 * Ranges *
 **********)
(** {3 Ranges} *)

(** The abstract type for ranges.  *)
type range

(** Write a range from its bounds.  *)
val (@..) : int -> int -> range

(** The number of elements in a range.  *)
val sizeof : range -> int

(** The null range, that is to say, an empty range.  *)
val null_range : range

(*********
 * Types *
 *********)
(** {3 Types} *)

(** Enumerated type. *)
val new_enumerated : ?symboltable:table -> ?name:string -> string list -> t

(** Derived type. (structural copy) *)
val new_derived    : ?symboltable:table -> ?name:string -> t -> t

(** Unconstrained subtype.  *)
val new_unconstr   : ?symboltable:table -> ?name:string -> t -> t

(** Constrained subtype.  *)
val new_constr     : ?symboltable:table -> ?name:string -> t -> range -> t

(** Modular type. Parameter is modulus. *)
val new_modular    : ?symboltable:table -> ?name:string -> int -> t

(** Floating-point type. Parameter is number of digits. *)
val new_float      : ?symboltable:table -> ?name:string -> int -> t

(**
 * Array type.
 * The first parameter is the component type ;
 * the second one is a list of index types.
 * @raise Invalid_argument if the index list is empty.
 *)
val new_array      : ?symboltable:table -> ?name:string -> t -> t list -> t

(** Is a type compatible with another one ?  *)
val is_compatible : t -> t -> bool

(** Retrieve a builtin type from its name.  *)
val builtin_type : string -> t

(** Get an attribute for a given type.  *)
val attr_get : t -> string -> value list -> value

(**
 * Shortcut for [attr_get] with no arguments.
 * st @. "ident" is like st'ident in Ada.
 *)
val (@.) : t -> string -> value

(** Binary operators.  *)
val operator_exists : t -> string -> bool

(** Extract a type from a value. *)
val typeof : value -> t
