(*
  Ada2Newspeak: compiles Ada code into Newspeak. Newspeak is a minimal language
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans, Olivier Levillain

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

  Etienne Millon
  email: etienne.millon AT gmail . com

*)

(**
 * The Ada95 strong typing system.
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

(** Create typed data from a Newspeak value.  *)
val from_nat : t -> Newspeak.Nat.t -> value

(**
 * Gather [Newspeak.Nat] data from a given type.
 * May return [None] if the wrong type is provided.
 *)
val to_nat : value -> Newspeak.Nat.t option

(*****************
 * Symbol tables *
 *****************)
(** {3 Symbol tables} *)

(** The type for symbol tables.  *)
type table

(**
 * Create a new symbol table.
 * Parameter is a size hint.
 *)
val create_table  : int -> table

(** Add a type symbol to a table. *)
val add_type      : table -> string list -> string -> t -> unit

(** Add a variable symbol to a table. *)
val add_variable  : table -> string list -> string -> t -> unit

(** Add a subprogram symbol to a table. *)
val add_subprogram : table
                  -> (string list*string)
                  -> (string*bool*bool*t) list
                  -> t option
      -> unit

(** Remove a type, given its name. *)
val remove_type   : table -> (string list*string)-> unit

(**
 * Get a type from a symbol table.
 * @raise Not_found if no type could be found.
 * @param context : an optional list of packages to be searched
 * @param the queried package
 * @param the queried identifier
 *)
val find_type :    table
               -> ?context:string list list
               ->  string list
               ->  string
            -> t

(**
 * Get a variable from a symbol table.
 * @raise Not_found if no type could be found.
 * @param see [find_type]
 *)
val find_variable :     table
                    -> ?context:string list list
                    ->  string list
                    ->  string
            -> t

(** Pretty-print a symbol table to the standard output. *)
val print_table   : table -> unit

(**********
 * Ranges *
 **********)
(** {3 Ranges} *)

(** The abstract type for ranges.  *)
type range

(** The null range, (holding no elements).  *)
val null_range : range

(**
 * Write a range from its bounds.
 * If [a <= b], [a @.. b] is the range of values between [a] and [b] ;
 * else it is the [null_range].
 *)
val (@..)  : int -> int -> range

(**
 * Write a range from its bounds ([Newspeak.Nat.t] version).
 *)
val (@...) : Newspeak.Nat.t -> Newspeak.Nat.t -> range

(** The number of elements in a range. [0] for the [null_range]. *)
val sizeof : range -> Newspeak.Nat.t

(*********
 * Types *
 *********)

(**
 * {3 Types}
 * The type [t] is an abstraction for what Ada95 types (and subtypes) are.
 *)

(**
 * An unknown type, different from every other one.
 *)
val unknown        : t

(** Derived type. (structural copy) *)
val new_derived    : t -> t

(** Unconstrained subtype. *)
val new_unconstr   : t -> t

(** Constrained subtype. *)
val new_constr     : t -> range -> t

(**
 * Plain integer range.
 * This differs from new_constr integer, because :
 *   - it is a different type
 *   - it does not rely on integer
 *)
val new_range      : range -> t

(**
 * Enumerated type.
 * If [~symboltable] is provided, the set of litterals for
 * the built type will be added to the symbol table.
 *)
val new_enumerated : ?symboltable:table -> string list -> t

(** Floating-point type. Parameter is number of digits. *)
val new_float      : int -> t

(**
 * Array type.
 * The first parameter is the component type ;
 * the second one is a list of index types.
 * @raise Invalid_argument if the index list is empty.
 *)
val new_array      : t -> t list -> t

(** Is a type compatible with another one ?  *)
val is_compatible : t -> t -> bool

(**
 * Pretty-printer for types.
 * Type references (parents, ...) are displayed hashed for brievity's sake.
 *)
val print : t -> string

(*****************
 * Builtin types *
 *****************)

(**
 * {3 Builtin types}
 *)

(** Retrieve a builtin type from its name.  *)
val builtin_type : string -> t

(** Get an attribute for a given type.  *)
val attr_get : t -> string -> value

(** The type for integer constants. *)
val universal_integer : t

(** The type for real constants. *)
val universal_real : t

(** The type for characters and character litterals. *)
val character : t

(** The boolean type. *)
val boolean : t

val natural   : t
val positive  : t
val integer   : t
val std_float : t

(******************
 * Tests on types *
 ******************)

val is_boolean             : t -> bool
val is_scalar              : t -> bool
val is_numeric             : t -> bool
val is_integer             : t -> bool
val is_discrete            : t -> bool

