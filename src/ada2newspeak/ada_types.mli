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

(** Create typed data from a Newspeak value.  *)
val from_nat : t -> Newspeak.Nat.t -> value

(**
 * Gather [Newspeak.Nat] data from a given type.
 * May return [None] if the wrong type is provided.
 *)
val to_nat : value -> Newspeak.Nat.t option

(**
 * Checked equality on values.
 * [a @= b] if [a] and [b] denote the same typed value.
 *)
val (@=) : value -> value -> bool

(**
 * Unchecked equality on values.
 * This comparison is without checking the type compatibility.
 * If [a @= b], it is guaranteed that [a @=? b].
 *)
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

(**
 * Get a type from a symbol table.
 * @raise Not_found if no type could be found.
 *)
val find_type     : table -> string -> t

(**
 * Get a variable from a symbol table.
 * @raise Not_found if no type could be found.
 *)
val find_variable : table -> string -> value

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
 * Type constructors have two optional arguments, which should be given
 * together. [~symboltable] is the name of a [table] to which the type is to be
 * add with the given [~name].
 *)

(** Derived type. (structural copy) *)
val new_derived    : ?symboltable:table -> ?name:string -> t -> t

(** Unconstrained subtype. *)
val new_unconstr   : ?symboltable:table -> ?name:string -> t -> t

(** Constrained subtype. *)
val new_constr     : ?symboltable:table -> ?name:string -> t -> range -> t

(** Modular type. Parameter is modulus. *)
val new_modular    : ?symboltable:table -> ?name:string -> int -> t

(**
 * Plain integer range.
 * This differs from new_constr integer, because :
 *   - it is a different type
 *   - it does not rely on integer
 *   - it does not need an explicit "universal_integer" type
 *)
val new_range      : ?symboltable:table -> ?name:string -> range -> t

(**
 * Enumerated type.
 * [~symboltable] and [~name] have an additional meaning : if provided, the set
 * of litterals for the constructed type will be added to the symbol table.
 *)
val new_enumerated : ?symboltable:table -> ?name:string -> string list -> t

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
