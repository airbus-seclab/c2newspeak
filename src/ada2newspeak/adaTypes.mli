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

(**
 * A plain piece of data. Used to describe compile-time values.
 *)
type data_t =
  | IntVal   of Newspeak.Nat.t (** Integer value        *)
  | FloatVal of float          (** Floating-point value *)
  | BoolVal  of bool           (** Boolean value        *)

(**
 * Comparison of data_t values.
 * Will raise an error if both sides are not of the same type.
 *)
val data_compare : data_t -> data_t -> int

(*********
 * Types *
 *********)

(**
 * {3 Types}
 * The type [t] is an abstraction for what Ada95 types (and subtypes) are.
 *)

(**
 * Unknown type. This is most useful for debugging.
 * The string parameter is a description of where it comes from.
 *)
val new_unknown : string -> t

(** Derived type. *)
val new_derived    : t -> t

(** Constrained subtype. *)
val new_constr     : t -> AdaSyntax.contrainte -> t

(** Subtype deriving from a enumerated using representation clause. *)
val new_enum     : t -> (string * int) list  -> t
(**
 * Plain integer range.
 *)
val new_range      : AdaSyntax.contrainte -> t

(** Enumerated type. *)
val new_enumerated : string list -> t

(** Floating-point type. Parameter is number of digits. *)
val new_float      : Newspeak.Nat.t -> t

(**
 * Array type.
 * The first parameter is the component type ;
 * the second one is the index type.
 * @raise Invalid_argument if the index list is empty.
 *)
val new_array  : component:t -> index:t list -> t

(**
 * Record type.
 * The argument is a label-types association list.
 *)
val new_record : (string * t) list -> t

(**
 * New access (pointer) type.
 *)
val new_access : t -> t

(**
 * Is a type compatible with another one ?
 * [is_compatible a b] if a value of type b can be assigned
 * to a lvalue of type b.
 *)
val is_compatible : t -> t -> bool

(* Precondition : (is_record t) *)
val handle_enum_repr_clause : t -> (Newspeak.Nat.t * Newspeak.Nat.t) list
                              -> t (*WG!!!unit*)

(**
 * Fetch a value for an enumeration litteral.
 *)
val get_enum_litt_value : t -> data_t -> data_t

(**
 * Returns (component, index)
 *)
val extract_array_types : t -> (t * t list)

val extract_array_range : t -> t 

(**
 * Return the base (first element) of an index type.
 * This allows a correct translation in firstpass :
 * the actual offset is   (index - base) * (component size)
 *)
val extract_base: t -> Newspeak.Nat.t

(** Precondition : is_unknown t *)
val get_reason : t -> string

(**
 * Return a list of symbols for an enumerated type,
 * or None if the type is not enumerated.
 *)
val extract_symbols : t -> (string * int) list option

(**
 * Return the pointee type, or fail.
 *)
val extract_access_type : t -> t

val record_field : t -> string -> int * t

(**
 * Return the record fields'names, or fail.
 *)
val all_record_fields : t -> string list

(**
 * Coercion from universal types to finite types.
 *)
val coerce_types : t -> t -> t

(** For finite types, return all values. *)
val all_values : t -> Newspeak.Nat.t list

(** For finite types, return the number of values. *)
val length_of : t -> Newspeak.Nat.t

(**
 * Pretty-printer for types.
 * Type references (parents, ...) are displayed hashed for brievity's sake.
 *)
val print : t -> string

(** Printer for data_t. *)
val print_data : data_t -> string

(*****************
 * Builtin types *
 *****************)

(**
 * {3 Builtin types}
 *)

(** Get an attribute for a given type.  *)
val attr_get :  t -> string -> AdaSyntax.expression * t

(** The type for integer constants. *)
val universal_integer : t

(** The type for real constants. *)
val universal_real : t

(** Standard.Character *)
val character : t

(** Standard.Boolean *)
val boolean : t

(** Standard.Integer *)
val integer : t

(** Standard.Float *)
val std_float : t

(** System.Address *)
val system_address : t

(******************
 * Tests on types *
 ******************)
                            (* Unk Arr Rec Flo Enu Sig UIn URe Acc *)

val is_array    : t -> bool (*      X                              *)
val is_boolean  : t -> bool (*                  ?                  *)
val is_discrete : t -> bool (*                  X   X   X          *)
val is_float    : t -> bool (*              X               X      *)
val is_integer  : t -> bool (*                      X   X          *)
val is_numeric  : t -> bool (*              X       X   X   X      *)
val is_record   : t -> bool (*          X                          *)
val is_scalar   : t -> bool (*              X   X   X   X   X      *)
val is_unknown  : t -> bool (*  X                                  *)

(*
 * The ? for is_boolean means that it only tests that the type
 * has the same base as Standard.Boolean.
 *)

(****************
 *  Translator  *
 ****************)

(**compute the extrema of a type looking for extra constraints*)
val compute_int_constr: t -> Newspeak.bounds option


(** Compute the CIR type associated to some type. *)
val translate : t -> Cir.typ

(** Returns a check expression, according to the type's constraint. *)
val check_exp : t -> Cir.exp -> Cir.exp
