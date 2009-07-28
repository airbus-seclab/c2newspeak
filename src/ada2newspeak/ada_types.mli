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

type data_t =
  | IntVal   of Newspeak.Nat.t (** Integer value        *)
  | FloatVal of float          (** Floating-point value *)
  | BoolVal  of bool           (** Boolean value        *)

val data_eq : data_t -> data_t -> bool

val data_lt : data_t -> data_t -> bool

(** A typed piece of data. *)
type value = t * data_t

(*********
 * Types *
 *********)

(**
 * {3 Types}
 * The type [t] is an abstraction for what Ada95 types (and subtypes) are.
 *)

val new_unknown : string -> t

(** Derived type. (structural copy) *)
val new_derived    : t -> t

(** Constrained subtype. *)
val new_constr     : t -> Syntax_ada.contrainte -> t

(**
 * Plain integer range.
 *)
val new_range      : Syntax_ada.contrainte -> t

(** Enumerated type. *)
val new_enumerated : string list -> t

(** Floating-point type. Parameter is number of digits. *)
val new_float      : int -> t

(**
 * Array type.
 * The first parameter is the component type ;
 * the second one is the index type.
 * @raise Invalid_argument if the index list is empty.
 *)
val new_array  : component:t -> index:t list -> t

val new_record : (string*t) list -> t

(**
 * Is a type compatible with another one ?
 * [is_compatible a b] if a value of type b can be assigned
 * to a lvalue of type b.
 *)
val is_compatible : t -> t -> bool

(* Precondition : (is_record t) *)
val handle_representation_clause : t -> (string * Newspeak.Nat.t) list -> unit

(**
 * unless (is_array t), will return None.
 * Returns (component, index)
 *)
val extract_array_types : t -> (t * t) option

val extract_array_base : t -> Newspeak.Nat.t

(** Precondition : is_unknown t *)
val get_reason : t -> string

val extract_symbols : t -> (string*int) list option

val record_field : t -> string -> int * t

(**
 * Coercion from universal types to finite types.
 *)
val coerce_types : t -> t -> t

(**
 * Pretty-printer for types.
 * Type references (parents, ...) are displayed hashed for brievity's sake.
 *)
val print : t -> string

val print_data : data_t -> string

(*****************
 * Builtin types *
 *****************)

(**
 * {3 Builtin types}
 *)

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

val integer   : t
val std_float : t

(******************
 * Tests on types *
 ******************)

val is_boolean  : t -> bool
val is_scalar   : t -> bool
val is_numeric  : t -> bool
val is_integer  : t -> bool
val is_discrete : t -> bool
val is_float    : t -> bool

val is_unknown  : t -> bool

(****************
 *  Translator  *
 ****************)

val translate : t -> Cir.typ

val check_exp : t -> Cir.exp -> Cir.exp
