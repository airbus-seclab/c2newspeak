(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language
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

  Jasmine Duchon
  email : jasmine . duchon AT free . fr

*)

(**
 * Various helper functions.
 *
 * @author Jasmine Duchon
 * @author Etienne Millon
 *)

exception NonStaticExpression
exception AmbiguousTypeException

type verbose_level =
  | Silent
  | Debug
  | Warning
  | Error

(** Generic error. *)
val mkerror : verbose_level -> string -> (string -> unit)

(**
 * [nat] exponentiation.
 *)
val puiss   : Syntax_ada.nat -> Syntax_ada.nat -> Syntax_ada.nat

(**
 * Follows Ada's [mod] operator. [a mod b] is defined by the following :
 *   - [a mod b] has the sign of [b]
 *   - [a mod b] has an absolute value less than the absolute value of [b]
 *   - there exists a signed integer [n] such as [a = b*n + (a mod b)]
 *)
val mod_ada : Syntax_ada.nat -> Syntax_ada.nat -> Syntax_ada.nat

(**
 * Follows Ada's [rem] operator. For positive [a] and [b], [a rem b] is the
 * remainder of the Euclidean division of [a] by [b]. The extension to integers
 * is defined by the following relations :
 *   - [  a  rem (-b) =   a rem b ]
 *   - [(-a) rem   b  = -(a rem b)]
 *)
val rem_ada : Syntax_ada.nat -> Syntax_ada.nat -> Syntax_ada.nat

(**
 * Convert a boolean into the native type.
 *)
val nat_of_bool : bool -> Newspeak.Nat.t

(**
 * Check bounds.
 * [between a b n] evaluates to [a <= n <= b].
 *)
val between : float -> float -> float -> bool

(**
 * Checks that a constraint is compatible with another one.
 * [constraint_is_constraint_compatible ref cur] checks if [cur]
 * is compatible with [ref].
 * May raise an error.
 *)
val constraint_check_compatibility :
  Syntax_ada.contrainte -> Syntax_ada.contrainte -> unit

(**
 * Checks that a value is compatible with a constraint.
 * The constraint should be static, and the type of the value already checked
 * against the type of the constraint.
 *)
val value_is_static_constraint_compatible :
  Syntax_ada.contrainte -> Ada_types.data_t -> bool

(**
 * Check a value against a static subtype.
 * If successfull, do nothing [()], else report an error.
 * @raise NonStaticExpression if the subtype is not static.
 *)
val check_static_subtyp:
  Syntax_ada.subtyp -> Ada_types.data_t -> unit

(**
 * Compute the Newspeak integer kind associated to the specified range.
 * See Ada_config.size_of_range
 *)
val ikind_of_range : Syntax_ada.nat -> Syntax_ada.nat -> Newspeak.ikind

(**
 * Check compatibility between types.
 * [check_typ expected found] returns a boolean indicating whether the [found]
 * type is compatible with the [expected] type.
 * @param found      the actual type to be checked
 * @param expected   a typ option : [Some t] means that the type [t] is
                     expected, and [None] means that there is no context type.
 * @return the resulting type
 * As Ada is strongly typed, it is almost the same as testing equality, with
 * one exception : integer constants are compatible with all integer types.
 *)
val check_typ : Syntax_ada.typ option -> Syntax_ada.typ -> Syntax_ada.typ

(**
 * Extract the underlying type of a subtype.
 *)
val base_typ : Syntax_ada.subtyp -> Syntax_ada.typ

(**
 * Extract a subtype from its indication.
 *)
val extract_subtyp :
  Syntax_ada.subtyp_indication -> Syntax_ada.subtyp

(**
 * Directly extract the type from a subtype indication.
 * Functionnaly, [extract_type = function x -> base_typ (extract_subtyp x)].
 *)
val extract_typ : Syntax_ada.subtyp_indication -> Syntax_ada.typ

(**
 * Test the equality of types.
 * Functionnaly, [eq_base_type st1 st2 = ((base_typ st1) = (base_typ st2))].
 *)
val eq_base_typ :
  Syntax_ada.subtyp -> Syntax_ada.subtyp -> bool

(**
 * Similar to [check_typ] but returns a boolean without raising an error.
 *)
val known_compatible_typ :
  Syntax_ada.typ option -> Syntax_ada.typ -> bool

(**
 * Is a type an integer type ?
 *)
val integer_class : Syntax_ada.typ -> bool

(**
 * Guess the type of operands for a binary operation.
 * @param typ the expected type for the result.
 *)
val typ_operand :
  Ast.binary_op -> Syntax_ada.typ option -> Syntax_ada.typ option

(**
 * Check if operands are compatible with a binary operation.
 *)
val check_operand_typ :
  Ast.binary_op -> Syntax_ada.typ -> unit

(**
 * Check a compilation unit against the name of the file around it.
 *)
val check_compil_unit_name :
  Syntax_ada.compilation_unit -> string -> bool

(**
 * Extract the identifier for a representation clause.
 *)
val extract_representation_clause_name :
             Syntax_ada.representation_clause -> string

(**
 * Provides a default value.
 *   - [with_default  None    x] yields [x]
 *   - [with_default (Some v) x] yields [v]
 *)
val with_default : 'a option -> 'a -> 'a

(**
 * Make up a string from a list.
 * @param some list of elements
 * @param a printer
 * @param a separator
 * @param whether the function shall enclose the result between brackets.
 *)
val list_to_string : 'a list -> ('a -> string) -> string -> bool -> string

val ident_list_to_string : string list -> string

(** Converts a qualified name into a dotted string.  *)
val name_to_string : Syntax_ada.name -> string

(** Create a function name for an overloaded operator *)
val make_operator_name : Syntax_ada.binary_op -> string

val operator_of_string : string -> Syntax_ada.binary_op

(**
 * Maybe apply some function to an option value :
 * may f (None) = None
 * may f (Some v) = Some (f v)
 *)
val may : ('a -> 'b) -> 'a option -> 'b option
