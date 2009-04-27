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
 * Operator less-than for [Syntax_ada.value].
 *)
val inf_val : Syntax_ada.value -> Syntax_ada.value -> bool

(**
 * Operator equals for [Syntax_ada.value].
 *)
val  eq_val : Syntax_ada.value -> Syntax_ada.value -> bool

(**
 * Compute a type_declaration for a new enumeration type,
 * given a list of its discrete values.
 *)
val make_enum : Syntax_ada.identifier list -> Syntax_ada.typ_declaration

(**
 * Check bounds.
 * [between a b n] evaluates to [a <= n <= b].
 *)
val between : 'a -> 'a -> 'a -> bool

(**
 * Checks that a constraint is compatible with another one.
 * [constraint_is_constraint_compatible ref cur] checks if [cur]
 * is compatible with [ref].
 *)
val constraint_is_constraint_compatible :
  Syntax_ada.contrainte -> Syntax_ada.contrainte -> bool

(**
 * Checks that a value is compatible with a constraint.
 * The constraint should be static, and the type of the value already checked
 * against the type of the constraint.
 *)
val value_is_static_constraint_compatible :
  Syntax_ada.contrainte -> Syntax_ada.value -> bool

(**
 * Check a value against a static subtype.
 * If successfull, do nothing [()], else report an error.
 * @raise NonStaticExpression if the subtype is not static.
 *)
val check_static_subtyp:
  Syntax_ada.subtyp -> Syntax_ada.value -> unit

(**
 * Checks whether a constraint is static.
 *)
val constraint_is_static: Syntax_ada.contrainte -> bool

(**
 * Compute the Newspeak integer kind associated to the specified range.
 * See Ada_config.size_of_range
 *)
val ikind_of_range : Syntax_ada.nat -> Syntax_ada.nat -> Newspeak.ikind

(**
 * Build a type_declaration for a constrained integer range.
 *)
val make_range :
  Syntax_ada.expression -> Syntax_ada.expression -> Syntax_ada.typ_declaration

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
  Syntax_ada.binary_op -> Syntax_ada.typ option -> Syntax_ada.typ option

(**
 * Check if operands are compatible with a binary operation.
 *)
val check_operand_typ :
  Syntax_ada.binary_op -> Syntax_ada.typ -> unit

(**
 * Check a compilation unit against the name of the file around it.
 *)
val check_compil_unit_name :
  Syntax_ada.compilation_unit -> string -> bool

(**
 * Extract the identifier for a representation clause.
 *)
val extract_representation_clause_name :
                    Syntax_ada.representation_clause -> Syntax_ada.identifier

(**
 * Provides a default value.
 *   - [with_default  None    x] yields [x]
 *   - [with_default (Some v) x] yields [v]
 *)
val with_default : 'a option -> 'a -> 'a

(**
 * Wraps what the "current package" is and which packages are
 * marked using the "with" and "use" constructs.
 *)
class package_manager :
object
    (** Set the current package. *)
    method set_current :Syntax_ada.package -> unit

    (** Reset the current package. *)
    method reset_current :unit

    (**
     * Get the current package.
     * If [set_current] has not been called, or [reset_current] has been called
     * after the last call to [set_current], the empty package is returned.
     *)
    method current    :Syntax_ada.package

    (** Add a package to the "with" list. *)
    method add_with   :Syntax_ada.package -> unit

    (** Is a package in the "with" list ? *)
    method is_with    :Syntax_ada.package -> bool

    (**
     * Add a "use" clause to the context.
     * The added package must have been added to the "with" list.
     * Adding a package several times is not an error.
     *)
    method add_use    :Syntax_ada.package -> unit

    (**
     * Remove a "use" clause from the context.
     * Removing a package several times is not an error : if a package has been
     * added n times, it is necessary to remove it n times to remove it from the
     * context.
     * Removing a package that is not in the current context will be silently
     * ignored.
     *)
    method remove_use :Syntax_ada.package -> unit

    (**
     * Get the current context.
     * It returns a list of packages that have been added and not removed.
     * The order is not specified.
     *)
    method get_use    :Syntax_ada.package list

    (** Returns the "extern" flag for this manager. *)
    method is_extern :bool

    (** Perform an action with the "extern" flag. *)
    method as_extern_do :(unit->unit)->unit

end

val list_to_string : 'a list -> ('a -> string) -> string -> bool -> string

val name_to_string : Syntax_ada.name -> string

val ident_list_to_string : Syntax_ada.identifier list -> string

(** Create a function name for an overloaded operator *)
val make_operator_name : string -> string

(** Converts a CIR binary operation into its string representation. *)
val operator_of_binop : Syntax_ada.binary_op -> string
