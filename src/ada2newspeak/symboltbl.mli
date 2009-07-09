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
 * Abstract type for context stacks.
 * A context stack is a non-empty list of symbol tables.
 * Impure (has side effects).
 *
 * For example, this program                will look like this :
 *                           \
 *                            |      +---------------------------------------+
 *                            |      | Standard library          (RO, NOPOP) |
 *                           /       |   - Integer : type                    |
 *                         |_        |   - True    : variable (type=Boolean) |
 *                                   |   - ...                               |
 *                                   +---------------------------------------+
 *  package body p is                | Program library               (NOPOP) |
 *    -- in spec : X : Integer;      |   - p       : package                 |
 *    procedure F (T: in Boolean) is +---------------------------------------+
 *      Y : Integer;                 | Package                               |
 *    begin                          |   - X       : variable (type=Integer) |
 *      declare                      +---------------------------------------+
 *        Z : Integer;               | SP parameters                         |
 *      begin                        |   - T       : variable (type=Boolean) |
 *        null;                      +---------------------------------------+
 *      end;                         | SP decl_part                          |
 *    end F;                         |   - Y       : variable (type=Integer) |
 *  end p;                           +---------------------------------------+
 *                                   | local decl_part                 (TOP) |
 *                                   |   - Z       : variable (type=Integer) |
 *                                   +---------------------------------------+
 *                                   |
 * RO = nothing can be added         |
 * NOPOP = exit_context              | stack grows this way
 *         will complain             v
 *
 *)
type t

(**
 * Create a new context stack, initially holding
 * built-in types and variables, and an empty library.
 *)
val create : unit -> t

(**
 * Pretty-printer.
 *)
val print : t -> string

(**
 * Create a new context and enter into it.
 * Contexts may be named ; in that case a unit_symbol is added
 * to the parent table in order to allow bottom-up adressing.
 *)
val enter_context : ?name:string -> ?desc:string -> ?weakly:bool -> t -> unit

(**
 * Discard the current context and "go up".
 * Named contexts can remain accessible.
 *)
val exit_context : t -> unit

(** FIXME document exact specs *)
val normalize_name    : t -> Syntax_ada.name -> bool -> Syntax_ada.name

(**
 * Find the intersection of possible types.
 * Used for example to resolve overloading in binary operations.
 *)
val type_ovl_intersection : t
                          -> string
                          -> string
           -> Ada_types.t

(** Find data.  *)
exception ParameterlessFunction of Ada_types.t
val s_find_variable :    t
                      -> ?expected_type:Ada_types.t
                      -> string option * string
           -> Ada_types.t

val s_find_variable_value :    t
                      -> ?expected_type:Ada_types.t
                      -> string option * string
           -> Ada_types.t*(Ada_types.data_t option)

val s_find_type     :    t
                      -> string option * string
           -> Ada_types.t
val s_find_subprogram : t
                      -> string option * string
    -> (string*bool*bool*Ada_types.t) list * Ada_types.t option


(** Add data.  *)
val s_add_type       : t -> string -> Ada_types.t -> unit
val s_add_variable   : t -> string -> ?value:Ada_types.data_t -> Ada_types.t -> unit

val s_add_subprogram : t
                  -> (string)
                  -> (string*bool*bool*Ada_types.t) list
                  -> Ada_types.t option
      -> unit

val s_add_use : t -> string -> unit

(** Set the current package. *)
val set_current       : t -> string -> unit
(** Reset the current package. *)
val reset_current     : t -> unit
(**
 * Get the current package.
 * If [set_current] has not been called, or [reset_current] has been called
 * after the last call to [set_current], the empty package is returned.
 *)
val current           : t -> string option
(** Add a package to the "with" list. *)
val add_with          : t -> string -> unit
(** Is a package in the "with" list ? *)
val is_with           : t -> string -> bool
val s_get_use         : t -> string list

val add_renaming_decl : t -> string -> Syntax_ada.name -> unit

(*********************
 * Post-build search *
 *********************)

val first_child  : t -> unit
val next_sibling : t -> unit
