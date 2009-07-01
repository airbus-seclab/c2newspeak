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

(*****************
 * Symbol tables *
 *****************)
(** {3 Symbol tables} *)

(** The type for symbol tables.  *)
type table

(** Create a new symbol table.  *)
val create_table  : ?desc:string -> unit -> table

(** Add a type symbol to a table. *)
val add_type      : table -> string -> Ada_types.t -> unit

(** Add a subprogram symbol to a table. *)
val add_subprogram : table
                  -> string
                  -> (string*bool*bool*Ada_types.t) list
                  -> Ada_types.t option
      -> unit

(**
 * Get a type from a symbol table.
 * @raise Not_found if no type could be found.
 * @param context : an optional list of packages to be searched
 * @param the queried package
 * @param the queried identifier
 *)
val find_type :    table
               ->  string
            -> Ada_types.t

exception ParameterlessFunction of Ada_types.t

(**
 * Get a variable from a symbol table.
 * @raise ParamterlessFunction if no variable could be found,
 * but a parameterless function of the same name exists.
 * @param see [find_type]
 *)
val find_variable :     table
                    -> ?expected_type:Ada_types.t
                    ->  string
            -> Ada_types.t

val find_subprogram :     table
                      ->  string
      -> (string*bool*bool*Ada_types.t) list * Ada_types.t option

(** Pretty-print a symbol table to the standard output. *)
val print_table   : table -> string

(** Retrieve a builtin type from its name.  *)
val builtin_type : string -> Ada_types.t

(** (Old) package_manager + proxies on methods *)

(**
 * Add a "use" clause to the context.
 * The added package must have been added to the "with" list.
 * Adding a package several times is not an error.
 *)
val add_use           : table -> string -> unit
(**
 * Remove a "use" clause from the context.
 * Removing a package several times is not an error : if a package has been
 * added n times, it is necessary to remove it n times to remove it from the
 * context.
 * Removing a package that is not in the current context will be silently
 * ignored.
 *)
val remove_use        : table -> string -> unit
(**
 * Get the current context.
 * It returns a list of packages that have been added and not removed.
 * The order is not specified.
 *)
val get_use           : table -> string list
(** Returns the "extern" flag for this manager. *)
val is_extern         : table -> bool
(** Perform an action with the "extern" flag. *)
val as_extern_do      : table -> (unit->unit)->unit
val add_renaming_decl : table -> Syntax_ada.name -> Syntax_ada.name -> unit

(**
 * Context stack.
 * This module represents the lexical scoping in programs.
 *)
module SymStack : sig

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
   * Return the "top" of the stack, reflecting the innermost context.
   *)
  val top : t -> table

  (**
   * Create a new compilation unit :
   *   - discard every context until library level
   *   - create a new package-level context
   *   - push it onto the current stack
   *)
  val new_unit : t -> string -> unit

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
  val s_find_variable :    t
                        -> ?expected_type:Ada_types.t
                        -> ?package:string
                        -> string
             -> Ada_types.t
  val s_find_type     :    t
                        -> ?package:string
                        -> string
             -> Ada_types.t

  (** Add data.  *)
  val s_add_type       : t -> string -> Ada_types.t -> unit
  val s_add_variable   : t -> string -> Ada_types.t -> unit

  val s_add_subprogram : t
                    -> (string)
                    -> (string*bool*bool*Ada_types.t) list
                    -> Ada_types.t option
        -> unit

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
end
