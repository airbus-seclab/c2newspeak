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
val create_table  : unit -> table

(** Add a type symbol to a table. *)
val add_type      : table -> string list*string -> Ada_types.t -> unit

(** Add a variable symbol to a table. *)
val add_variable  : table -> string list*string -> Ada_types.t -> unit

(** Add a subprogram symbol to a table. *)
val add_subprogram : table
                  -> (string list*string)
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
               ->  string list*string
            -> Ada_types.t

(**
 * Get a variable from a symbol table.
 * @raise Not_found if no type could be found.
 * @param see [find_type]
 *)
val find_variable :     table
                    ->  string list*string
            -> Ada_types.t

val find_subprogram :     table
                      ->  string list*string
      -> (string*bool*bool*Ada_types.t) list * Ada_types.t option

val add_renaming_declaration :    table
                               -> string
                               -> string
                     -> unit

(** Pretty-print a symbol table to the standard output. *)
val print_table   : table -> unit

(** Retrieve a builtin type from its name.  *)
val builtin_type : string -> Ada_types.t

(** (Old) package_manager + proxies on methods *)

(** Set the current package. *)
val set_current       : table -> Syntax_ada.name -> unit
(** Reset the current package. *)
val reset_current     : table -> unit
(**
 * Get the current package.
 * If [set_current] has not been called, or [reset_current] has been called
 * after the last call to [set_current], the empty package is returned.
 *)
val current           : table -> Syntax_ada.package
(** Add a package to the "with" list. *)
val add_with          : table -> Syntax_ada.name -> unit
(** Is a package in the "with" list ? *)
val is_with           : table -> Syntax_ada.package -> bool
(**
 * Add a "use" clause to the context.
 * The added package must have been added to the "with" list.
 * Adding a package several times is not an error.
 *)
val add_use           : table -> Syntax_ada.name -> unit
(**
 * Remove a "use" clause from the context.
 * Removing a package several times is not an error : if a package has been
 * added n times, it is necessary to remove it n times to remove it from the
 * context.
 * Removing a package that is not in the current context will be silently
 * ignored.
 *)
val remove_use        : table -> Syntax_ada.name -> unit
(**
 * Get the current context.
 * It returns a list of packages that have been added and not removed.
 * The order is not specified.
 *)
val get_use           : table -> Syntax_ada.package list
(** Returns the "extern" flag for this manager. *)
val is_extern         : table -> bool
(** Perform an action with the "extern" flag. *)
val as_extern_do      : table -> (unit->unit)->unit
(** FIXME document exact specs *)
val normalize_name    : table -> Syntax_ada.name -> bool -> Syntax_ada.name
val add_renaming_decl : table -> Syntax_ada.name -> Syntax_ada.name -> unit

(**
 * Context stack.
 * This module represents the lexical scoping in programs.
 *)
module Stack : sig

  (**
   * Abstract type for context stacks.
   * A context stack is a non-empty list of symbol tables.
   * Impure (has side effects).
   *
   * For example, this program :              will look like this :
   *
   *                                   +---------------------------------------+
   *                                   | Standard library          (RO, NOPOP) |
   *                                   |   - Integer : type                    |
   *                                   |   - True    : variable (type=Boolean) |
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
   *                                   | local decl_part                       |
   *                                   |   - Z       : variable (type=Integer) |
   *                                   +---------------------------------------+
   *                                   |
   *                                   v stack grows this way
   *
   *)
  type t

  (**
   * Create a new context stack, initially holding
   * built-in types and variables, and an empty library.
   *)
  val create : unit -> t

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
  val enter_context : t -> string option-> unit

  (**
   * Discard the current context and "go up".
   * Named contexts can remain accessible.
   *)
  val exit_context : t -> unit
end
