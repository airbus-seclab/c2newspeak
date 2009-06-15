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

val import_table : table -> table -> unit

(**
 * Get a type from a symbol table.
 * @raise Not_found if no type could be found.
 * @param context : an optional list of packages to be searched
 * @param the queried package
 * @param the queried identifier
 *)
val find_type :    table
               -> ?context:string list list
               ->  string list*string
            -> Ada_types.t

(**
 * Get a variable from a symbol table.
 * @raise Not_found if no type could be found.
 * @param see [find_type]
 *)
val find_variable :     table
                    -> ?context:string list list
                    ->  string list*string
            -> Ada_types.t

val find_subprogram :     table
                      -> ?context:string list list
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
val package : table -> Ada_utils.package_manager

val set_current : table -> Syntax_ada.name -> unit
val reset_current : table -> unit
val current : table ->Syntax_ada.package
val add_with   : table -> Syntax_ada.name -> unit
val is_with    : table -> Syntax_ada.package -> bool
val add_use    : table -> Syntax_ada.name -> unit
val remove_use : table -> Syntax_ada.name -> unit
val get_use    : table -> Syntax_ada.package list
val is_extern : table -> bool
val as_extern_do : table -> (unit->unit)->unit
val normalize_name : table ->  Syntax_ada.name -> bool -> Syntax_ada.name
val add_renaming_decl : table ->  Syntax_ada.name -> Syntax_ada.name -> unit

(**
 * Context stack.
 * This module represents the lexical scoping in programs.
 *)
module Stack : sig

  (**
   * Abstract type for context stacks.
   * A context stack is a non-empty list of symbol tables.
   * Impure (has side effects).
   *)
  type t

  (**
   * Create a new context stack, initially holding
   * built-in types and variables.
   *)
  val create : unit -> t

  (**
   * Return the "top" of the stack, reflecting the innermost context.
   *)
  val top : t -> table

  (**
   * Create a new context and enter into it.
   *)
  val enter_context : t -> unit

  (**
   * Discard the current context and "go up".
   *)
  val exit_context : t -> unit
end
