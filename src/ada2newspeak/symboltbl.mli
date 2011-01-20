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
val enter_context : ?name:string -> ?desc:string -> t -> unit

(**
 * Return the current context and "go up".
 *)
val exit_context : t -> unit

type scope = Lexical | In_package of string

(**
 * Find the intersection of possible types.
 * Used for example to resolve overloading in binary operations.
 *)
val get_possible_common_type: 
  t -> AdaSyntax.lval -> AdaSyntax.lval -> AdaTypes.t option

(** Find data.  *)

exception Parameterless_function of scope * string * AdaTypes.t
exception Variable_no_storage    of AdaTypes.t * AdaTypes.data_t

val find_variable :    t
                    -> ?expected_type: AdaTypes.t
                    -> string option * string
         -> scope * (string * AdaTypes.t * bool)

val find_variable_with_error_report:
  t -> ?expected_type: AdaTypes.t -> string option * string
  -> scope * (string * AdaTypes.t * bool)


(* TODO: try to remove all optional arguments *)

val find_variable_value:
  t -> ?expected_type: AdaTypes.t
  -> string option * string
  -> scope * (string * AdaTypes.t * (AdaTypes.data_t option) * bool)

val find_type: t -> string option * string -> scope * AdaTypes.t

(* TODO: try to remove ?silent option *)
val find_subprogram : t
                    -> ?silent: bool
                    -> string option * string
                    -> (string option * AdaTypes.t) list
                    ->  AdaTypes.t option
                    -> ( string option * string -> scope * AdaTypes.t)
    -> scope * (string * AdaSyntax.param list * AdaTypes.t option)

val is_already_defined : t  -> string -> bool

val is_operator_overloaded : t -> string -> bool

(** Add data.  *)
val add_type     : t -> string -> Newspeak.location -> AdaTypes.t -> unit

(* TODO: try to minimize interface *)
(*WG**)
val replace_type : t -> string ->  AdaTypes.t -> unit

val replace_typ_enum : 
  t -> (string * AdaTypes.data_t) -> AdaTypes.t -> AdaTypes.t -> unit


(* TODO: remove optional arguments *)
val add_variable : t -> string -> Newspeak.location
                     -> ?value:AdaTypes.data_t
                     -> ?no_storage:bool
                     -> ?ro:bool
                     -> AdaTypes.t
                       -> unit

val add_subprogram : t
                -> string
                -> AdaSyntax.param list
                -> AdaTypes.t option
      -> unit

val add_use : bool -> t -> string -> unit

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

(* Add a package to the "with" list. 
val add_with          : t -> string -> unit
Is a package in the "with" list ? 
val is_with           : t -> string -> bool
*)
val is_ada_pck :  string -> bool

val s_get_use         : t -> string list

val add_renaming_decl : 
  t -> string option * string -> (AdaSyntax.param list) option 
    -> string option * string -> unit

val make_name_of_lval: AdaSyntax.lval -> string list
