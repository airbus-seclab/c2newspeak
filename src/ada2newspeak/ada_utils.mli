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

(**
 * This type, and the log_progress function are used to log compilation
 * progress to STDERR. Lines with "###PROGRESS###" are meant to be parsed
 * by some external script.
 *)
type progress =
  | Parsing   of string (* Name of file                  *)
  | Semcheck  of string (* Name of compilation unit      *)
  | Translate of string (* Name of compilation unit      *)
  | Post                (* Everything after ada2newspeak *)
  | Done of progress    (* When something is done        *)

(**
 * Log progress, as explained in "progress".
 *)
val log_progress : progress -> unit

(**
 * Convert a boolean into a "native" integer.
 *)
val nat_of_bool : bool -> Newspeak.Nat.t

(**
 * Check a compilation unit against the name of the file around it.
 *)
val check_compil_unit_name :
  Syntax_ada.compilation_unit -> string -> bool

(**
 * Provides a default value.
 *   - [with_default  None    x] yields [x]
 *   - [with_default (Some v) x] yields [v]
 *)
val with_default : 'a option -> 'a -> 'a

(** Converts a qualified name into a dotted string.  *)
val name_to_string : Syntax_ada.name -> string

(** Create a function name for an overloaded operator *)
val make_operator_name : Syntax_ada.binary_op -> string

(**
 * Prepend a special string to an operator name.
 * Will raise an error if it is not a valid operator name.
 *)
val operator_of_string : string -> string

(**
 * Maybe apply some function to an option value :
 * may f (None) = None
 * may f (Some v) = Some (f v)
 *)
val may : ('a -> 'b) -> 'a option -> 'b option
