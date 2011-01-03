(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007-2010  Charles Hymans, Etienne Millon, Sarah Zennou
  
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

  Charles Hymans
  EADS Innovation Works - SE/IS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org

  Etienne Millon
  EADS Innovation Works - SE/IS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: etienne.millon@eads.net
  
  Sarah Zennou
  EADS Innovation Works - SE/IS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: sarah.zennou@eads.net
  
*)

(**
  * A type for variable names that do not appear in the original source code.
  * For example, variables needed to store temporary values created by the
  * evaluation of C expressions with side effects.
  *)
type t =
  | Cstr of string          (** String litterals : filename and contents *)
  | Return                  (** Return value (inside function) *)
  | Value_of of string      (** Return value (inside caller) *)
  | Misc of string          (** Generic temporary variable *)
  | Goto_label of string    (** Boolean used to replace a 'goto' statement *)
  | Argv                    (** Parameter of the main function whose type is _char **_ *)
  | Argv_value              (** String of one the cells of the main parameter of type _char **_ *)
  | Arg                     (** Function argument *)
  | Ada_operator of string  (** Ada operator. See ada2newspeak/ada_utils *)

(**
  * The first parameter is a unique integer provided by the caller, added to
  * the returned string.
  *)
val to_string : int -> t -> string

(**
  * Name of return value (from the callee).
  * The same as `to_string (_ Return)`
  *)
val return_value : string

(**
  * "Inspect" functions.
  * Given a variable name, what category does it belong to ?
  *)

(**
  * Special variables.
  * Ie, variables which do not appear in the C program.
  *
  *   is_special (Temps.to_string _ _) = true
  *)
val is_special : string -> bool

(**
  * String litterals.
  * Returns `true` for a special string litteral variable, `false` otherwise.
  *
  *   is_string_litteral (Temps.to_string _ (Cstr (_, s))) = Some s
  *)
val is_string_litteral : string -> bool

(**
  * Return values, inside the function.
  *
  *   is_return_value (Temps.to_string _ Return) = true
  *)
val is_return_value : string -> bool

(**
  * Return values, in the caller.
  * Returns `Some function_name` for a return value, `None` otherwise.
  *
  *   is_value_of (Temps.to_string _ (Value_of s)) = Some s
  *)
val is_value_of : string -> string option

(**
  * Generic values.
  * Each generic value has a domain. Returns `Some domain` for those variables,
  * `None` otherwise.
  *
  *   is_generic_temp (Temps.to_string _ (Misc s)) = Some s
  *)
val is_generic_temp : string -> string option

(**
  * Goto labels created during goto elimination pass.
  * Return `Some label` for those variables, `None` otherwise.
  *
  *   is_goto_label (Temps.to_string _ (Goto_label s)) = Some s
  *)
val is_goto_label : string -> string option

(**
  * Ada overloaded operators.
  * Return `Some op_name` for those variables, `None` otherwise.
  *
  *   is_ada_operator (Temps.to_string _ (Ada_operator op)) = Some op
  *)
val is_ada_operator : string -> string option
