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
type t = Cstr of string * string (** String litterals : filename and string contents *)
       | Return                  (** Return value (inside function) *)
       | Value_of of string      (** Return value (inside caller) *)
       | Misc of string          (** Generic temporary variable *)
       | Goto_label of string    (** Boolean used to replace a 'goto' statement *)
       | Ada_operator of string  (** Ada operator. See ada2newspeak/ada_utils *)

(**
  * The first parameter is a unique integer provided by the caller, added to
  * the returned string.
  *)
val to_string : int -> t -> string

