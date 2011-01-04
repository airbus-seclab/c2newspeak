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

(* TODO: factor these functions into one function type_of_operator ?.*)
(**
 * This module is used to typecheck expressions. It will raise errors whenever
 * its functions are called with incompatible types or operations.
 *)

val type_of_binop : Ast.binary_op -> AdaTypes.t -> AdaTypes.t -> AdaTypes.t

(**
 * Preprocessed binary operations (those who do not
 * exist in the AST) need special functions.
 *)

val type_of_xor : AdaTypes.t -> AdaTypes.t -> AdaTypes.t

val type_of_abs : AdaTypes.t -> AdaTypes.t

val type_of_uplus : AdaTypes.t -> AdaTypes.t

val type_of_not : AdaTypes.t -> AdaTypes.t
