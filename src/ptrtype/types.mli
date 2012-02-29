(*
 * ptrtype: do finer typechecks on C pointers
 * Copyright (C) 2012 Etienne Millon
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * Etienne Millon <etienne.millon@eads.net>
 * EADS Innovation Works - SE/IT
 * 12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
 *)

type unknown = { id : int }

type var_type =
  | Unknown of unknown
  | Instanciated of simple

and simple =
  | Int
  | Float
  | Fun of simple list * simple list
  | Ptr of simple
  | Array of simple
  | Struct of (int * simple) list
  | Var of var_type ref

type variable =
  | VGlobal of string
  | VLocal of string
  | VFun of Newspeak.fid

val string_of_variable : variable -> string

val type_eq : simple -> simple -> bool

val string_of_simple : simple -> string

val shorten : simple -> simple

val vars_of_typ : simple -> int list

val extract_fun_type : simple -> simple list * simple list
