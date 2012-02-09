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

type ('a, 'b) t

val empty : ('a, 'b) t

val get : ('a, 'b) t -> 'a -> 'b
val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

val add_lbl : Newspeak.lbl -> ('a, 'b) t -> ('a, 'b) t
val assert_lbl : Newspeak.lbl -> ('a, 'b) t -> unit

val add_fun : ('a, 'b) t -> Newspeak.fid -> 'b -> ('a, 'b) t
val get_fun : ('a, 'b) t -> Newspeak.fid -> 'b
