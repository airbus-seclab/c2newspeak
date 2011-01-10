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

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org

  Olivier Levillain
  email: olivier.levillain@penjili.org

  Etienne Millon
  email: etienne.millon AT gmail . com

  Jasmine Duchon
  email: jasmine . duchon AT free . fr
*)


(** Params contains fondamental parameters for Ada2Newspeak *)

(** {1 File extensions} *)

val ada_suffix : string
val ada_spec_suffix : string
val npko_suffix : string


(** {1 Version and other info } *)

val version_string : string
val comment_string : string

(** {1 ABI} *)

val ret_lbl : int
val brk_lbl : int
val ret_ident : string
