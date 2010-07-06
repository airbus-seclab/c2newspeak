(*
  This file is part of npksolver, a solver for Newspeak,
  a minimal language framework well suited for static analysis.

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

  Copyright 2009, 2010 Etienne Millon <etienne.millon@eads.net>

 *)

(** @author Etienne Millon <etienne.millon@eads.net> *)

(**
 * Program compiler.
 * Takes a Newspeak file, and outputs :
 *   - the program's text.
 *   - a list of annotations.
 *   - a list of global variables with their sizes.
 *)
val compile : Lowspeak.t -> Prog.t

(**
 * The size of a type.
 *)
val size_of_typ : Prog.typ -> int

(**
 * Pretty-printer for Prog types.
 *)
module Print : sig
  val stmtk : Prog.stmtkind -> string
  val lval  : Prog.lval     -> string
  val exp   : Prog.exp      -> string
  val addr  : Prog.addr     -> string
  val typ   : Prog.typ      -> string
end
