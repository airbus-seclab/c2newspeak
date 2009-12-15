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
 *)

(** @author Etienne Millon <etienne.millon@eads.net> *)

val compile : Newspeak.t -> Prog.t * Prog.annotation list * (string * int) list

val size_of_typ : Prog.typ -> int

val to_var : ?env:(Prog.lval -> 'a) -> 'a Domain.c_dom -> Prog.lval -> Prog.var

val from_var : Prog.var -> Prog.lval

module Print : sig
  val stmtk : Prog.stmtkind -> string
  val lval  : Prog.lval     -> string
  val exp   : Prog.exp      -> string
  val var   : Prog.var      -> string
end
