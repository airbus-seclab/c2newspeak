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
(**
 * Stripped-down language :
 *   - only ints
 *   - only globals
 *   - no function calls
 *)

type t = blk

and blk = stmt list

and stmt = stmtkind * Newspeak.location

and stmtkind =
  | Set      of var * exp
  | Guard    of exp
  | Select   of blk * blk
  | InfLoop  of blk
  | DoWith   of blk * lbl * blk
  | Goto     of lbl
  | Decl     of blk

and var =
  | G of string
  | L of int

and exp =
  | Const of cst
  | Var   of var
  | Not   of exp
  | Op    of binop * exp * exp

and lbl = int

and cst = int

and binop =
  | Plus
  | Minus
  | Mult
  | Div
  | Gt
  | Eq
