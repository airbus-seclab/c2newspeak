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
  | Set     of lval * exp
  | Guard   of exp
  | Select  of blk * blk
  | InfLoop of blk
  | DoWith  of blk * lbl * blk
  | Goto    of lbl
  | Decl    of blk
  | Assert  of exp

and lval =
  | G     of string
  | L     of int
  | Shift of lval * exp

and var =
  | Local  of int
  | Global of string

and typ =
  | Int
  | Array of int

and exp =
  | Const  of cst
  | Lval   of lval * typ
  | Not    of exp
  | Op     of binop * exp * exp
  | AddrOf of lval
  | Belongs of bounds * Newspeak.location * exp

and bounds = int * int

and lbl = int

and cst =
  | CInt of int
  | Nil

and binop =
  | Plus
  | Minus
  | Mult
  | Div
  | Gt
  | Eq
  | PlusPtr of Newspeak.location

and annotation =
  | Widening
  | Domain of string
