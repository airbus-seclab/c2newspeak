(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans
  
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
*)

type exp = 
    Empty
  | Cst of Newspeak.Nat.t
  | Var of string
  | Access of exp
  | Shift of (exp * exp)
  | Join of (exp * exp)

type stmt = 
  | Set of (exp * exp)
  | Guard of exp
  | Select of (blk * blk)
  | InfLoop of blk
  | DoWith of (blk * int)
  | Goto of int
  | Call of (exp list * string * exp list)

and blk = (stmt * Newspeak.location) list

type formula = 
  | AreNotEqual of (exp * exp)
  | IsNotNull of exp

val translate_lval: Newspeak.lval -> exp

val translate_exp: Newspeak.exp -> exp

val to_string: exp -> string

val test: unit -> unit
