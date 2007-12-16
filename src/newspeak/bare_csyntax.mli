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
*)
open Newspeak

type prog = (Csyntax.compdefs * (global * location) list)

and global =
    | FunctionDef of (string * typ * blk)
(* true for extern, true for const *)
    | GlbDecl of (string * typ * bool * init option)

and typ = Csyntax.typ

and init = 
    | Data of exp
    | Sequence of init list

and stmt = (stmtkind * location)

and blk = stmt list

and stmtkind =
    | Decl of (string * typ * init option)
    | If of (exp * blk * blk)
    | Switch of (exp * (exp option * blk * location) list)
    | While of (exp * blk)
    | DoWhile of (blk * exp)
    | For of (blk * exp * blk * blk)
    | Return of exp option
    | Exp of exp
    | Break
    | Continue
    | Block of blk

and field = string

and exp = 
    | Cst of cst
    | Var of string
    | Field of (exp * string)
    | Index of (exp * exp)
    | Deref of exp
    | AddrOf of exp
    | Unop of (unop * exp)
    | And of (exp * exp)
    | Or of (exp * exp)
    | Binop of (binop * exp * exp)
    | Call of (exp * exp list)
    | Sizeof of Csyntax.typ
    | SizeofV of string
    | Str of string
    | Cast of (exp * typ)
    | Set of (exp * exp)
(* returns the value and then increment it *)
    | ExpPlusPlus of exp

and cst = Int64.t

and unop = Not

and binop =
    | Plus
    | Minus
    | Mult
    | Mod
    | Gt
    | Eq
    | BAnd

val negate: exp -> exp
