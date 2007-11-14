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

type prog = (global * location) list

and global =
    | FunctionDef of (declaration * blk)
(* true for extern *)
    | GlbDecl of (bool * declaration * init option)
    | Typedef of declaration

and declaration = (base_typ * var_modifier)

and init = 
    | Data of exp
    | Sequence of init list
    | CstStr of string

and base_typ =
    | Void 
    | Integer of (sign_t * ityp)    
    | Struct of declaration list
    | Union of declaration list
    | Name of string

and var_modifier =
    | Abstract
    | Variable of string
    | Function of (var_modifier * declaration list)
    | Array of (var_modifier * Int64.t option)
    | Pointer of var_modifier

and ityp = 
    | Char 
    | Short
    | Int
    | Long
    | LongLong

and stmt = (stmtkind * location)

and blk = stmt list

and stmtkind =
    | Decl of (declaration * init option)
    | Set of (lv * exp)
    | If of (exp * blk * location) list
    | Switch of (exp * (exp option * blk * location) list)
    | While of (exp * blk)
    | DoWhile of (blk * exp)
    | Return of exp option
    | Exp of exp
    | Break
    | Block of blk

and field = string

and lv = 
    | Var of string
    | Field of (lv * string)
    | Index of (lv * exp)
    | Deref of exp

and exp = 
    | Const of cst
    | Lval of lv
    | AddrOf of lv
    | Unop of (unop * exp)
    | And of (exp * exp)
    | Binop of (binop * exp * exp)
    | Call of (string * exp list)
    | Sizeof of declaration
    | SizeofV of string

and cst = Int64.t

and unop = Not

and binop =
    | Plus
    | Minus
    | Mult
    | Gt
    | Eq

val size_of_ityp: ityp -> int

val negate: exp -> exp
