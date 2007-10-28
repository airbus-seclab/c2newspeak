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

type prog = (composites * glbdecls * fundefs)

and composites = (string, typ) Hashtbl.t

and glbdecls = (string, typ * location * exp option * bool) Hashtbl.t

and fundefs = (string, ftyp * location * blk) Hashtbl.t

and decl = (typ * string)

and ftyp = decl list * typ

and typ =
    | Void
    | Int of ikind
    | Float of int
    | Ptr of typ
    | Array of array_t
    | StructOrUnion of (bool * fields_t * int) (* true for a structure *)
    | Fun of ftyp

and fields_t = (string * (int * typ)) list

and array_t = (typ * int option)

and blk = stmt list

and stmt = (stmtkind * location)

and stmtkind =
    | Decl of (decl * blk)
    | Set of (lv * exp)
    | If of (exp * blk * location) list
    | Switch of (exp * (exp option * blk * location) list)
    | While of (exp * blk)
    | DoWhile of (blk * exp)
    | Return of exp
    | Exp of exp
    | Break

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
    | Binop of (binop * exp * exp)
    | Call of (string * exp list)

and unop = 
    | Not

and binop =
    | Plus
    | Mult
    | Gt
    | Eq

and cst = Int64.t

val ftyp_of_typ: typ -> ftyp

val fields_of_typ: typ -> fields_t

val array_of_typ: typ -> array_t

val deref_typ: typ -> typ

val size_of: typ -> int
