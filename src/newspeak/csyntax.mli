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

(* first None is for extern, second None for no init *)
and glbdecls = (string, typ * location * init option option * const) Hashtbl.t

and const = bool
 
and fundefs = (string, ftyp * location * body option) Hashtbl.t

and body = (typ * string * location) list * blk

and init = (int * typ * typ_exp) list

and ftyp = (typ * string) list * typ

and typ =
    | Void
    | Int of ikind
    | Float of int
    | Ptr of typ
    | Array of array_t
    | Struct of (fields_t * int)
    | Union of (fields_t * int)
    | Fun of ftyp

and fields_t = (string * (int * typ)) list

and array_t = (typ * int option)

and blk = stmt list

and stmt = (stmtkind * location)

and stmtkind =
    | Init of (int * init)
    | Set of (typ_lv * typ_exp)
    | If of (typ_exp * blk * blk)
    | Switch of (exp * (typ_exp option * blk * location) list)
(* TODO: incorrect translation, have a loop *)
    | Loop of blk
    | Return
    | Call of (typ_lv option * fn * typ_exp list)
    | Break

and typ_exp = (exp * typ)

and typ_lv = (lv * typ)

and lv = 
    | Local of int
    | Global of string
    | Field of (lv * string * int)
    | Index of (lv * array_t * exp)
    | Deref of (exp * int)

and exp = 
    | Const of cst
    | Lval of typ_lv
    | AddrOf of typ_lv
    | Unop of (unop * exp)
    | Binop of (binop * exp * exp)

and fn = string * ftyp

and unop = 
    | Not

and binop =
    | Plus of ikind 
    | Minus of ikind
    | Mult of ikind
    | PlusP of typ
    | Gt of typ
    | Eq of typ

and cst = Int64.t

val ftyp_of_typ: typ -> ftyp

val fields_of_typ: typ -> fields_t

val array_of_typ: typ -> array_t

val deref_typ: typ -> typ

val size_of: typ -> int

val int_typ: typ

val typ_of_cst: cst -> typ

val typ_of_unop: unop -> typ

val typ_of_binop: binop -> typ

val promote: ikind -> ikind

val exp_of_int: int -> exp

val ftyp_equals: ftyp -> ftyp -> bool

val undefined: string
