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

(* None for extern *)
and glbdecls = (string, typ * location * init option option) Hashtbl.t

and fundefs = 
    (string, ftyp * location * ((decl * location) list * blk) option) Hashtbl.t

and decl = (typ * string)

and init = (int * typ * exp) list

and ftyp = decl list * typ

and typ =
    | Void
    | Int of ikind
    | Float of int
    | Ptr of typ
    | Array of array_t
    | StructOrUnion of (bool * fields_t * int) (* true for Struct *)
    | Fun of ftyp

and fields_t = (string * (int * typ)) list

and array_t = (typ * int option)

and blk = stmt list

and localdecl = (decl * location)

and stmt = (stmtkind * location)

and stmtkind =
    | Init of (string * init)
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
    | SizeofV of string

and unop = 
    | Not

and binop =
    | Plus
    | Mult
    | Gt
    | Eq

and cst = Int64.t

let ftyp_of_typ t =
  match t with
      Fun x -> x
    | _ -> Npkcontext.error "Csyntax.fun_of_typ" "Function type expected"

let fields_of_typ t =
  match t with
      StructOrUnion (_, f, _) -> f
    | _ -> 
	Npkcontext.error "Csyntax.fields_of_typ" 
	  "Struct or union type expected"

let array_of_typ t =
  match t with
      Array a -> a
    | _ -> Npkcontext.error "Csyntax.array_of_typ" "Array type expected"

let deref_typ t =
  match t with
      Ptr t -> t
    | _ -> Npkcontext.error "Csyntax.deref_typ" "Pointer type expected"

let rec size_of t =
  match t with
      Int (_, n) -> n 
    | Float n -> n
    | Ptr _ -> Config.size_of_ptr
    | Array (t, Some n) -> (size_of t) * n
    | StructOrUnion (_, _, n) -> n
    | Fun _ -> Npkcontext.error "Csyntax.size_of" "unknown size of function"
    | Array _ -> Npkcontext.error "Csyntax.size_of" "unknown size of array"
    | Void -> Npkcontext.error "Csyntax.size_of" "unknown size of void"

let undefined = "!undefined"
