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
(* TODO: remove Init *)
    | Init of (int * init)
    | Set of (typ_lv * typ_exp)
    | If of (typ_exp * blk * blk)
    | Switch of (exp * (typ_exp option * blk * location) list)
    | Loop of blk
    | Return
    | Call of (typ_lv option * (lv * ftyp) * typ_exp list)
    | Break
	
and typ_exp = (exp * typ)

and typ_lv = (lv * typ)

and lv = 
    | Local of int
    | Global of string
    | Field of (lv * string * int)
    | Index of (lv * array_t * exp)
    | Deref of (exp * typ)

(* Side-effect free expressions *)
and exp = 
    | Const of cst
    | Lval of typ_lv
    | AddrOf of typ_lv
    | Unop of (unop * exp)
    | Binop of (binop * exp * exp)

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

let ftyp_of_typ t =
  match t with
      Fun x -> x
    | _ -> Npkcontext.error "Csyntax.fun_of_typ" "Function type expected"

let fields_of_typ t =
  match t with
      Struct (f, _) | Union (f, _) -> f
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
    | Struct (_, n) | Union (_, n) -> n
    | Fun _ -> Npkcontext.error "Csyntax.size_of" "unknown size of function"
    | Array _ -> Npkcontext.error "Csyntax.size_of" "unknown size of array"
    | Void -> Npkcontext.error "Csyntax.size_of" "unknown size of void"

(* TODO: check that integer don't have a default type (like int) *)
let typ_of_cst i =
  let sign =
    if Int64.compare i (Int64.of_string "2147483647") > 0 
    then Unsigned else Signed
  in
    Int (sign, Config.size_of_int)

let int_typ = Int (Signed, Config.size_of_int)

let promote k = 
  match k with
      (_, n) when n < Config.size_of_int -> (Signed, Config.size_of_int)
    | _ -> k

let typ_of_unop op =
  match op with
      Not -> int_typ

let typ_of_binop op =
  match op with
      Mult k | Plus k | Minus k -> Int k
    | PlusP t -> Ptr t
    | Gt _ | Eq _ -> int_typ

let exp_of_int n = Const (Int64.of_int n)

let ftyp_equals (args1, ret1) (args2, ret2) =
  let (args1, _) = List.split args1 in
  let (args2, _) = List.split args2 in
    (args1, ret1) = (args2, ret2)

(* TODO: this is a bit of a hack. Avoid? *)
let undefined = "!undefined"
