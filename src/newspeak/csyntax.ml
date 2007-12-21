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
  
type prog = (compdefs * glbdecls * fundefs)

and compdefs = (string, (fields_t * int)) Hashtbl.t

(* None for extern *)
and glbdecls = (string, typ * location * init option option) Hashtbl.t
 
and fundefs = (string, ftyp * location * body option) Hashtbl.t

and body = (typ * string * location) list * blk

and init = (int * typ * exp) list

and ftyp = (typ * string) list * typ

and typ =
    | Void
    | Int of ikind
    | Float of int
    | Ptr of typ
    | Array of array_t
    | Struct of string
    | Union of string
    | Fun of ftyp

and fields_t = (string * (int * typ)) list

and array_t = (typ * int option)

and blk = stmt list

and stmt = (stmtkind * location)

and stmtkind =
(* TODO: remove Init, replace by set!!! *)
    | Init of (int * init)
    | Set of (lv * typ * exp)
    | If of (exp * blk * blk)
    | Switch of (exp * (typ_exp option * blk * location) list)
(* body, suffix 
   continue jumps directly before the suffix *)
    | Loop of (blk * blk)
    | Return
    | Call of (lv option * (lv * ftyp) * exp list)
    | Break
    | Continue
	
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
    | BNot of ikind
    | Cast of (typ * typ)

and binop =
    | Plus of ikind
    | Minus of ikind
    | Mult of ikind
    | BAnd of ikind
    | BXor of ikind
    | BOr of ikind
    | Mod
    | PlusP of typ
    | Gt of typ
    | Eq of typ
    | Shiftl of ikind
    | Shiftr of ikind

and cst = Int64.t

let ftyp_of_typ t =
  match t with
      Fun x -> x
    | _ -> Npkcontext.error "Csyntax.fun_of_typ" "Function type expected"

let fields_of_typ compdefs t =
  match t with
      Struct n | Union n -> 
	let (f, _) = Hashtbl.find compdefs n in
	  f
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

let rec size_of compdefs t =
  let rec size_of t =
    match t with
	Int (_, n) -> n 
      | Float n -> n
      | Ptr _ -> Config.size_of_ptr
      | Array (t, Some n) -> (size_of t) * n
      | Struct n | Union n -> 
	  let (_, sz) = Hashtbl.find compdefs n in
	    sz
      | Fun _ -> Npkcontext.error "Csyntax.size_of" "Unknown size of function"
      | Array _ -> Npkcontext.error "Csyntax.size_of" "Unknown size of array"
      | Void -> Npkcontext.error "Csyntax.size_of" "Unknown size of void"
  in
    size_of t

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

let exp_of_int n = Const (Int64.of_int n)

let ftyp_equals (args1, ret1) (args2, ret2) =
  let (args1, _) = List.split args1 in
  let (args2, _) = List.split args2 in
    (args1, ret1) = (args2, ret2)

(* TODO: this is a bit of a hack. Avoid? *)
let undefined = "!undefined"

let cast (e, t) t' =
  let (e, t) =
    match (t, e, t') with
	(Array (elt_t, _), Lval (lv, Array a), Ptr _) ->
	  (AddrOf (Index (lv, a, exp_of_int 0), elt_t), Ptr elt_t)
      | (Fun _, Lval lv, Ptr _) -> (AddrOf lv, Ptr t)
      | _ -> (e, t)
  in
    if t = t' then e
    else Unop (Cast (t, t'), e)
