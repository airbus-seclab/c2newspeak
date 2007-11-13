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
    | StructOrUnion of (bool * fields_t * int) (* true for Struct *)
    | Fun of ftyp

and fields_t = (string * (int * typ)) list

and array_t = (typ * int option)

and blk = stmt list

and stmt = (stmtkind * location)

and stmtkind =
(* TODO: remove Init *)
    | Init of ((int * typ) * init)
    | Set of (lv * exp)
    | If of (typ_exp * blk * location) list
    | Switch of (exp * (exp option * blk * location) list)
    | While of (typ_exp * blk)
    | DoWhile of (blk * typ_exp)
    | Return of exp
    | Exp of exp
    | Break
	
(* TODO: put it as a scalar ?? *)
and typ_exp = (exp * typ)

and lv = 
(* TODO: remove typ ? *)
    | Local of (int * typ)
(* TODO: remove typ ? *)
    | Global of (string * typ)
    | Field of (lv * string * int)
    | Index of (lv * exp)
    | Deref of (exp * int)

and exp = 
    | Const of cst
    | Lval of lv
(* TODO: remove typ ? *)
    | AddrOf of lv
    | Unop of (unop * exp)
    | Binop of (binop * exp * exp)
    | Call of (string * exp list)
    | Sizeof of exp

and unop = 
    | Not

and binop =
    | Plus
    | Minus
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

(* TODO: check that integer don't have a default type (like int) *)
let typ_of_cst i =
  let sign =
    if Int64.compare i (Int64.of_string "2147483647") > 0 
    then Unsigned else Signed
  in
    Int (sign, Config.size_of_int)

let int_typ = Int (Signed, Config.size_of_int)

let typ_of_unop op =
  match op with
      Not -> int_typ

let promote k = 
  match k with
      (_, n) when n < Config.size_of_int -> (Signed, Config.size_of_int)
    | _ -> k

let typ_of_binop op t1 t2 =
  match (op, t1, t2) with
      ((Mult|Plus|Minus), Int k1, Int k2) ->
	let k1 = promote k1 in
	let k2 = promote k2 in
	let k = max_ikind k1 k2 in
	  Int k
    | (Plus, Ptr _, Int _) -> t1
    | ((Gt|Eq), _, _) -> int_typ
    | _ ->
	Npkcontext.error "Csyntax.type_of_binop" 
	  "Unexpected binary operator and arguments"
