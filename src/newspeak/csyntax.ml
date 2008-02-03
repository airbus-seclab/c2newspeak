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

(* size and alignment *)
and compdefs = (string, (fields_t * int * int)) Hashtbl.t

(* None for extern *)
and glbdecls = (string, typ * location * init option option) Hashtbl.t
 
and fundefs = (string, ftyp * location * body option) Hashtbl.t

and body = (typ * string * location) list * blk

and init = (int * typ * exp) list

(* true if variable list of arguments *)
and ftyp = (typ * string) list * bool * typ

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
(* (init, body, suffix)
   continue jumps directly before the suffix *)
    | Loop of (blk * blk * blk)
    | Return
    | Call of (lv option * (lv * ftyp) * exp list)
    | Break
    | Continue
	
and typ_exp = (exp * typ)

and typ_lv = (lv * typ)

(* Side-effect free expressions *)
and lv = 
    | Local of int
    | Global of string
    | Field of (lv * int)
    | Index of (lv * array_t * exp)
    | Deref of (exp * typ)

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
    | Div of ikind
    | Mult of ikind
    | BAnd of ikind
    | BXor of ikind
    | BOr of ikind
    | Mod
    | PlusP of typ
    | MinusP
    | Gt of typ
    | Eq of typ
    | Shiftl of ikind
    | Shiftr of ikind
    | PlusF of int
    | MinusF of int
    | DivF of int
    | MultF of int

and cst = 
    | CInt of Int64.t
    | CFloat of string

let ftyp_of_typ t =
  match t with
      Fun x -> x
    | _ -> Npkcontext.error "Csyntax.fun_of_typ" "Function type expected"

let fields_of_typ compdefs t =
  match t with
      Struct n | Union n -> 
	let (f, _, _) = Hashtbl.find compdefs n in
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

let size_of compdefs t =
  let rec size_of t =
    match t with
	Int (_, n) -> n 
      | Float n -> n
      | Ptr _ -> Config.size_of_ptr
      | Array (t, Some n) -> (size_of t) * n
      | Struct n | Union n -> 
	  let (_, sz, _) = Hashtbl.find compdefs n in
	    sz
      | Fun _ -> Npkcontext.error "Csyntax.size_of" "Unknown size of function"
      | Array _ -> Npkcontext.error "Csyntax.size_of" "Unknown size of array"
      | Void -> Npkcontext.error "Csyntax.size_of" "Unknown size of void"
  in
    size_of t

let align_of compdefs t =
  let rec align_of t =
    match t with
	Struct n | Union n ->
	  let (_, _, a) = Hashtbl.find compdefs n in
	    a
      | Array (t, _) -> align_of t
      | _ -> size_of compdefs t
  in
    align_of t

(* TODO: check that integer don't have a default type (like int) *)
let typ_of_cst i =
  match i with
      CInt i ->
	let sign =
	  if Int64.compare i (Int64.of_string "2147483647") > 0 
	  then Unsigned else Signed
	in
	  Int (sign, Config.size_of_int)
    | CFloat v -> Float Config.size_of_double
	

let int_kind = (Signed, Config.size_of_int)

let int_typ = Int int_kind

let promote k = 
  match k with
      (_, n) when n < Config.size_of_int -> int_kind
    | _ -> k

let exp_of_int n = Const (CInt (Int64.of_int n))

let exp_of_float x = Const (CFloat (string_of_float x))

(* TODO: this is a bit of a hack. Avoid? *)
let undefined = "!undefined"

let cast (e, t) t' =
  let (e, t) =
    match (t, e, t') with
	(Array (elt_t, _), Lval (lv, Array a), (Ptr _|Int _)) ->
	  (AddrOf (Index (lv, a, exp_of_int 0), elt_t), Ptr elt_t)
      | (Fun _, Lval lv, (Ptr _|Int _)) -> (AddrOf lv, Ptr t)
      | _ -> (e, t)
  in
    if t = t' then e
    else Unop (Cast (t, t'), e)

let rec len_of_exp e =
  let i = 
    match e with
	Const (CInt i) -> i
      | Binop (Plus _, e1, e2) ->
	  let i1 = Int64.of_int (len_of_exp e1) in
	  let i2 = Int64.of_int (len_of_exp e2) in
	    Int64.add i1 i2
      | Binop (Minus _, e1, e2) ->
	  let i1 = Int64.of_int (len_of_exp e1) in
	  let i2 = Int64.of_int (len_of_exp e2) in
	    Int64.sub i1 i2
      | Binop (Mult _, e1, e2) ->
	  let i1 = Int64.of_int (len_of_exp e1) in
	  let i2 = Int64.of_int (len_of_exp e2) in
	    Int64.mul i1 i2
      | Binop (Div _, e1, e2) ->
	  let i1 = Int64.of_int (len_of_exp e1) in
	  let i2 = Int64.of_int (len_of_exp e2) in
	    Int64.div i1 i2
      | _ -> 
	  Npkcontext.error "Csyntaxt.len_of_exp" 
	    "static expression expected"
  in
    if ((Int64.compare i Int64.zero <= 0) 
	 || (Int64.compare i (Int64.of_int max_int) > 0)) then begin
      Npkcontext.error "Csyntax.len_of_exp" 
	("invalid size for array: "^(Int64.to_string i))
    end;
    Int64.to_int i

(* TODO: check this for various architecture ? 
   Here align everything on 4
   let align o sz = 
  let offset = o mod 4 in
    if offset = 0 then o
    else if offset + sz <= 4 then o
    else (o - offset) + 4
*)

(* [align o x] returns the smallest integer greater or equal than o,
   which is equal to 0 modulo x *)
let next_aligned o x =
  let m = o mod x in
    if m = 0 then o else o + x - m

let ftyp_equal (args1, va_list1, ret1) (args2, va_list2, ret2) =
  let (args1, _) = List.split args1 in
  let (args2, _) = List.split args2 in
    (args1, va_list1, ret1) = (args2, va_list2, ret2)
