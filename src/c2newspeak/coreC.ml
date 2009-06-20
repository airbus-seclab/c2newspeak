(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2009  Charles Hymans
  
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

module Nat = Newspeak.Nat

(* TODO: have hashtables rather *)
type t = (string * glbinfo) list * (string * funinfo) list * assertion list

and glbinfo = (decl * Newspeak.location)

and funinfo = (ftyp * bool * blk * Newspeak.location)

and assertion = spec_token list

and spec_token = 
  | SymbolToken of char
  | IdentToken of string
  | CstToken of Cir.cst
      
and decl = 
    VDecl of (typ * is_static * is_extern * init option)
  | EDecl of exp
(* struct or union: composite *)
  | CDecl of (is_struct * field_decl list)
  
(* true for structure, false for union *)
and is_struct = bool

and is_extern = bool

and is_static = bool

and field_decl = (string * typ)

and ftyp = (typ * string) list option * typ

and typ =
  | Void
  | Int of Newspeak.ikind
  | Bitfield of (Newspeak.ikind * exp)
  | Float of int
  | Ptr of typ
  | Array of array_typ
(* true for structure *)
  | Comp of (string * bool)
  | Fun of ftyp
  | Va_arg
  | Typeof of string
     
and array_typ = typ * exp option
 
and init = 
  | Data of exp
  | Sequence of (string option * init) list

and stmt = (stmtkind * Newspeak.location)

and blk = stmt list

and stmtkind =
    LocalDecl of (string * decl)
  | If of (exp * blk * blk)
  | CSwitch of (exp * (exp * blk * Newspeak.location) list * blk)
      (* init, while exp is true do blk and then blk, 
	 continue jumps before the second blk 
	 init may cotain break or continue stmt!
      *)
  | For of (blk * exp * blk * blk)
  | DoWhile of (blk * exp)
  | Exp of exp
  | Break
  | Continue
  | Return of exp option
  | Block of blk
  | Goto of lbl
  | Label of lbl
  | UserSpec of assertion

and lbl = string

and static = bool

and exp = 
    | Cst of (Cir.cst * typ)
    | Var of string
    | Field of (exp * string)
    | Index of (exp * array_typ * exp)
    | Deref of exp
    | AddrOf of exp
    | Unop of (unop * exp)
    | IfExp of (exp * exp * exp * typ)
    | Binop of ((binop * typ) * typ_exp * typ_exp)
    | Call of (funexp * ftyp * exp list)
    | Sizeof of typ
    | Offsetof of (typ * string)
    | Str of string
    | FunName
    | Cast of (exp * typ * typ)
(* None is a regular assignment *)
    | Set of (exp * (binop * typ) option * exp)
(* boolean is true if the operation is appled after the evaluation of the 
   expression *)
    | OpExp of ((binop * typ) * exp * bool)
    | BlkExp of (blk * bool)

and funexp =
    Fname of string
  | FunDeref of exp

and typ_exp = (exp * typ)

and unop = Not | BNot of Newspeak.ikind

and binop =
  | Plus
  | Minus
  | Mult
  | Div
  | Mod
  | Gt
  | Eq
  | BAnd
  | BXor
  | BOr
  | Shiftl
  | Shiftr

let char_kind = (Newspeak.Signed, Config.size_of_char)

let char_typ = Int char_kind

let uint_typ = Int (Newspeak.Unsigned, Config.size_of_int)

let int_kind = (Newspeak.Signed, Config.size_of_int)

let int_typ = Int int_kind

let exp_of_char c = Cst (Cir.CInt (Nat.of_int (Char.code c)), char_typ)

let exp_of_int i = Cst (Cir.CInt (Nat.of_int i), int_typ)

let comp_of_typ t =
  match t with
      Comp (n, _)-> n
    | _ -> 
	Npkcontext.report_error "Csyntax.comp_of_typ" 
	  "struct or union type expected"

let ftyp_of_typ t = 
  match t with
      Fun ft -> ft
    | _ -> Npkcontext.report_error "CoreC.ftyp_of_typ" "function type expected"

let deref_typ t =
  match t with
      Ptr t -> t
    | _ -> Npkcontext.report_error "CoreC.deref_typ" "pointer type expected"

let min_ftyp (args_t1, ret_t1) (args_t2, ret_t2) =  
  let equals (t1, _) (t2, _) = t1 = t2 in
(* TODO???
  let equals (t1, _) (t2, _) =  
    match (t1, t2) with  
      | (Ptr Fun _, Ptr Fun _) -> true  
      | (Ptr _, Ptr _) -> true  
      | _ -> t1 = t2  
  in  
    *)
  let args_t =  
    match (args_t1, args_t2) with  
        (None, args_t) | (args_t, None) -> args_t  
      | (Some args_t1, Some args_t2) ->
	  let eq = 
	    try List.for_all2 equals args_t1 args_t2 
	    with Invalid_argument _ -> false
	  in
            if not eq then begin
              Npkcontext.report_error "Csyntax.min_ftyp"
		"different argument types for function"
            end;
            Some args_t1
  in
    if (ret_t1 <> ret_t2) then begin
      Npkcontext.report_error "Csyntax.min_ftyp" 
	"different return types for function"
    end;
    (args_t, ret_t1)  

let rec string_of_exp e =
  match e with
      Cst (Cir.CInt c, _) -> Newspeak.Nat.to_string c
    | Cst _ -> "Cst"
    | Var x -> x
    | Field (e, f) -> (string_of_exp e)^"."^f
    | Index (e1, _, e2) -> 
	"("^(string_of_exp e1)^")["^(string_of_exp e2)^"]"
    | Deref e -> "*("^(string_of_exp e)^")"
    | AddrOf e -> "&("^(string_of_exp e)^")"
    | Unop (_, e) -> "op("^(string_of_exp e)^")"
    | IfExp (e1, e2, e3, _) -> 
	let e1 = string_of_exp e1 in
	let e2 = string_of_exp e2 in
	let e3 = string_of_exp e3 in
	  "("^e1^") ? ("^e2^") : ("^e3^")"
    | Binop (_, (e1, _), (e2, _)) -> 
	(string_of_exp e1) ^" op "^(string_of_exp e2)
    | Call _ -> "Call"
    | Offsetof _ -> "Offsetof"
    | Sizeof _ -> "Sizeof"
    | Str _ -> "Str"
    | FunName -> "FunName"
    | Cast (e, _, _) -> 
	let e = string_of_exp e in
	  "(typ) "^e
    | Set (lv, None, e) -> (string_of_exp lv)^" = "^(string_of_exp e)^";"
    | Set _ -> "Set"
    | OpExp _ -> "OpExp"
    | BlkExp _ -> "BlkExp"

let rec string_of_typ t =
  match t with
    | Void -> "Void"
    | Int (sign, sz) -> 
	let sign =
	  match sign with
	      Newspeak.Signed -> ""
	    | Newspeak.Unsigned -> "u"
	in
	  sign^"int"^(string_of_int sz)
    | Bitfield _ -> "Bitfield"
    | Float _ -> "Float"
    | Ptr _ -> "Ptr"
    | Array _ -> "Array"
    | Comp _ -> "Comp"
    | Fun ft -> string_of_ftyp ft
    | Va_arg -> "Va_arg"
    | Typeof _ -> "Typeof"

and string_of_ftyp (args_t, ret_t) =
  let args_t = 
    match args_t with
	None -> ""
      | Some l -> string_of_args_t l
  in
  let ret_t = string_of_typ ret_t in
    args_t^" -> "^ret_t

and string_of_args_t x =
  match x with
      (t, _)::[] -> string_of_typ t
    | (t, _)::tl -> (string_of_typ t)^", "^(string_of_args_t tl)
    | [] -> "void"
	
let promote k = 
  match k with
      (_, n) when n < Config.size_of_int -> int_kind
    | _ -> k
