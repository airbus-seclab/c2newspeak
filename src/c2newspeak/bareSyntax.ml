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

type t = (global * Newspeak.location) list

(* TODO: cleanup, simplify types *)
and global = 
    FunctionDef of (bool * ((base_typ * var_modifier) * blk))
  | GlbDecl of ((bool * bool) * (base_typ * ((var_modifier * Newspeak.size_t list) * init option) list))
  | GlbTypedef of (base_typ * ((var_modifier * Newspeak.size_t list) * init option) list)
  | GlbUserSpec of Csyntax.assertion

and blk = stmt list

and stmt = (stmtkind * Newspeak.location)

and stmtkind =
    LocalDecl of ((bool * bool) * (base_typ * ((var_modifier * Newspeak.size_t list) * init option) list))
  | Exp of exp
  | Return
  | Block of blk
  | If of (exp * blk * blk)
      (* init, while exp is true do blk and then blk, 
	 continue jumps before the second blk 
	 init may cotain break or continue stmt!
      *)
  | For of (blk * exp * blk * blk)
  | DoWhile of (blk * exp)
  | CSwitch of (exp * (exp * blk * Newspeak.location) list * blk)
  | Break
  | Continue
  | Typedef of (base_typ * ((var_modifier * Newspeak.size_t list) * init option) list)
  | Label of Csyntax.lbl
  | Goto of Csyntax.lbl
  | UserSpec of Csyntax.assertion
(* TODO: cleanup expressions => factor more with binop ? *)
and exp =
    Cst of Csyntax.cst
  | Var of string
  | RetVar
  | Field of (exp * string)
  | Index of (exp * exp)
  | AddrOf of exp
  | Unop of (Csyntax.unop * exp)
  | IfExp of (exp * exp * exp)
  | Binop of (Csyntax.binop * exp * exp)
  | And of (exp * exp)
  | Or of (exp * exp)
  | Call of (exp * exp list)
  | Sizeof of (base_typ * var_modifier) (* TODO => introduce typ *)
  | SizeofE of exp
  | Offsetof of ((base_typ * var_modifier) * offset_exp)
  | Str of string
  | FunName
  | Cast of (exp * (base_typ * var_modifier))
      (* None is a regular assignment *)
  | Set of (exp * Csyntax.binop option * exp)
      (* boolean is true if the operation is applied after the evaluation of the 
	 expression *)
  | OpExp of (Csyntax.binop * exp * bool)
      (* block ended by and expression *)
  | BlkExp of blk

and aux_offset_exp =
    OffComp of string
  | OffField of aux_offset_exp * string

(* TODO: this type could probably be simplified?? *)
and offset_exp =
  | OIdent of string
  | OField of aux_offset_exp * string
  | OArray of aux_offset_exp * string * exp

and base_typ =
    | Void 
    | Integer of Newspeak.ikind
    | Float of int
    | Composite of (bool * (string * field list option))
    | Name of string
    | Enum of ((string * exp option) list) option
    | Va_arg
    | Typeof of string

and var_modifier = (int * modifier)

and modifier = 
    | Abstract
    | Variable of (string * Newspeak.location)
    | Function of (var_modifier * decl list)
    | Array of (var_modifier * exp option)

and decl = (base_typ * var_modifier)

and field = (base_typ * var_modifier * exp option)

and init = 
  | Data of exp
(* TODO: maybe remove the string option here ???? *)
  | Sequence of (string option * init) list

let int_typ = Csyntax.Int (Signed, Config.size_of_int)

let exp_of_int i = Cst (Cir.CInt (Nat.of_int i), int_typ)

let neg x = 
  match x with
      Cst (Cir.CInt c, Csyntax.Int (_, sz)) -> 
	Cst (Cir.CInt (Nat.neg c), Csyntax.Int (Signed, sz))
    | _ -> Binop (Csyntax.Minus, exp_of_int 0, x)
