(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007-2011  Charles Hymans, Sarah Zennou
  
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

  Sarah Zennou
  email: sarah(dot)zennou(at)eads(dot)net
*)

(** Simple language.
    All variables have type int.
*)

module type T =
sig 
  type t = {
    globals: vid list;                     (** program variables *)
    init: blk;                            (** initialization block of globals *)
    fundecs: (fid, fundec) Hashtbl.t;     (** table of all declared functions *)
    src_lang: Newspeak.src_lang;          (** source programming language *)
  }
     
  and fundec = blk
      
  and blk = stmt list
      
  and stmt = stmtkind * Newspeak.location
      
  and stmtkind =
      Set of (lval * exp)                 (** assignment *)
    | If of (exp * blk * blk)             (** if then else *)
    | While of (exp * blk)                (** while loop *)
    | Call of funexp                      (** function call *)
    | Assert of assertion                 (** assertion *)

  and funexp = FunId of fid
    
  and lval = Global of vid                (** global variable *)
    
  and exp =
      Const of cst                        (** integer constant *)
    | Lval of lval                        (** left value *)
    | Random of (integer * integer)       (** random value *)
    | UnOp of (unop * exp)                (** unary operation *)
    | BinOp of (binop * exp * exp)        (** binary operation *)
	
  and cst = CInt of integer
    
  and unop = Not                          (** negation *)
      
  and binop = 
      PlusI                               (** addition *)
    | MinusI                              (** substraction *)
    | MultI                               (** multiplication *)
    | DivI                                (** division *)
    | Mod                                 (** modulo *)
    | Gt                                  (** strictly greater than *)
    | Eq                                  (** equality *)
	
  and bounds = integer * integer
      
  and assertion = (lval * cmp * cst)      (** x == c
					      x <= c *)
  and cmp =
      Equals
    | IsLess
	
  and vid = string
      
  and fid = string
      
  and integer = Int32.t
      
  val to_string: t -> string
    
  val string_of_unop: unop -> string
    
  val string_of_binop: binop -> string
    
  val string_of_loc: Newspeak.location -> string
    
  val string_of_lval: lval -> string
    
  val string_of_exp: exp -> string
    
  val string_of_stmtkind: stmtkind -> string
    
  val string_of_stmt: stmt -> string
    
  val string_of_blk: blk -> string

end

include T
