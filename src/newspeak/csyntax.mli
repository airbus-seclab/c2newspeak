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

type prog = global list

and global =
    | FunctionDef of (declaration * blk)
    | GlobalDecl of declaration

and declaration = (base_typ * var_modifier * location)

and base_typ =
    | Void 
    | Integer of (sign * ityp)    
    | Struct of declaration list
    | Union of declaration list

and var_modifier =
    | Variable of string
    | FunctionName of fname
    | Array of (var_modifier * Int64.t)
    | Pointer of var_modifier
    | FunctionProto of (var_modifier * base_typ list)

and ityp = 
    | Char 
    | Int

and sign =
    | Signed
    | Unsigned

and blk = declaration list * stmt list

and stmt = (stmtkind * location)

and stmtkind =
    | Set of (lv * exp)
    
and lv = 
    | Var of vname
    | Field of (lv * field)

and field = string

and exp = 
    | Const of cst
    | Lval of lv

and cst = Int64.t

and location = (file * line_nb * charac)

and file = string

and line_nb = int

and charac = int

and fname = string

and vname = string

