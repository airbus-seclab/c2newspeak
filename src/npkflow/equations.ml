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

type t = string list * (string, blk) Hashtbl.t * blk

and blk = stmt list

and stmt = stmtkind * Newspeak.location

and stmtkind =
    Set of (exp * exp)
  | Decl of blk
  | Select of (blk * blk)
  | InfLoop of blk
  | BlkLbl of blk
  | Goto of int
  | Call of exp
  | Display

and exp =
    Const
  | Local of int
  | Global of string
  | BinOp of (exp * exp)
  | Deref of exp

let rec string_of_exp x =
  match x with
      Const -> "cst"
    | Local x -> (string_of_int x)^"-"
    | Global _ -> "global"
    | BinOp _ -> "bop"
    | Deref e -> "*("^(string_of_exp e)^")"

let rec string_of_stmt (x, _) =
  match x with
      Set (lv, e) -> (string_of_exp lv)^" = "^(string_of_exp e)
    | Decl body -> "push;\n"^(string_of_blk body)^"pop;"
    | Select _ -> "Select"
    | InfLoop _ -> "while (1)"
    | BlkLbl _ -> "BlkLbl"
    | Goto _ -> "Goto"
    | Call _ -> "Call"
    | Display -> "Display"

and string_of_blk x = 
  match x with
      hd::tl -> 
	let hd = string_of_stmt hd in
	let tl = string_of_blk tl in
	  hd^"\n"^tl
    | [] -> ""

let string_of_fundec f body = f^" =\n"^(string_of_blk body)

let to_string (globals, fundecs, init) = 
  let res = ref "globals:\n" in
    List.iter (fun x -> res := !res^x^"\n") globals;
    res := !res^"function declarations\n";
    Hashtbl.iter (fun f x -> res := !res^(string_of_fundec f x)^"\n") fundecs;
    res := !res^"init\n";
    res := !res^(string_of_blk init);
    !res

