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

open Csyntax

let process_exp loc e =
  let rec process e =
    match e with
	Field (e, f) -> 
	  let (blk1, e, blk2) = process e in
	    (blk1, Field (e, f), blk2)

      | _ -> ([], e, [])
  in
  let (pre, e, post) = process e in
(* TODO: for post, this is truly a hack!!! removed blkExp ASAP *)
  let e = 
    if post = [] then e
    else BlkExp (post@(Exp e, loc)::[], true)
  in
  let e = 
    if pre = [] then e
    else BlkExp (pre@(Exp e, loc)::[], false)
  in
    e

let process_stmt_exp loc e =
  let rec process e =
    match e with
	Set (lv, op, IfExp (c, e1, e2)) -> 
	  process (IfExp (c, Set (lv, op, e1), Set (lv, op, e2)))
	    
      | IfExp (c, e1, e2) -> 
	  let c = process_exp loc c in
	  let e1 = process_exp loc e1 in
	  let e2 = process_exp loc e2 in
	    If (c, (Exp e1, loc)::[], (Exp e2, loc)::[])

      | _ -> Exp (process_exp loc e)
  in
    (process e, loc)

let process_stmt (x, loc) =
  match x with
      Exp e -> process_stmt_exp loc e
    | _ -> (x, loc)

let process_blk x = List.map process_stmt x

let process_global (x, loc) =
  let x = 
    match x with
	FunctionDef (f, ft, static, body) -> 
	  let body = process_blk body in
	    FunctionDef (f, ft, static, body)
      | _ -> x
  in
    (x, loc)

let process (globals, spec) =
  let globals = List.map process_global globals in
    (globals, spec)
