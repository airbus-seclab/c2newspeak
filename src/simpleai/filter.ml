(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans
  
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

module Nat = Newspeak.Nat
module S = Simple

let is_int t =
  match t with
      Int (Signed, 32) -> true
    | _ -> false

let process_const c =
  match c with
      CInt n -> 
	if not (Newspeak.belongs n (Newspeak.domain_of_typ (Signed, 32))) 
	then begin
	  invalid_arg ("Filter.process_const: "
		       ^"integer not representable as a 32 bits integer")
	end;
	S.CInt (Int32.of_string (Nat.to_string n))
    | _ -> invalid_arg "Filter.process_const: integer constant expected"
    
let rec process_lval lv =
  match lv with
      Global x -> S.Global x
    | _ -> invalid_arg "Filter.process_lval: not implemented yet"
	
and process_exp e =
  match e with
      Const c -> S.Const (process_const c)
    | _ -> invalid_arg "Filter.process_exp: not implemented yet"
  
let rec process_blk x = List.map process_stmt x 
  
and process_stmt (x, loc) = 
  Context.set_loc loc;
  (process_stmtkind x, loc)
    
and process_stmtkind x =
  match x with
      Set (lv, e, t) -> 
	if not (is_int t) then begin
	  invalid_arg ("Filter.process_stmtkind: "
		       ^"integer expected at assignment")
	end;
	S.Set (process_lval lv, process_exp e)
    | _ -> invalid_arg "Filter.process_stmtkind: not implemented yet"
	
	
let process prog = 
  let globals = Hashtbl.create 100 in
  let fundecs = Hashtbl.create 100 in
    
  let init = process_blk prog.init in
    {
      S.fnames = prog.fnames;
      S.globals = globals;
      S.init = init;
      S.fundecs = fundecs;
      S.src_lang = prog.src_lang;
    }
