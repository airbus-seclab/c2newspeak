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

open Newspeak
module F = Equations

let build prog =
  let globals = ref [] in
  let fundecs = Hashtbl.create 100 in
    
  let translate_global x _ = globals := x::!globals in

  let rec translate_lval x =
    match x with
	Local x -> F.Local x
      | Global x -> F.Global x
      | Deref (e, _) -> F.Deref (translate_exp e)
      | _ -> 
	  invalid_arg ("Factory.translate_lval: not implemented yet: "
		       ^(Newspeak.string_of_lval x))
  
  and translate_exp x = 
    match x with
	Const _ -> F.Const
      | Lval (lv, _) -> translate_lval lv
      | UnOp (_, e) -> translate_exp e
      | _ -> 
	  invalid_arg ("Factory.translate_exp: not implemented yet: "
		       ^(Newspeak.string_of_exp x))
  in

  let translate_fn x =
    match x with
	FunId f -> f
      | _ -> invalid_arg "Factory.translate_fn: not implemented yet"
  in

  let rec translate_blk x =
    match x with
	hd::tl -> 
	  let hd = translate_stmt hd in
	  let tl = translate_blk tl in
	    hd@tl
      | [] -> []

  and translate_stmt (x, loc) = 
    match x with
	Set (lv, e, _) ->
	  let lv = translate_lval lv in
	  let e = F.Deref (translate_exp e) in
	    (F.Set (lv, e), loc)::[]
(*
      | Copy (lv1, lv2, _) -> 
*)
      | Guard _ -> []
      | Decl (_, _, body) -> (F.Decl (translate_blk body), loc)::[]
      | Select (br1, br2) -> 
	  let br1 = translate_blk br1 in
	  let br2 = translate_blk br2 in
	    (F.Select (br1, br2), loc)::[]
(*
      | InfLoop _
      | DoWith _
      | Goto _
*)
      | Call f -> (F.Call (translate_fn f), loc)::[]
      | _ -> 
	  invalid_arg ("Factory.translate_stmt: statement not handled yet: "
		       ^(Newspeak.string_of_stmt (x, loc)))
  in

  let translate_fundec f (_, body) =
    let body = translate_blk body in
      Hashtbl.add fundecs f body
  in

  let build_entry () =
    let main_tainted = "!main_tainted!" in
    let tainted_exp = F.Global main_tainted in
    let loc = Newspeak.unknown_loc in
    let ((args, ret), _) = Hashtbl.find prog.fundecs "main" in
    let call = ref ((F.Call "main", Newspeak.unknown_loc)::[]) in
    let rec append_args args =
      match args with
	  _::tl -> 
	    let taint = (F.Taint (F.Local 0), loc) in
	    let set = (F.Set (F.Local 0, tainted_exp), loc) in
	      call := (F.Decl (taint::set::(!call)), loc)::[];
	      append_args tl
	| [] -> ()
    in
      globals := main_tainted::!globals;
      append_args args;
      call := (F.Set (tainted_exp, tainted_exp), loc)::!call;
      call := (F.Taint tainted_exp, loc)::!call;
      match ret with
	  Some _ -> (F.Decl (!call), loc)::[]
	| None -> !call
  in

  let entry = build_entry () in
    Hashtbl.iter translate_global prog.globals;
    Hashtbl.iter translate_fundec prog.fundecs;
    (!globals, fundecs, entry)
