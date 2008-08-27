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
module S = Ptrspeak

let translate (globdecs, fundecs, _) = 
  let vars = Hashtbl.create 100 in
  let funs = Hashtbl.create 100 in
  let prog = ref [] in

  let vcnt = ref 0 in
  let stack = ref [] in
  let current_fun = ref "" in

  let push_local x =
    let v = !current_fun^"."^(string_of_int !vcnt) in
      Hashtbl.add vars v (!current_fun^"."^x);
      stack := v::!stack;
      incr vcnt
  in

  let pop_local () =
    match !stack with
	_::tl -> stack := tl
      | _ -> 
	  invalid_arg ("Npkpointer.translate.pop_local: "
		       ^"unexpected empty variable stack")
  in
    
  let get_local x = List.nth !stack x in

  let translate_ftyp (args, ret) =
    match ret with
	None -> args
      | Some t -> t::args
  in

  let translate_args ftyp =
    let params = translate_ftyp ftyp in
    let locals = ref !stack in
    let bind _ =
      match !locals with
	  x::tl -> 
	    locals := tl;
	    x
	| _ -> 
	    invalid_arg ("Npkpointer.translate.translate_ftyp: "
			 ^"unexpected empty variable stack")
    in
      List.map bind params
  in

  let rec translate_lval lv =
    match lv with
	Global x -> S.Var x
      | Local x -> S.Var (get_local x)
      | Deref (e, _) -> S.Deref (translate_exp e)
      | Shift (lv, _) -> translate_lval lv
	    
  and translate_exp e =
    match e with
	Const _ -> S.Const
      | Lval (lv, _) -> S.Deref (translate_lval lv)
      | AddrOf (lv, _) -> translate_lval lv
      | _ -> 
	  invalid_arg "Npkpointer.translate_exp: expression not implemented yet"
  in

  let translate_fn fn =
    match fn with
	FunId fid -> 
	  let (ftyp, _) = Hashtbl.find fundecs fid in
	    (S.Var fid, ftyp)
      | FunDeref (e, ftyp) -> (translate_exp e, ftyp)
  in
    
  let rec translate_stmt (x, _) =
    match x with
	Set (lv, e, _) ->
	  let e1 = translate_lval lv in
	  let e2 = translate_exp e in
	    prog := (S.Set (e1, e2))::!prog
      | Copy (lv1, lv2, _) -> 
	  let e1 = translate_lval lv1 in
	  let e2 = S.Deref (translate_lval lv2) in
	    prog := (S.Set (e1, e2))::!prog
      | Decl (x, _, body) ->
	  push_local x;
	  translate_blk body;
	  pop_local ()
      | ChooseAssert choices -> List.iter translate_choice choices
      | InfLoop body -> translate_blk body
      | DoWith (body, _, action) -> 
	  translate_blk body;
	  translate_blk action
      | Goto _ -> ()
      | Call fn ->
	  let (e, ftyp) = translate_fn fn in
	  let params = translate_args ftyp in
	    prog := (S.Call (e, params))::!prog

  and translate_choice (_, body) = translate_blk body

  and translate_blk x = List.iter translate_stmt x in

  let translate_init_list x init =
    let rec translate init =
      match init with
	  [] -> ()
	| (_, _, e)::tl -> 
	    let e = translate_exp e in
	      prog := (S.Set (S.Var x, e))::!prog;
	      translate tl
    in
      translate init
  in

  let translate_global x (_, init) =
    Hashtbl.add vars x x;
    match init with
	Zero -> ()
      | Init init_list -> translate_init_list x init_list
  in

  let translate_fundec fid (ftyp, body) =
    current_fun := fid;
    vcnt := 0;
    let formals = translate_ftyp ftyp in
    let push_local _ = 
      let x = ("formal"^(string_of_int !vcnt)) in
	push_local x
    in
      List.iter push_local formals;
      Hashtbl.add funs fid !stack;
      translate_blk body;
      List.iter (fun _ -> pop_local ()) formals
  in

    Hashtbl.iter translate_global globdecs;
    Hashtbl.iter translate_fundec fundecs;
    (vars, (funs, !prog))
