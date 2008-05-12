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


(* implementation of copy propagation for newspeak *)
(* should do constant propagation *)
(* should do expression propagation *)

(*
get_global

get_local

env maps variable to top or an expression
*)
(*
  TODO: unused variables elimination
*)
(*
  TODO: Assumption: to add as a sanity checks:
  if x =(t) e; then x is declared with type t
*)

(*
  TODO: idea for change for newspeak:
  the number of the variable, should be the offset from the height of the
  stack AT THE CALL of current function !
*)

open Newspeak

let nb_of_params (args, ret) =
  let n = List.length args in
    match ret with
	None -> n
      | Some _ -> n + 1

let process_exp env e =
  let rec process env e =
  match e with
      Lval x -> Store.exp_of_local env x
    | UnOp (op, e) -> 
	let e = process env e in
	  UnOp (op, e)
    | BinOp (op, e1, e2) ->
	let e1 = process env e1 in
	let e2 = process env e2 in
	  BinOp (op, e1, e2)
    | _ -> e
  in
  let e = process env e in
    Normalize.process_exp e

let rec process_blk env x =
  match x with
      hd::tl -> 
	let (env, hd) = process_stmt env hd in
	let (env, tl) = process_blk env tl in
	  (env, hd::tl)
    | [] -> (env, [])
  
and process_stmt env (x, loc) =
  match x with
      Set (lv, e, t) ->
	let e = process_exp env e in
	let env = Store.assign env lv e in
	  (env, (Set (lv, e, t), loc))
    | Decl (v, t, body) -> 
	let env = Store.push env in
	let (env, body) = process_blk env body in
	let env = Store.pop env in
	  (env, (Decl (v, t, body), loc))
    | ChooseAssert choices ->
	let choices = List.map (process_choice env) choices in
	  (Store.forget env, (ChooseAssert choices, loc))
    | InfLoop body ->
	let env = Store.forget env in
	let (_, body) = process_blk env body in
	  (env, (InfLoop body, loc))
    | DoWith (body, l, []) ->
	let (_, body) = process_blk env body in
	  (Store.forget env, (DoWith (body, l, []), loc))
    | _ -> (Store.forget env, (x, loc))

and process_choice env (cond, body) =
  let cond = List.map (process_exp env) cond in
  let (_, body) = process_blk env body in
    (cond, body)

let process_fundec (t, body) =
  let body =
    let n = nb_of_params t in
    let env = Store.universe n in
    let (_, body) = process_blk env body in
      body
  in
    (t, body)

let process (gdecls, fundecs, specs) =
  let res = Hashtbl.create 100 in
  let process_fun fid fundec = 
    let fundec = process_fundec fundec in
      Hashtbl.add res fid fundec
  in
    Hashtbl.iter process_fun fundecs ;
    (gdecls, res, specs)
