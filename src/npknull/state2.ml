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
open PtrSpeak

type t = {
  store: Store2.t;
}
      
let universe () = 
  {
    store = Store2.universe ();
  }

let init globals =
  {
    store = Store2.init globals;
  }

let rec translate_exp e =
  match e with
      Empty -> GraphExp.Empty
    | LocalVar x -> GraphExp.Var x
    | GlobalVar x -> GraphExp.Var x
    | Access e -> GraphExp.Deref (translate_exp e)
    | Join (e1, e2) ->
	let p1 = translate_exp e1 in
	let p2 = translate_exp e2 in
	  GraphExp.Join (p1, p2)

let translate_formula f =
  match f with
      AreNotEqual (e1, e2) -> 
	GraphExp.AreNotEqual (translate_exp e1, translate_exp e2)

let assign lv e state =
  let lv_p = translate_exp lv in
  let e_p = translate_exp e in
  let store = Store2.assign lv_p e_p state.store in
(* TODO: have VarAccess3 implement an assign too on a different syntax! *)
    { 
      store = store;
    }

let join state1 state2 =
  {
    store = Store2.join state1.store state2.store;
  }

let is_subset state1 state2 = Store2.is_subset state1.store state2.store

let guard _ state = state

let remove_variables x state = 
  { 
    store = Store2.remove_variables x state.store;
  }

let split root_variables state =
  let (_reachable_variables, store, unreachable_store) = 
    Store2.split root_variables state.store 
  in
    ({ store = store; }, 
     { store = unreachable_store; })

let size_of state = Store2.size_of state.store

let print state = Store2.print state.store

let compose _ _ = invalid_arg "State2.compose: Not implement because probably not needed, think about it"

let substitute subst state = 
  { store = Store2.substitute subst state.store }

let list_nodes state = Store2.list_nodes state.store

let restrict nodes state =
  { store = Store2.restrict nodes state.store
  }

let normalize roots state =
  let (store, subst) = Store2.normalize roots state.store in
  (* TODO: maybe should remove variables from access too? *)
    ({ store = store }, subst)

(* TODO: try to simplify this primitive, there are too many things going 
   on here:
   - unification
   - substitution
   - union
*)
let transport roots state input = Store2.transport roots state.store input.store

let glue state1 state2 =
  { store = Store2.glue state1.store state2.store
  }

let implies state e = Store2.implies state.store (translate_formula e)
