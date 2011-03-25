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

module type SimplePtrStore = functor (Subst: Transport.T) ->
sig
  type t

  val universe: unit -> t
  val join: t -> t -> t
  val is_subset: t -> t -> bool
  val remove_variables: string list -> t -> t
  val substitute: Subst.t -> t -> t
  val glue: t -> t -> t
  val restrict: VarSet.t -> t -> t
  val print: t -> unit
  val assign: (State2.PtrSyntax.exp * State2.PtrSyntax.exp) -> t -> t
  val split: string list -> t -> (t * t)
(* None is for Top, think about this *)
  val eval_exp: t -> State2.PtrSyntax.exp -> State2.address option

end

module Make(Store1Make: State2.PtrStore)(Store2Make: SimplePtrStore)
(Subst: Transport.T) =
struct
  module Store1 = Store1Make(Subst)
  module Store2 = Store2Make(Subst)
    
  type t = {
    store1: Store1.t;
    store2: Store2.t
  }
      
  let apply f1 f2 state =
    { store1 = f1 state.store1;
      store2 = f2 state.store2 }

  let apply2 f1 f2 state1 state2 =
    { store1 = f1 state1.store1 state2.store1;
      store2 = f2 state1.store2 state2.store2 }

  let universe () = 
    { store1 = Store1.universe ();
      store2 = Store2.universe () }

  let join = apply2 Store1.join Store2.join
      
  let is_subset state1 state2 =
    (Store1.is_subset state1.store1 state2.store1)
    && (Store2.is_subset state1.store2 state2.store2)

  let remove_variables variables =
    apply (Store1.remove_variables variables) 
      (Store2.remove_variables variables)

  let substitute subst =
    apply (Store1.substitute subst) (Store2.substitute subst)

  let glue = apply2 Store1.glue Store2.glue

  let restrict variables = 
    apply (Store1.restrict variables) (Store2.restrict variables)

  let print state =
    Store1.print state.store1;
    Store2.print state.store2

  (* TODO: will do a simplifying translation here of languages *)
  let assign args = apply (Store1.assign args) (Store2.assign args)

  let split roots state = 
    let (reachable_variables, reachable_store1, unreachable_store1) =
      Store1.split roots state.store1
    in
    let (reachable_store2, unreachable_store2) =
      Store2.split reachable_variables state.store2
    in
      (reachable_variables,
       { store1 = reachable_store1; store2 = reachable_store2 }, 
       { store1 = unreachable_store1; store2 = unreachable_store2 })

  let eval_exp state e = 
    let address1 = Store1.eval_exp state.store1 e in
    let address2 = Store2.eval_exp state.store2 e in
   (* TODO: this Some/None, seems like a bit of a hack, think about it
   what about refine_address rather than eval_exp? *)
      match address2 with
	  (* TODO: should create a module Address *)
	  Some address2 -> State2.meet_address address1 address2
	| None -> address1

  let satisfies state e = Store1.satisfies state.store1 e

  let transport roots state1 state2 = 
    Store1.transport roots state1.store1 state2.store1

  let normalize roots state = 
    let (store1, subst) = Store1.normalize roots state.store1 in
    let store2 = Store2.substitute subst state.store2 in
      ({ store1 = store1; store2 = store2 }, subst)

  let list_nodes state = Store1.list_nodes state.store1
end
