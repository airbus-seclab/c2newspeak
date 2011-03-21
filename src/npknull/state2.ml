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

module ValueSyntax =
struct
  type lval = 
      VariableStart of string
    | Variables of string list (* TODO: should be easier with a VarSet *)
	
  (* true if not null *)
  type exp = 
      NotNull
    | Lval of lval
    | Unknown
end

module type ValueStore =
sig
  type t
    
  val universe: unit -> t
(* true means is not null *)
  val assign: (ValueSyntax.lval * ValueSyntax.exp) -> t -> t
  val join: t -> t -> t
  val is_subset: t -> t -> bool
(* TODO: prefer only VarSets rather than string lists *)
  val remove_variables: string list -> t -> t
  val split: string list -> t -> (t * t)
  val substitute: Subst2.t -> t -> t
  val restrict: VarSet.t -> t -> t
  val glue: t -> t -> t
  val print: t -> unit
  val is_not_null: t -> ValueSyntax.lval -> bool
end

let rec translate_exp e =
  match e with
      Empty -> GraphExp.Empty
    | LocalVar x -> GraphExp.Var x
    | GlobalVar x -> GraphExp.Var x
    | Access e -> GraphExp.Deref (translate_exp e)
    | Shift e -> translate_exp e
    | Join (e1, e2) ->
	let p1 = translate_exp e1 in
	let p2 = translate_exp e2 in
	  GraphExp.Join (p1, p2)

let deref store e = 
  let variables = Store2.eval_exp store (translate_exp e) in
    VarSet.elements variables

let lval_to_list store e =
  let rec lval_to_list e =
    match e with
	LocalVar x | GlobalVar x -> x::[]
      | Access e -> deref store e
      | Shift e -> lval_to_list e
      | _ -> invalid_arg ("State2.lval_to_list: not implemented yet: "^PtrSpeak.to_string e)
  in
    lval_to_list e

let lval_to_value store lv =
  match lv with
      (* TODO: this case should not be possible *)
      Empty -> invalid_arg "should be unreachable"
    | LocalVar x -> ValueSyntax.VariableStart x
    | GlobalVar x -> ValueSyntax.VariableStart x
    | Shift e -> ValueSyntax.Variables (lval_to_list store e)
    | Access e -> ValueSyntax.Variables (deref store e)
    | Join (e1, e2) -> 
	let variables = (lval_to_list store e1)@(lval_to_list store e2) in
	  ValueSyntax.Variables variables

(* TODO: maybe dead code! *)
let rec exp_to_lvalue e =
  match e with
      LocalVar x -> ValueSyntax.VariableStart x
    | _ -> 
	invalid_arg ("State2.exp_to_lvalue: not implemented yet: "
		     ^PtrSpeak.to_string e)

let exp_to_value store e = 
  match e with
      LocalVar _ | GlobalVar _ -> ValueSyntax.NotNull
    | Access lv -> ValueSyntax.Lval (lval_to_value store lv)
    | Empty | Join _ | Shift _ -> ValueSyntax.Unknown

module Make(ValueStore: ValueStore) =
struct
  type t = {
    store: Store2.t;
    value: ValueStore.t;
  }
	
  let universe () = 
    {
      store = Store2.universe ();
      value = ValueStore.universe ()
    }
      
    (* TODO: remove? *)
  let init _globals = invalid_arg "Not necessary, remove code"
    (*
      {
      store = Store2.init globals;
      }
    *)
    
  let assign lv e state =
    let lv_p = translate_exp lv in
    let e_p = translate_exp e in
    let store = Store2.assign lv_p e_p state.store in
    let lv_value = lval_to_value state.store lv in
    let e_value = exp_to_value state.store e in
    let value = ValueStore.assign (lv_value, e_value) state.value in
      (* TODO: have VarAccess3 implement an assign too on a different syntax! *)
      { 
	store = store;
	value = value;
      }

  let join state1 state2 =
    {
      store = Store2.join state1.store state2.store;
      value = ValueStore.join state1.value state2.value
    }

  let is_subset state1 state2 = 
    (Store2.is_subset state1.store state2.store)
    && (ValueStore.is_subset state1.value state2.value)

  let guard _ state = state

  let remove_variables x state = 
    { 
      store = Store2.remove_variables x state.store;
      value = ValueStore.remove_variables x state.value
    }

  let split root_variables state =
    let (reachable_variables, store, unreachable_store) = 
      Store2.split root_variables state.store 
    in
    let (value, unreachable_value) =
      ValueStore.split reachable_variables state.value
    in
      ({ store = store; value = value }, 
       { store = unreachable_store; value = unreachable_value })

  let size_of state = Store2.size_of state.store

  let print state = 
    Store2.print state.store;
    ValueStore.print state.value

    (* TODO: remove? *)
  let compose _ _ = invalid_arg "State2.compose: Not implement because probably not needed, think about it"

  let substitute subst state = 
    { store = Store2.substitute subst state.store;
      value = ValueStore.substitute subst state.value }

      (* TODO: why is this method needed? *)
  let list_nodes state = Store2.list_nodes state.store

  let restrict nodes state =
    { store = Store2.restrict nodes state.store;
      value = ValueStore.restrict nodes state.value
    }

  let normalize roots state =
    let (store, subst) = Store2.normalize roots state.store in
      (* TODO: maybe should remove variables from access too? *)
    let value = ValueStore.substitute subst state.value in
      ({ store = store; value = value }, subst)

(* TODO: try to simplify this primitive, there are too many things going 
   on here:
   - unification
   - substitution
   - union
*)
  let transport roots state input = Store2.transport roots state.store input.store
    
  let glue state1 state2 =
    { store = Store2.glue state1.store state2.store;
      value = ValueStore.glue state1.value state2.value
    }
      
  let satisfies state e = 
    match e with
	AreNotEqual (e1, e2) -> 
	  let formula = 
	    GraphExp.AreNotEqual (translate_exp e1, translate_exp e2) 
	  in
	    Store2.satisfies state.store formula
      | IsNotNull e -> 
	  let e = lval_to_value state.store e in
	    ValueStore.is_not_null state.value e
end
