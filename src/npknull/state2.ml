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

type address = 
    VariableStart of string
  | Variables of VarSet.t

(* TODO: could be more precise if necessary *)
let meet_address a1 a2 =
  match (a1, a2) with
      ((VariableStart _ as a), _) | (_, (VariableStart _ as a)) -> a
    | _ -> a1

module PtrSyntax =
struct
  type exp = 
      Empty
    | Var of (string * bool)
    | Deref of exp
    | Join of (exp * exp)

  type formula = (exp * exp)

  let rec string_of_exp e = 
    match e with
	Empty -> "{}"
      | Var (x, is_zero) -> 
	  let offset = if is_zero then "0" else "?" in
	    "("^x^", "^offset^")"
      | Deref e -> "*("^string_of_exp e^")"
      | Join (e1, e2) -> "("^string_of_exp e1^"|"^string_of_exp e2^")"
end

module ValueSyntax =
struct
  type exp = 
      NotNull
    | Lval of address
    | Unknown

  let variables_of_lval lv =
    match lv with
	VariableStart x -> VarSet.singleton x
      | Variables set -> set

  let join_lval lv1 lv2 =
    if (lv1 = lv2) then lv1
    else begin
      let set1 = variables_of_lval lv1 in
      let set2 = variables_of_lval lv2 in
	Variables (VarSet.union set1 set2)
    end

  let string_of_lval x = 
    match x with
	VariableStart x -> "("^x^", 0)"
      | Variables x -> VarSet.to_string x

  let string_of_exp e =
    match e with
	NotNull -> "!= 0"
      | Lval lv -> string_of_lval lv
      | Unknown -> "?"
end

module type PtrStore = functor(Subst: Transport.T) ->
sig
  type t
  val universe: unit -> t
  val join: t -> t -> t
  val is_subset: t -> t -> bool
(* TODO: simplify: should only have remove_variable? 
   rather than remove_variables? think about it *)
  val remove_variables: string list -> t -> t
  val substitute: Subst.t -> t -> t
  val glue: t -> t -> t
  val restrict: VarSet.t -> t -> t
  val print: t -> unit

  val assign: (PtrSyntax.exp * PtrSyntax.exp) -> t -> t
  val satisfies: t -> PtrSyntax.formula -> bool
  val split: string list -> t -> (string list * t * t)

  val eval_exp: t -> PtrSyntax.exp -> address

  val transport: string list -> t -> t -> Subst.t
  val normalize: string list -> t -> (t * Subst.t)
  val list_nodes: t -> VarSet.t
end

module type ValueStore = functor(Subst: Transport.T) -> 
sig
  type t
  val universe: unit -> t
  val join: t -> t -> t
  val is_subset: t -> t -> bool
  (* TODO: prefer only VarSets rather than string lists *)
  val remove_variables: string list -> t -> t
  val substitute: Subst.t -> t -> t
  val glue: t -> t -> t
  val restrict: VarSet.t -> t -> t
  val print: t -> unit

  val assign: (address * ValueSyntax.exp) -> t -> t
  val satisfies: t -> address -> bool
  val split: string list -> t -> (t * t)
end

module Make(Store2Make: PtrStore)(ValueStoreMake: ValueStore)
  (Subst: Transport.T) =
struct
  module ValueStore = ValueStoreMake(Subst)
  module Store2 = Store2Make(Subst)

  let rec translate_exp e =
    match e with
	Empty -> PtrSyntax.Empty
      | LocalVar x -> PtrSyntax.Var (x, true)
      | GlobalVar x -> PtrSyntax.Var (x, true)
      | Access e -> PtrSyntax.Deref (translate_exp e)
      | Shift e -> 
	  let e = translate_exp e in begin
	      match e with
		  PtrSyntax.Var (x, _) -> PtrSyntax.Var (x, false)
		| PtrSyntax.Deref _ | PtrSyntax.Empty -> e
		| _ -> 
		    invalid_arg ("State2.translate_exp: not implemented yet"
				 ^PtrSyntax.string_of_exp e)
	    end
      | Join (e1, e2) ->
	  let p1 = translate_exp e1 in
	  let p2 = translate_exp e2 in
	    PtrSyntax.Join (p1, p2)
	      
  let deref store e = Store2.eval_exp store (translate_exp e)
      
  let lval_to_list store e =
    let rec lval_to_list e =
      match e with
	  LocalVar x | GlobalVar x -> VarSet.singleton x
	| Shift e -> lval_to_list e
	    (* TODO: should be better to return a VarSet *)
	| Join (e1, e2) -> 
	    VarSet.union (lval_to_list e1) (lval_to_list e2)
	| Empty -> invalid_arg "State2.lval_to_list: case not implemented yet"
	| Access _ -> ValueSyntax.variables_of_lval (deref store e)
    in
      lval_to_list e

  let lval_to_value store lv =
    let rec lval_to_value lv =
      match lv with
	  (* TODO: this case should not be possible => strange, try to 
	     remove by having a distinction between lval and exp in ptrSpeak? *)
	  Empty -> invalid_arg "should be unreachable"
	| LocalVar x -> VariableStart x
	| GlobalVar x -> VariableStart x
	| Shift lv -> Variables (lval_to_list store lv)
	| Access _ -> deref store lv
	| Join (lv1, lv2) -> 
	    ValueSyntax.join_lval (lval_to_value lv1) (lval_to_value lv2)
    in
      lval_to_value lv

  let rec exp_to_value store e = 
    let rec exp_to_value e =
      match e with
	  LocalVar _ | GlobalVar _ -> ValueSyntax.NotNull
	| Access lv -> ValueSyntax.Lval (lval_to_value store lv)
	| Shift e -> exp_to_value e 
	| Empty | Join _ -> ValueSyntax.Unknown
    in
      exp_to_value e

  (* TODO: try to simplify/factor with other functions *)
  let exp_to_lval_value store lv =
    let rec exp_to_lval_value lv =
      match lv with
	  (* TODO: this case should not be possible => strange, try to 
	     remove by having a distinction between lval and exp in ptrSpeak? *)
	  Empty -> invalid_arg "should be unreachable"
	| LocalVar x -> VariableStart x
	| GlobalVar x -> VariableStart x
	| Shift e -> exp_to_lval_value e
	| Access _ -> deref store lv
	| Join (e1, e2) -> 
	    ValueSyntax.join_lval (exp_to_lval_value e1) (exp_to_lval_value e2)
    in
      exp_to_lval_value lv

  type t = {
    store: Store2.t;
    value: ValueStore.t;
  }
	
  let universe () = 
    {
      store = Store2.universe ();
      value = ValueStore.universe ()
    }
      
  let assign lv e state =
    let lv_p = translate_exp lv in
    let e_p = translate_exp e in
    let store = Store2.assign (lv_p, e_p) state.store in
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

  let print state = 
    Store2.print state.store;
    ValueStore.print state.value

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
	  let e1 = translate_exp e1 in
	  let e2 = translate_exp e2 in
	    Store2.satisfies state.store (e1, e2)
      | IsNotNull e -> 
	  let e = exp_to_lval_value state.store e in
	    ValueStore.satisfies state.value e
end
