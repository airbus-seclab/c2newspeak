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

module type State = functor (Subst: Transport.T) ->
sig
  type t
    
  val universe: unit -> t
  val init: string list -> t
  val join: t -> t -> t
  val assign: PtrSpeak.exp -> PtrSpeak.exp -> t -> t
  val guard: PtrSpeak.exp -> t -> t
  val compose: t -> t -> t
  val is_subset: t -> t -> bool
  val substitute: Subst.t -> t -> t
  val remove_variables: string list -> t -> t
  val split: string list -> t -> (t * t)
  val print: t -> unit
  val size_of: t -> int
  val transport: string list -> t -> t -> Subst.t
  val glue: t -> t -> t
  val normalize: string list -> t -> (t * Subst.t)
  val list_nodes: t -> VarSet.t
  val restrict: VarSet.t -> t -> t
  val satisfies: t -> PtrSpeak.formula -> bool
end
  
module Make(State: State)(Subst: Transport.T) =
struct
  module State = State(Subst)

  type t = State.t option
      
  let universe () = Some (State.universe ())
    
  let init v = Some (State.init v)
    
  let emptyset () = None
    
  let join s1 s2 = 
    match (s1, s2) with
	(None, s) | (s, None) -> s
      | (Some s1, Some s2) -> Some (State.join s1 s2)

  let assign lv e s =
    match s with
	None -> None
      | Some s -> Some (State.assign lv e s)
	
  let guard e s =
    match s with
	None -> None
      | Some s -> Some (State.guard e s)
	
  let compose s1 s2 =
    match (s1, s2) with
	(None, _) | (_, None) -> None
      | (Some s1, Some s2) -> Some (State.compose s1 s2)
	
  let is_subset s1 s2 =
    match (s1, s2) with
	(None, _) -> true
      | (_, None) -> false
      | (Some s1, Some s2) -> State.is_subset s1 s2
	
  let substitute subst s = 
    match s with
	None -> None
      | Some s -> Some (State.substitute subst s)

  let remove_variables x s =
    match s with
	None -> None
      | Some s -> Some (State.remove_variables x s)

  let print s =
    match s with
	None -> ()
      | Some s -> State.print s

  let size_of s =
    match s with
	None -> 0
      | Some s -> State.size_of s

(* TODO: try to avoid having to implement this function at the state with
   bottom level *)
  let transport roots state input =
    match (state, input) with
	(None, _) | (_, None) -> Subst.identity ()
      | (Some state, Some input) -> State.transport roots state input

  let normalize roots state =
    match state with
	None -> (None, Subst.identity ())
      | Some state -> 
	  let (state, subst) = State.normalize roots state in
	    (Some state, subst)

  let split root_variables s =
    match s with
	None -> (None, None) (* TODO: strange that it is necessary to code this
				should not have to consider bottom...
				think about how to do that in the fixpoin 
				algorithm, rather should use an exception
				for bottom and never have it as a value... *)
      | Some s -> 
	  let (state, unreachable) = State.split root_variables s in
	    (Some state, Some unreachable)

  let glue state1 state2 =
    match (state1, state2) with
	(None, _) | (_, None) -> None
      | (Some state1, Some state2) -> Some (State.glue state1 state2)

  let list_nodes state =
    match state with
	None -> VarSet.empty
      | Some state -> State.list_nodes state

  let restrict nodes state =
    match state with
	None -> None
      | Some state -> Some (State.restrict nodes state)

  let satisfies state e =
    match state with
	None -> true
      | Some state -> State.satisfies state e
end
