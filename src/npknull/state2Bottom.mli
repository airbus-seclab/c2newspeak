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
  val join: t -> t -> t
  val assign: PtrSpeak.exp -> PtrSpeak.exp -> t -> t
  val guard: PtrSpeak.exp -> t -> t
  val compose: t -> t -> t
  val is_subset: t -> t -> bool
  val substitute: Subst.t -> t -> t
  val remove_variables: string list -> t -> t
  val split: string list -> t -> (t * t)
  val print: t -> unit
  val transport: string list -> t -> t -> Subst.t
  val glue: t -> t -> t
  val normalize: string list -> t -> (t * Subst.t)
  val list_nodes: t -> VarSet.t
  val restrict: VarSet.t -> t -> t
  val satisfies: t -> PtrSpeak.formula -> bool
end

module Make(State: State): Modular.State
