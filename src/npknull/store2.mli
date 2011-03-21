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

module type Transport =
sig
  type t

  val identity: unit -> t
  val associate: string -> string -> t -> t
  val apply: t -> string -> VarSet.t
  val apply_set: t -> VarSet.t -> VarSet.t
end

module Make(Subst: Transport):
sig
  type t
    
  val universe: unit -> t
    
  val assign: GraphExp.t -> GraphExp.t -> t -> t
    
  val eval_exp: t -> GraphExp.t -> VarSet.t
    
  (* TODO: why is this needed? *)
  val eval_pathSet: t -> VarSet.t -> VarSet.t
    
  val init: string list -> t
    
  val join: t -> t -> t
    
  val is_subset: t -> t -> bool
    
  val remove_variables: string list -> t -> t
    
  val print: t -> unit
    
  val size_of: t -> int
    
  val split: string list -> t -> (string list * t * t)
    
  val substitute: Subst.t -> t -> t
    
  val transport: string list -> t -> t -> Subst.t
    
  val glue: t -> t -> t
    
  val normalize: string list -> t -> (t * Subst.t)
    
  val list_nodes: t -> VarSet.t
    
  val restrict: VarSet.t -> t -> t
    
  val satisfies: t -> GraphExp.formula -> bool
end

val test: unit -> unit
