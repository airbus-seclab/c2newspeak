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

type address = 
    VariableStart of string
  | Variables of VarSet.t

val meet_address: address -> address -> address

module PtrSyntax:
sig
  type exp = 
      Empty
    | Var of string
    | Deref of exp
    | Join of (exp * exp)
    | InfDeref of exp
	
  type formula = exp * exp
end

module ValueSyntax:
sig
  (* true if not null *)
  type exp = 
      NotNull
    | Lval of address
    | Unknown

  val string_of_lval: address -> string
end

module type PtrStore = functor(Subst: Transport.T) ->
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

  val assign: (PtrSyntax.exp * PtrSyntax.exp) -> t -> t
  val satisfies: t -> PtrSyntax.formula -> bool
  val split: string list -> t -> (string list * t * t)

  val eval_exp: t -> PtrSyntax.exp -> address

  val transport: string list -> t -> t -> Subst.t
  val normalize: string list -> t -> (t * Subst.t)
  val list_nodes: t -> VarSet.t
end

module type ValueStore = functor (Subst: Transport.T) -> 
sig
  type t
  val universe: unit -> t
  val join: t -> t -> t
  val is_subset: t -> t -> bool
  val remove_variables: string list -> t -> t
  val substitute: Subst.t -> t -> t
  val glue: t -> t -> t
  val restrict:  VarSet.t -> t -> t
  val print: t -> unit

  val assign: (address * ValueSyntax.exp) -> t -> t
  val satisfies: t -> address -> bool
  val split: string list -> t -> (t * t)
end

module Make(PtrStore: PtrStore)(ValueStore: ValueStore): State2Bottom.State
