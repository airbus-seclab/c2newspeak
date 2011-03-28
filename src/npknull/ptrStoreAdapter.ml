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

module OffsetInsensitivePtrSyntax =
struct
  type exp = 
      Empty
    | Var of string
    | Deref of exp
    | Join of (exp * exp)
    | InfDeref of exp
	
  type formula = exp * exp

(* TODO: think about it, where to put this? in Make down there? *)
  let rec translate_exp e =
    match e with
	State2.PtrSyntax.Empty -> Empty
      | State2.PtrSyntax.Var (x, _) -> Var x
      | State2.PtrSyntax.Deref e -> Deref (translate_exp e)
      | State2.PtrSyntax.Join (e1, e2) -> 
	  Join (translate_exp e1, translate_exp e2)
(* TODO: remove InfDeref *)
      | State2.PtrSyntax.InfDeref e -> InfDeref (translate_exp e)

  let translate_formula (e1, e2) = (translate_exp e1, translate_exp e2)
end

module type OffsetInsensitivePtrStore = functor(Subst: Transport.T) ->
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

  val assign: 
    (OffsetInsensitivePtrSyntax.exp * OffsetInsensitivePtrSyntax.exp) -> t -> t
  val satisfies: t -> OffsetInsensitivePtrSyntax.formula -> bool
  val split: string list -> t -> (string list * t * t)

  val eval_exp: t -> OffsetInsensitivePtrSyntax.exp -> State2.address

  val transport: string list -> t -> t -> Subst.t
  val normalize: string list -> t -> (t * Subst.t)
  val list_nodes: t -> VarSet.t
end

module Make(StoreMake: OffsetInsensitivePtrStore)(Subst: Transport.T) =
struct
  module Store = StoreMake(Subst)

  include Store

  let assign (lv, e) store =
    let lv = OffsetInsensitivePtrSyntax.translate_exp lv in
    let e = OffsetInsensitivePtrSyntax.translate_exp e in
      Store.assign (lv, e) store

  let eval_exp store e =
    let e = OffsetInsensitivePtrSyntax.translate_exp e in
      Store.eval_exp store e

  let satisfies store formula = 
    let formula = OffsetInsensitivePtrSyntax.translate_formula formula in
      Store.satisfies store formula
end
