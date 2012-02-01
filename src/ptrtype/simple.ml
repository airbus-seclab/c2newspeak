(*
 * ptrtype: do finer typechecks on C pointers
 * Copyright (C) 2012 Etienne Millon
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * Etienne Millon <etienne.millon@eads.net>
 * EADS Innovation Works - SE/IT
 * 12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
 *)

module T = Tyspeak
module N = Newspeak

let hashtbl_values
  : (('a, 'b) Hashtbl.t -> 'b list)
  = fun h ->
    Hashtbl.fold (fun _k v l -> v::l) h []

type simple =
  | Int
  | Ptr of simple

let scalar_compatible st ty =
  match (st, ty) with
  | (N.Int _, Int) -> true
  | (N.Ptr, Ptr _) -> true
  | _ -> false

let tc_assert ok =
  if not ok then
    assert false

let same_type (x:simple) y =
  tc_assert (x = y)

module Env : sig
  type t

  val empty : t

  val get : t -> simple T.lval -> simple
  val add : simple T.lval -> simple -> t -> t

  val add_lbl : Newspeak.lbl -> t -> t
  val has_lbl : Newspeak.lbl -> t -> bool
end = struct

  type t =
    { lvals : (simple T.lval * simple) list
    ; lbls  : Newspeak.lbl list
    }

  let empty =
    { lvals = []
    ; lbls  = []
    }

  let get env k =
    List.assoc k env.lvals

  let add lv t env =
    { env with lvals = (lv, t)::env.lvals }

  let add_lbl lbl env =
    { env with lbls = lbl::env.lbls}

  let has_lbl lbl env =
    List.mem lbl env.lbls
end

let rec check_exp env (be, _te) =
  (*
   * General idea :
   *   - check subexps
   *   - check result <- TODO
   *)
  match be with
  | T.Const _ -> ()
  | T.Lval (lv, ty) ->
      let t_lv = Env.get env lv in
      same_type t_lv ty
  | T.AddrOf _ -> ()
  | T.BinOp (_op, a, b) ->
      check_exp env a;
      check_exp env b
  | T.UnOp (_op, e) ->
      check_exp env e
  | T.AddrOfFun (_fid, _ftyp) -> assert false

(*
 * Propagate typing constraints through control flow.
 * Besides the exp type in Guard, this is mostly generic.
 *)
let rec check_stmt env (sk, _loc) =
  match sk with
  | T.Set (lv, e, st) ->
      let (_be, te) = e in
      let t_lv = Env.get env lv in
      check_exp env e;
      tc_assert (scalar_compatible st te);
      same_type te t_lv;
      ()
  | T.Copy (dst, src, _sz) ->
      let tdst = Env.get env dst in
      let tsrc = Env.get env src in
      same_type tdst tsrc
  | T.Guard e ->
      let (_be, te) = e in
      check_exp env e;
      same_type te Int
  | T.Decl (vid, ty, blk) ->
      let new_env = Env.add (T.Local vid) ty env in
      check_blk new_env blk
  | T.Select (a, b) ->
      check_blk env a;
      check_blk env b
  | T.InfLoop blk ->
      check_blk env blk
  | T.DoWith (blk, lbl) ->
      let new_env = Env.add_lbl lbl env in
      check_blk new_env blk
  | T.Goto lbl ->
      tc_assert (Env.has_lbl lbl env)
  | T.Call (_args, _fe, _ret) ->
      assert false
  | T.UserSpec _ -> ()

and check_blk env =
  List.iter (check_stmt env)

let check tpk =
  let init_env = Env.empty in
  let fun_blocks =
    List.map
      (fun fdec -> fdec.T.body)
      (hashtbl_values tpk.T.fundecs)
  in
  let all_blocks = tpk.T.init::fun_blocks in
  List.iter (check_blk init_env) all_blocks
