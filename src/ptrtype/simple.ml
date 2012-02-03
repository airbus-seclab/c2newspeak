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

type var_type =
  | Unknown of int
  | Instanciated of simple

and simple =
  | Int
  | Ptr of simple
  | Var of var_type ref

let rec string_of_simple = function
  | Int -> "Int"
  | Ptr s -> "Ptr (" ^ string_of_simple s ^ ")"
  | Var {contents = Unknown n} -> "_a"^string_of_int n
  | Var {contents = Instanciated s} -> "@"^string_of_simple s^"@"

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
  type ('a, 'b) t

  val empty : ('a, 'b) t

  val get : ('a, 'b) t -> 'a -> 'b
  val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

  val add_lbl : Newspeak.lbl -> ('a, 'b) t -> ('a, 'b) t
  val has_lbl : Newspeak.lbl -> ('a, 'b) t -> bool
end = struct

  type ('a, 'b) t =
    { lvals : ('a * 'b) list
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


(************
 * Checking *
 ************)

let const_compatible cst ty =
  match (cst, ty) with
  | (N.CInt _, Int) -> true
  | (N.Nil, Ptr _) -> true
  | _ -> false

let binop_compatible ret op a b =
  match (ret, op, a, b) with
  | (Int, (N.PlusI|N.MinusI
          |N.MultI|N.DivI|N.Mod), Int, Int) -> true
  | (Int, N.Eq st, _, _) -> (a = b) && scalar_compatible st a
  | (Int, N.Gt st, Int, Int) -> scalar_compatible st Int
  | (Int, (N.BOr _|N.BAnd _|N.BXor _|N.Shiftlt|N.Shiftrt), Int, Int) -> true
  | (Ptr tr, N.PlusPI, Ptr ta, Int) -> tr = ta
  | (Int, N.MinusPP, Ptr ta, Ptr tb) -> ta = tb
  | _ -> false

let unop_compatible ret op e =
  match (ret, op, e) with
  | (_, (N.Belongs _|N.Coerce _|N.Focus _), _) -> ret = e
  | (Int, (N.Not|N.BNot _), Int) -> true
  | (Int, N.PtrToInt _, Ptr _) -> true
  | (Ptr _, N.IntToPtr _, Int) -> true
  | (_, N.Cast (sret, se), _) ->
      (scalar_compatible sret ret && scalar_compatible se e)
  | _ -> false

let rec check_exp env (be, te) =
  (*
   * General idea :
   *   - check subexps (generic)
   *   - check result (specific)
   *)
  match be with
  | T.Const c -> tc_assert (const_compatible c te)
  | T.Lval (lv, ty) ->
      let t_lv = Env.get env lv in
      same_type t_lv ty;
      same_type te ty;
  | T.AddrOf lv ->
      let t_lv = Env.get env lv in
      same_type te (Ptr t_lv)
  | T.BinOp (op, a, b) ->
      let (_, ta) = a in
      let (_, tb) = b in
      check_exp env a;
      check_exp env b;
      tc_assert (binop_compatible te op ta tb)
  | T.UnOp (op, e) ->
      let (_, top) = e in
      check_exp env e;
      tc_assert (unop_compatible te op top)
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

let blocks_in tpk =
  let fun_blocks =
    List.map
      (fun fdec -> fdec.T.body)
      (Utils.hashtbl_values tpk.T.fundecs)
  in
  tpk.T.init::fun_blocks

let check tpk =
  List.iter
    (check_blk Env.empty)
    (blocks_in tpk)

(*************
 * Inference *
 *************)

let (new_unknown, reset_unknowns) =
  let c = ref 0 in
  let n () =
    let v = !c in
    incr c;
    Unknown v
  and r () =
    c := 0
  in
  (n, r)

let rec vars_of_typ = function
  | Int -> []
  | Ptr t -> vars_of_typ t
  | Var ({contents = Unknown n}) -> [n]
  | Var ({contents = Instanciated t}) -> vars_of_typ t

let no_vars_in t =
  vars_of_typ t = []

let infer_const = function
  | N.CInt _ -> Int
  | N.CFloat _ -> assert false
  | N.Nil ->
      let t = new_unknown () in
      Ptr (Var (ref t))

let infer_unop op (_e, t) =
  match (op, t) with
  | (N.Belongs _, _) -> t
  | (N.Coerce _, _) -> t
  | (N.Focus _, _) -> t
  | (N.Not, Int) -> Int
  | (N.BNot _, Int) -> Int

  | _ -> assert false

let infer_binop op a b =
  match (op, a, b) with
  | _ -> assert false

let rec infer_lv env = function
  | T.Local s -> T.Local s
  | T.Global s -> T.Global s
  | T.Deref (e, sz) -> T.Deref (infer_exp env e, sz)
  | T.Shift (lv, e) -> T.Shift (infer_lv env lv, infer_exp env e)

and infer_exp env (e, _) =
  match e with
  | T.Const c -> (T.Const c, infer_const c)
  | T.Lval (lv, _ty) ->
      let lv' = infer_lv env lv in
      let ty = Env.get env lv in
      (T.Lval (lv', ty), ty)
  | T.AddrOf lv ->
      let lv' = infer_lv env lv in
      let ty = Env.get env lv in
      (T.AddrOf lv', Ptr ty)
  | T.UnOp (op, e) ->
      let e' = infer_exp env e in
      (T.UnOp (op, e'), infer_unop op e')
  | T.BinOp (op, a, b) ->
      let a' = infer_exp env a in
      let b' = infer_exp env b in
      (T.BinOp (op, a', b'), infer_binop op a' b')
  | T.AddrOfFun _ -> assert false

let rec infer_stmtkind env sk =
  match sk with
  | T.Set (lv, e, st) ->
      let lv' = infer_lv env lv in
      let e' = infer_exp env e in
      T.Set (lv', e', st)
  | T.Select (a, b) ->
      let a' = infer_blk env a in
      let b' = infer_blk env b in
      T.Select (a', b')
  | T.InfLoop blk ->
      let blk' = infer_blk env blk in
      T.InfLoop blk'
  | T.DoWith (blk, lbl) ->
      let new_env = Env.add_lbl lbl env in
      let blk' = infer_blk new_env blk in
      T.DoWith (blk', lbl)
  | T.Goto lbl ->
      tc_assert (Env.has_lbl lbl env);
      T.Goto lbl

  | _ -> assert false

and infer_stmt env (sk, loc) =
  let sk' = infer_stmtkind env sk in
  (sk', loc)

and infer_blk env =
  List.map (infer_stmt env)

let infer_fdec env fdec =
  (*
   * Prepare a new environment with rets & args.
   * Infer the body under it.
   * Retrieve the values from the typing environment.
   * (generalization should be done here)
   *)
  let new_env =
    List.fold_left
      (fun e (name, _ty) ->
        Env.add
          (T.Local name)
          (Var (ref (new_unknown ())))
          e
      )
      env
      (fdec.T.rets@fdec.T.args)
  in
  let blk = infer_blk new_env fdec.T.body in
  let extract_types l =
    List.map
      (fun (name, _ty) ->
        let t = Env.get new_env (T.Local name) in
        (name, t)
      )
      l
  in
  { T.body = blk
  ; T.rets = extract_types fdec.T.rets
  ; T.args = extract_types fdec.T.args
  ; T.position = fdec.T.position
  }

let infer tpk =
  let env = Env.empty in
  reset_unknowns ();

  let init = infer_blk env tpk.T.init in
  let fdecs = Utils.hashtbl_map (infer_fdec env) tpk.T.fundecs in

  let global_names = Utils.hashtbl_keys tpk.T.globals in
  let globals = Hashtbl.create 0 in
  List.iter
    (fun g ->
      let t = Env.get env (T.Global g) in
      tc_assert (no_vars_in t);
      Hashtbl.add globals g t
    )
    global_names;
  { T.globals = globals
  ; T.init = init
  ; T.fundecs = fdecs
  ; T.ptr_sz = tpk.T.ptr_sz
  ; T.src_lang = tpk.T.src_lang
  ; T.abi = tpk.T.abi
  }

