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

open Types

(************
 * Checking *
 ************)

(*
 * Input : simple Tyspeak.t
 *
 * Output : unit
 *
 * This phase is quite straightforward : we walk through the program and check
 * if types are compatible through constructs. Every annotation is present, so
 * most functions are pure (they only read their environment).
 *)

let same_type x y =
  if not (type_eq x y) then
    begin
      let sx = string_of_simple x in
      let sy = string_of_simple y in
      failwith (Printf.sprintf "Typechecking failed : %s != %s\n" sx sy)
    end

let tc_scalar_compatible st lty =
  let ty = shorten lty in
  let error () =
    failwith
      ( "Scalar type "
      ^ N.string_of_scalar st
      ^ " is not compatible with "
      ^ string_of_simple ty
      )
  in
  match (st, ty) with
  | (N.Int _, Int) -> ()
  | (N.FunPtr, Ptr (Fun (_, _))) -> ()
  | (N.FunPtr, _) -> error ()
  | (N.Ptr, Ptr (Fun (_, _))) -> error ()
  | (N.Ptr, Ptr _) -> ()
  | (N.Float _, Float) -> ()
  | _ -> error ()

let tc_const_compatible cst ty =
  match (cst, ty) with
  | (N.CInt _, Int) -> ()
  | (N.Nil, Ptr _) -> ()
  | (N.CFloat _, Float) -> ()
  | _ ->
      failwith
        ( "Incompatibility between "
        ^ N.string_of_cst cst
        ^ " and "
        ^ string_of_simple ty
        )

let tc_binop_compatible ret op la lb =
  let a = shorten la in
  let b = shorten lb in
  match (ret, op, a, b) with
  | (Int, ( N.PlusI | N.MinusI
          | N.MultI | N.DivI
          | N.Mod | N.BOr _
          | N.BAnd _ | N.BXor _
          | N.Shiftlt | N.Shiftrt
          ) , Int, Int) -> ()
  | (Int, N.Eq st, _, _) when (a = b) -> tc_scalar_compatible st a
  | (Int, N.Gt st, Int, Int) -> tc_scalar_compatible st Int
  | (Ptr tr, N.PlusPI, Ptr ta, Int) when tr = ta -> ()
  | (Int, N.MinusPP, Ptr ta, Ptr tb) when ta = tb -> ()
  | _ ->
      failwith
        (String.concat ""
          [ "Incompatible types in binary operation : "
          ; N.string_of_binop op
          ; " : "
          ; string_of_simple a
          ; " x "
          ; string_of_simple b
          ; " -> "
          ; string_of_simple ret
          ]
        )

let tc_unop_compatible ret op le =
  let e = shorten le in
  match (ret, op, e) with
  | (_, (N.Belongs _|N.Coerce _|N.Focus _), _) when ret = e -> ()
  | (Int, (N.Not|N.BNot _), Int) -> ()
  | (Int, N.PtrToInt _, Ptr _) -> ()
  | (Ptr _, N.IntToPtr _, Int) -> ()
  | (_, N.Cast (sret, se), _) ->
      begin
        tc_scalar_compatible sret ret;
        tc_scalar_compatible se e
      end
  | _ ->
      failwith
        (String.concat ""
          [ "Incompatible types in unary operation : "
          ; N.string_of_unop op
          ; " : "
          ; string_of_simple e
          ; " -> "
          ; string_of_simple ret
          ]
        )

let rec get_lv_type env = function
  | T.Local v -> Env.get env (VLocal v)
  | T.Global v -> Env.get env (VGlobal v)
  | T.Deref ((_, t) as e, _sz) ->
      begin
        check_exp env e;
        match (shorten t) with
        | Ptr pt -> pt
        | _ -> invalid_arg "get_lv_type : not dereferencing a pointer type"
      end
  | T.Shift (lv, ((_, te) as e)) ->
      begin
        check_exp env e;
        same_type te Int;
        let tlv = get_lv_type env lv in
        match shorten tlv with
        | Array t -> t
        | _ ->
            let msg =
              Printf.sprintf
                "%s has type %s but should have an array type"
                (T.string_of_lval string_of_simple lv)
                (string_of_simple tlv)
            in
            failwith msg
      end

and check_exp env (be, te) =
  (*
   * General idea :
   *   - check subexps (generic)
   *   - check result (specific)
   *)
  match be with
  | T.Const c -> tc_const_compatible c te
  | T.Lval (lv, ty) ->
      let t_lv = get_lv_type env lv in
      same_type t_lv ty;
      same_type te ty;
  | T.AddrOf lv ->
      let t_lv = get_lv_type env lv in
      same_type te (Ptr t_lv)
  | T.BinOp (op, a, b) ->
      let (_, ta) = a in
      let (_, tb) = b in
      check_exp env a;
      check_exp env b;
      tc_binop_compatible te op ta tb
  | T.UnOp (op, e) ->
      let (_, top) = e in
      check_exp env e;
      tc_unop_compatible te op top
  | T.AddrOfFun (fid, (args, rets)) ->
      let (ea, er) = extract_fun_type (Env.get_fun env fid) in
      List.iter2 same_type ea args;
      List.iter2 same_type er rets

(*
 * Propagate typing constraints through control flow.
 * Besides the exp type in Guard, this is mostly generic.
 *)
let rec check_stmt env (sk, _loc) =
  match sk with
  | T.Set (lv, e, st) ->
      let (_be, te) = e in
      let t_lv = get_lv_type env lv in
      check_exp env e;
      tc_scalar_compatible st te;
      same_type te t_lv;
      ()
  | T.Copy (dst, src, _sz) ->
      let tdst = get_lv_type env dst in
      let tsrc = get_lv_type env src in
      same_type tdst tsrc
  | T.Guard e ->
      let (_be, te) = e in
      check_exp env e;
      same_type te Int
  | T.Decl (vid, ty, blk) ->
      let new_env = Env.add (VLocal vid) ty env in
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
      Env.assert_lbl lbl env
  | T.Call (args, T.FunId fid, rets) ->
      begin
        let targs = List.map (fun (e, t) -> check_exp env e; t) args in
        let t_ret = List.map snd rets in
        let (eargs, eret) = extract_fun_type (Env.get_fun env fid) in
        List.iter2 same_type eargs targs;
        List.iter2 same_type eret t_ret
      end
  | T.Call (_, T.FunDeref _, _) ->
      failwith "no funderef yet"
  | T.UserSpec _ -> ()

and check_blk env =
  List.iter (check_stmt env)

let check_fun env fdec =
  let new_env =
    List.fold_left
      (fun e (name, ty) -> Env.add (VLocal name) ty e)
      env
      (fdec.T.rets@fdec.T.args)
  in
  check_blk new_env fdec.T.body

let env_add_fundecs fdecs env =
  Hashtbl.fold (fun fname fdec e ->
    let t = type_of_fdec fdec in
    Env.add_fun e fname t
  ) fdecs env

let env_add_globals globals env =
  Hashtbl.fold (fun name ty e ->
    Env.add (VGlobal name) ty e
  ) globals env

let check tpk =
  let env =
    env_add_fundecs tpk.T.fundecs
    ( env_add_globals tpk.T.globals
      Env.empty
    )
  in
  check_blk env tpk.T.init;
  List.iter (check_fun env) (Utils.hashtbl_values tpk.T.fundecs)
