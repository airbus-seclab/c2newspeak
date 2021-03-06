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
open Unification

(*************
 * Inference *
 *************)

(*
 * Input : 'a Tyspeak.t
 *
 * Output : simple Tyspeak.t
 *
 * What we have to do here is to fill in the "holes" at each T.exp. For every
 * "new" variable (ie, at a Decl), we can create a new type variable with
 * new_unknown.
 *
 * The unification mechanism will then put constraints on these : whenever a and
 * b are known to be equal (or should be made equal); unify a b will walk the
 * two types and modify the inner structure so that they will compare equal. If
 * this is not possible, it means that the program is not well-typed and an
 * error message will be printed.
 *
 * There are two small pitfalls to avoid infinite types (ie, cycles in the type
 * object graph). It is necessary to
 *
 *   - "shorten" types from time to time, ie removing redundant indirections
 *   - check that no assignments lengthen the types. This is the "occurs" check.
 *
 * NB: This mechanism relies on sharing : when a reference (such as the one
 * under the Var constructor) is shared between two objects, its modification
 * will affect both roots. So, it is important _not_ to do any deep copies.
 *)

open Utils

let (new_unknown, reset_unknowns) =
  let c = ref 0 in
  let n () =
    let v = !c in
    incr c;
    Var (ref (Unknown {id = v}))
  and r () =
    c := 0
  in
  (n, r)

let infer_const = function
  | N.CInt _ -> Int
  | N.CFloat _ -> Float
  | N.Nil ->
      Ptr (new_unknown ())

let infer_unop op (_e, t) =
  match op with
  | N.Belongs _ -> t
  | N.Coerce _ -> t
  | N.Focus _ -> t
  | N.Not -> unify t Int; Int
  | N.BNot _ -> unify t Int; Int
  | N.Cast (N.Float _, N.Float _) -> unify t Float; Float
  | N.IntToPtr _src ->
      warning "Warning : accepting cast int -> ptr";
      unify t Int ;
      Ptr (new_unknown ())

  | _ ->
      error ("Unsupported unop : " ^ N.string_of_unop op)

let infer_binop op (_, a) (_, b) =
  match op with
  | N.PlusI
  | N.MinusI
  | N.MultI
  | N.DivI
  | N.BXor _
  | N.BAnd _
  | N.BOr _
    ->
      unify a Int;
      unify b Int;
      Int
  | N.PlusPI ->
      let t = new_unknown () in
      let p = Ptr t in
      unify a p;
      unify b Int;
      p
  | N.Eq _ | N.Gt _ ->
      unify a b;
      Int
  | _ ->
      let sop = N.string_of_binop op in
      let sa = string_of_simple a in
      let sb = string_of_simple b in
      error (Printf.sprintf "No such operation : %s %s %s" sa sop sb)

let rec infer_lv env = function
  | T.Local s -> T.Local s
  | T.Global s -> T.Global s
  | T.Deref (e, sz) -> T.Deref (infer_exp env e, sz)
  | T.Shift (lv, e) -> T.Shift (infer_lv env lv, infer_exp env e)

and lval_type env = function
  | T.Local v -> Env.get env (VLocal v)
  | T.Global v -> Env.get env (VGlobal v)
  | T.Deref(e, _sz) ->
      let (_, te) = infer_exp env e in
      let t = new_unknown () in
      unify (Ptr t) te;
      t
  | T.Shift (lv, e) ->
      let ((_, te) as exp_off) = infer_exp env e in
      unify te Int;
      let tlv = lval_type env lv in
      let var = match lv with
        | T.Local v -> VLocal v
        | T.Global v -> VGlobal v
        | _ -> failwith "Shift(Deref | Shift) not supported"
      in
      let nt = Env.get_npktype env var in
      match nt with
      | N.Array _ ->
          begin
            let t = new_unknown () in
            unify (Array t) tlv;
            t
          end
      | N.Region _ ->
          begin
            let t = new_unknown () in
            let o = T.static_eval string_of_simple exp_off in
            unify (Struct (ref [(o, t)])) tlv;
            t
          end
      | N.Scalar _ ->
          failwith "Cannot shift a Scalar lvalue"

and infer_exp env (e, _) =
  match e with
  | T.Const c -> (T.Const c, infer_const c)
  | T.Lval (lv, nty, _ty) ->
      let lv' = infer_lv env lv in
      let ty = lval_type env lv in
      (T.Lval (lv', nty, ty), ty)
  | T.AddrOf lv ->
      let lv' = infer_lv env lv in
      let ty = lval_type env lv in
      (T.AddrOf lv', Ptr ty)
  | T.UnOp (op, e) ->
      let e' = infer_exp env e in
      (T.UnOp (op, e'), infer_unop op e')
  | T.BinOp (op, a, b) ->
      let a' = infer_exp env a in
      let b' = infer_exp env b in
      (T.BinOp (op, a', b'), infer_binop op a' b')
  | T.AddrOfFun (fid, _) ->
      let (a, r) = extract_fun_type (Env.get env (VFun fid)) in
      let t = Ptr (Fun (a, r)) in
      (T.AddrOfFun (fid, (a, r)), t)

let infer_spectoken env = function
  | T.SymbolToken c -> T.SymbolToken c
  | T.IdentToken s -> T.IdentToken s
  | T.CstToken c -> T.CstToken c
  | T.LvalToken (lv, nty) ->
      let lv' = infer_lv env lv in
      T.LvalToken (lv', nty)

let infer_assertion env = List.map (infer_spectoken env)

let infer_funexp env = function
  | T.FunId fid -> (T.FunId fid, Env.get env (VFun fid))
  | T.FunDeref e ->
      let (_, t) as e' = infer_exp env e in
      let tf = new_unknown () in
      unify t (Ptr tf);
      (T.FunDeref e', tf)

let rec infer_stmtkind env sk =
  match sk with
  | T.Set (lv, e, st) ->
      let lv' = infer_lv env lv in
      let tlv = lval_type env lv in
      let (_, te) as e' = infer_exp env e in
      unify te tlv;
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
      Env.assert_lbl lbl env;
      T.Goto lbl
  | T.Decl (n, nty, _ty, blk) ->
      let var = T.Local n in
      let t0 = new_unknown () in
      let new_env = Env.add (VLocal n) (Some nty) t0 env in
      let blk' = infer_blk new_env blk in
      let ty = lval_type new_env var in
      T.Decl (n, nty, ty, blk')
  | T.UserSpec a -> T.UserSpec (infer_assertion env a)
  | T.Copy (dst, src, sz) ->
      let dst' = infer_lv env dst in
      let src' = infer_lv env src in
      T.Copy (dst', src', sz)
  | T.Guard e ->
      let e' = infer_exp env e in
      T.Guard e'
  | T.Call (args, fexp, rets) ->
      (*
       * Warning : plumbing ahead.
       *
       * Nothing interesting here : we just construct a Fun type corresponding
       * to the actual arguments and unify it with the formal arguments from
       * the environment.
       *)

      let infer_arg (e, nt) =
        let et = infer_exp env e in
        (et, nt)
      in

      let infer_ret (lv, nt) =
        (infer_lv env lv, nt)
      in

      let args' = List.map infer_arg args in
      let rets' = List.map infer_ret rets in

      let t_args = List.map (fun ((_, t), _) -> t) args' in
      let t_rets = List.map (fun (lv, _) -> lval_type env lv) rets' in

      let (fexp', tf) = infer_funexp env fexp in
      unify tf (Fun (t_args, t_rets));

      T.Call (args', fexp', rets')

and infer_stmt env (sk, loc) =
  Npkcontext.set_loc loc;
  let sk' = infer_stmtkind env sk in
  (sk', loc)

and infer_blk env =
  List.map (infer_stmt env)

let infer_fdec env fname fdec =
  (*
   * Prepare a new environment with rets & args.
   * Infer the body under it.
   * Retrieve the values from the typing environment.
   * (generalization should be done here)
   *)
  Npkcontext.set_loc fdec.T.position;
  let new_env =
    List.fold_left
      (fun e (name, nty, _ty) ->
        Env.add
          (VLocal name)
          (Some nty)
          (new_unknown ())
          e
      )
      env
      (fdec.T.rets@fdec.T.args)
  in
  let blk = infer_blk new_env fdec.T.body in
  let extract_types l =
    List.map
      (fun (name, nty, _ty) ->
        let t = lval_type new_env (T.Local name) in
        (name, nty, t)
      )
      l
  in
  let te = Env.get env (VFun fname) in
  let fdec' =
    { T.body = blk
    ; T.rets = extract_types fdec.T.rets
    ; T.args = extract_types fdec.T.args
    ; T.position = fdec.T.position
    ; T.fdectype = T.Forall ([], te)
    }
  in
  let get_type (_, _, t) = t in
  unify te (Fun (List.map get_type fdec'.T.args, List.map get_type fdec'.T.rets));
  fdec'

(*
 * Extend an environment from function declarations.
 *
 * Arity (#args and #rets) of global functions is correct
 * but every type is an unknown variable type.
 *)
let env_add_fundecs fdecs env =
  Hashtbl.fold (fun fname fdec e ->
    let make_new_type _ =
      new_unknown ()
    in
    let args' = List.map make_new_type fdec.T.args in
    let rets' = List.map make_new_type fdec.T.rets in
    let t = Fun (args', rets') in
    Env.add (VFun fname) None t e
  ) fdecs env

(*
 * Extend an environment from global variables.
 *)
let env_add_globals globals env =
  Hashtbl.fold (fun name (nty, _ty) e ->
    Env.add (VGlobal name) (Some nty) (new_unknown ()) e
  ) globals env

let infer tpk =
  reset_unknowns ();
  let env =
    env_add_fundecs tpk.T.fundecs
      (env_add_globals tpk.T.globals
        Env.empty
      )
  in

  let init = infer_blk env tpk.T.init in
  let fdecs = hashtbl_mapi (infer_fdec env) tpk.T.fundecs in

  let globals =
    hashtbl_mapi (fun g (nt, _t) ->
        let t = Env.get env (VGlobal g) in
        (nt, t)
    ) tpk.T.globals
  in
  
  let tpk' =
    { tpk with
      T.globals = globals
    ; T.init = init
    ; T.fundecs = fdecs
    }
  in

  unify_do tpk';
  Hashtbl.iter (fun g (_nt, t) ->
    if (vars_of_typ t <> []) then
      begin
        error ("Error : free type variables :\n" ^ g ^ " : " ^ string_of_simple t);
      end
  ) globals;

  tpk'
