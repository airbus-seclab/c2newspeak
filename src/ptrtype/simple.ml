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

open Simple_types
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

let (new_unknown, reset_unknowns) =
  let c = ref 0 in
  let n () =
    let v = !c in
    incr c;
    Var (ref (Unknown v))
  and r () =
    c := 0
  in
  (n, r)

let rec vars_of_typ = function
  | Int | Float -> []
  | Ptr t | Array t -> vars_of_typ t
  | Var ({contents = Unknown n}) -> [n]
  | Var ({contents = Instanciated t}) -> vars_of_typ t
  | Fun (args, rets) ->
          List.concat (List.map vars_of_typ (rets@args))

let type_clash ta tb =
  let sa = string_of_simple ta in
  let sb = string_of_simple tb in
  print_endline ("Type clash between :\n  "^sa^"\n  "^sb);
  exit 1

let is_atomic_type = function
  | Int
  | Float -> true
  | _ -> false

let occurs n t = List.mem n (vars_of_typ t)

let occurs_check_failed ta tb =
  let sa = string_of_simple ta in
  let sb = string_of_simple tb in
  failwith ("Occurs check failed : cannot unify "^sa^" and "^sb)

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

  | _ ->
      failwith ("Unsupported unop : " ^ N.string_of_unop op)

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
      failwith (Printf.sprintf "No such operation : %s %s %s" sa sop sb)

let rec infer_lv env = function
  | T.Local s -> T.Local s
  | T.Global s -> T.Global s
  | T.Deref (e, sz) -> T.Deref (infer_exp env e, sz)
  | T.Shift (lv, e) -> T.Shift (infer_lv env lv, infer_exp env e)

and lval_type env = function
  | (T.Local _ | T.Global _) as v -> Env.get env v
  | T.Deref(e, _sz) ->
      let (_, te) = infer_exp env e in
      let t = new_unknown () in
      unify (Ptr t) te;
      t
  | T.Shift (lv, e) ->
      let (_, te) = infer_exp env e in
      unify te Int;
      let t = new_unknown () in
      let tlv = lval_type env lv in
      unify (Array t) tlv;
      t

and infer_exp env (e, _) =
  match e with
  | T.Const c -> (T.Const c, infer_const c)
  | T.Lval (lv, _ty) ->
      let lv' = infer_lv env lv in
      let ty = lval_type env lv in
      (T.Lval (lv', ty), ty)
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
      let (a, r) = extract_fun_type (Env.get_fun env fid) in
      let t = Ptr (Fun (a, r)) in
      (T.AddrOfFun (fid, (a, r)), t)

let infer_spectoken env = function
  | T.SymbolToken c -> T.SymbolToken c
  | T.IdentToken s -> T.IdentToken s
  | T.CstToken c -> T.CstToken c
  | T.LvalToken (lv, _ty) ->
      let lv' = infer_lv env lv in
      let ty = lval_type env lv in
      T.LvalToken (lv', ty)

let infer_assertion env = List.map (infer_spectoken env)

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
  | T.Decl (n, _ty, blk) ->
      let var = T.Local n in
      let t0 = new_unknown () in
      let new_env = Env.add var t0 env in
      let blk' = infer_blk new_env blk in
      let ty = lval_type new_env var in
      T.Decl (n, ty, blk')
  | T.UserSpec a -> T.UserSpec (infer_assertion env a)
  | T.Copy (dst, src, sz) ->
      let dst' = infer_lv env dst in
      let src' = infer_lv env src in
      T.Copy (dst', src', sz)
  | T.Guard e ->
      let e' = infer_exp env e in
      T.Guard e'
  | T.Call (args, T.FunId fid, rets) ->
      (*
       * Warning : plumbing ahead.
       *
       * Nothing interesting here : we just construct a Fun type corresponding
       * to the actual arguments and unify it with the formal arguments from
       * the environment.
       *)
      let rets' =
        List.map
          (fun (lv, _) ->
            let lv' = infer_lv env lv
            in (lv', lval_type env lv')
          )
          rets
      in
      let args' =
        List.map
          (fun (e, _) ->
            let (e', t) = infer_exp env e in
            ((e', t), t)
          ) args
      in
      let t_args = List.map snd args' in
      let t_ret = List.map snd rets' in
      let tfcall = Fun (t_args, t_ret) in
      let tf = Env.get_fun env fid in
      unify tf tfcall;
      T.Call (args', T.FunId fid, rets')
  | T.Call (_, T.FunDeref _, _) ->
      failwith "no function pointers yet"

and infer_stmt env (sk, loc) =
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
  let new_env =
    List.fold_left
      (fun e (name, _ty) ->
        Env.add
          (T.Local name)
          (new_unknown ())
          e
      )
      env
      (fdec.T.rets@fdec.T.args)
  in
  let blk = infer_blk new_env fdec.T.body in
  let extract_types l =
    List.map
      (fun (name, _ty) ->
        let t = lval_type new_env (T.Local name) in
        (name, t)
      )
      l
  in
  let fdec' =
    { T.body = blk
    ; T.rets = extract_types fdec.T.rets
    ; T.args = extract_types fdec.T.args
    ; T.position = fdec.T.position
    }
  in
  let t = type_of_fdec fdec' in
  let te = Env.get_fun env fname in
  unify t te;
  fdec'

(*
 * Build an environment from function declarations.
 *
 * Arity (#args and #rets) of global functions is correct
 * but every type is an unknown variable type.
 *)
let fundec_env_infer fdecs =
  Hashtbl.fold (fun fname fdec env ->
    let make_new_type _ =
      new_unknown ()
    in
    let args' = List.map make_new_type fdec.T.args in
    let rets' = List.map make_new_type fdec.T.rets in
    let t = Fun (args', rets') in
    Env.add_fun env fname t
  ) fdecs Env.empty

let infer show_steps tpk =
  reset_unknowns ();
  let env = fundec_env_infer tpk.T.fundecs in

  let init = infer_blk env tpk.T.init in
  let fdecs = Utils.hashtbl_mapi (infer_fdec env) tpk.T.fundecs in

  let global_names = Utils.hashtbl_keys tpk.T.globals in
  let globals = Hashtbl.create 0 in
  List.iter
    (fun g ->
      let t = lval_type env (T.Global g) in
      if (vars_of_typ t <> []) then
        failwith ("Free type variables :\n" ^ g ^ " : " ^ string_of_simple t);
      Hashtbl.add globals g t
    )
    global_names;
  let tpk' =
    { T.globals = globals
    ; T.init = init
    ; T.fundecs = fdecs
    ; T.ptr_sz = tpk.T.ptr_sz
    ; T.src_lang = tpk.T.src_lang
    ; T.abi = tpk.T.abi
    }
  in

  unify_do show_steps tpk';

  tpk'
