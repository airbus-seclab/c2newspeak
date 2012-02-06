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
  | Float
  | Ptr of simple
  | Var of var_type ref

let rec string_of_simple = function
  | Int -> "Int"
  | Float -> "Float"
  | Ptr s -> "Ptr (" ^ string_of_simple s ^ ")"
  | Var {contents = Unknown n} -> "_a"^string_of_int n
  | Var {contents = Instanciated s} -> string_of_simple s

let tc_assert ok =
  if not ok then
    assert false

let rec shorten = function
  | Var ({contents = Instanciated (Var _ as t)} as vt) ->
      let t2 = shorten t in
      vt := Instanciated t;
      t2
  | Var {contents = Instanciated t} -> t
  | t -> t

let same_type lx ly =
  let x = shorten lx in
  let y = shorten ly in
  if x <> y then
    begin
      let sx = string_of_simple x in
      let sy = string_of_simple y in
      Printf.printf "Typechecking failed : %s != %s\n" sx sy;
      assert false
    end

let tc_scalar_compatible st lty =
  let ty = shorten lty in
  match (st, ty) with
  | (N.Int _, Int) -> ()
  | (N.Ptr, Ptr _) -> ()
  | (N.Float _, Float) -> ()
  | _ -> assert false

(************
 * Checking *
 ************)

let tc_const_compatible cst ty =
  match (cst, ty) with
  | (N.CInt _, Int) -> ()
  | (N.Nil, Ptr _) -> ()
  | (N.CFloat _, Float) -> ()
  | _ ->
      print_endline
        ( "Incompatibility between "
        ^ N.string_of_cst cst
        ^ " and "
        ^ string_of_simple ty
        );
      assert false

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
      print_endline
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
        );
      assert false

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
      print_endline
        (String.concat ""
          [ "Incompatible types in unary operation : "
          ; N.string_of_unop op
          ; " : "
          ; string_of_simple e
          ; " -> "
          ; string_of_simple ret
          ]
        );
      assert false

let get_lv_type env = function
  | (T.Local _ | T.Global _) as lv ->
      begin try
        Env.get env lv
      with Not_found ->
        begin
          failwith ("Cannot find lval : "^T.string_of_lval string_of_simple lv)
        end
      end
  | T.Deref ((_, t), _sz) ->
      begin
        match (shorten t) with
        | Ptr pt -> pt
        | _ -> assert false
      end
  | T.Shift _ -> assert false

let rec check_exp env (be, te) =
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
  | T.AddrOfFun (_fid, _ftyp) -> assert false

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

let check_fun env fdec =
  let new_env =
    List.fold_left
      (fun e (name, ty) -> Env.add (T.Local name) ty e)
      env
      (fdec.T.rets@fdec.T.args)
  in
  check_blk new_env fdec.T.body

let check tpk =
  let env = Env.empty in
  check_blk env tpk.T.init;
  List.iter (check_fun env) (Utils.hashtbl_values tpk.T.fundecs)

(*************
 * Inference *
 *************)

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
  | Ptr t -> vars_of_typ t
  | Var ({contents = Unknown n}) -> [n]
  | Var ({contents = Instanciated t}) -> vars_of_typ t

let no_vars_in t =
  vars_of_typ t = []

let type_clash ta tb =
  let sa = string_of_simple ta in
  let sb = string_of_simple tb in
  failwith ("Type clash : "^sa^" vs "^sb)

let is_atomic_type = function
  | Int
  | Float -> true
  | _ -> false

let occurs n t = List.mem n (vars_of_typ t)

let rec unify ta tb =
  let sta = shorten ta in
  let stb = shorten tb in
  match (sta, stb) with
  | ((Var ({contents = Unknown n} as r)), t)
    ->
      begin
        if occurs n t then
          type_clash sta stb
        else
          r := Instanciated t
      end
  | (_, (Var ({contents = Unknown _}))) -> unify stb sta
  | _ when is_atomic_type sta && sta = stb -> ()

  | Ptr pa, Ptr pb -> unify pa pb
    
  | _ -> type_clash sta stb

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
      Printf.printf "Unsupported unop : %s\n" (N.string_of_unop op);
      assert false

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
  | N.Eq _ ->
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
  | T.Shift _ -> assert false

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
  | T.AddrOfFun _ -> assert false

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
      tc_assert (Env.has_lbl lbl env);
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
  | T.Call _ -> assert false

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
      let t = lval_type env (T.Global g) in
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

