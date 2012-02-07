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
  | Fun of simple list * simple option
  | Ptr of simple
  | Var of var_type ref

let extract_fun_type = function
  | Fun (a, r) -> (a, r)
  | _ -> assert false

let rec string_of_simple = function
  | Int -> "Int"
  | Float -> "Float"
  | Ptr s -> "Ptr (" ^ string_of_simple s ^ ")"
  | Var {contents = Unknown n} -> "_a"^string_of_int n
  | Var {contents = Instanciated s} -> string_of_simple s
  | Fun (args, ret) ->
          let string_of_ret = function
              | None -> "Void"
              | Some t -> string_of_simple t
          in
            "("
          ^ String.concat " * " (List.map string_of_simple args)
          ^ ") -> "
          ^ string_of_ret ret

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
  let error () =
    print_endline
      ( "Scalar type "
      ^ N.string_of_scalar st
      ^ " is not compatible with "
      ^ string_of_simple ty
      );
    assert false
  in
  match (st, ty) with
  | (N.Int _, Int) -> ()
  | (N.FunPtr, Ptr (Fun (_, _))) -> ()
  | (N.FunPtr, _) -> error ()
  | (N.Ptr, Ptr (Fun (_, _))) -> error ()
  | (N.Ptr, Ptr _) -> ()
  | (N.Float _, Float) -> ()
  | _ -> error ()

let same_type_option xo yo =
  match (xo, yo) with
  | None, None -> ()
  | Some x, Some y -> same_type x y
  | _ -> assert false

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
  | T.AddrOfFun (fid, (args, retl)) ->
      let ret = Utils.option_of_list retl in
      let (ea, er) = extract_fun_type (Env.get_fun env fid) in
      List.iter2 same_type ea args;
      same_type_option er ret

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
  | T.Call (args, T.FunId fid, ret) ->
      begin
        let reto = Utils.option_of_list ret in
        let t_ret = Utils.option_map snd reto in
        let targs = List.map snd args in
        let (eargs, eret) = extract_fun_type (Env.get_fun env fid) in
        List.iter2 same_type eargs targs;
        same_type_option eret t_ret
      end
  | T.Call (_, T.FunDeref _, _) ->
      failwith "no funderef yet"
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

let type_of_fdec fdec =
  let args' = List.map snd fdec.T.args in
  let reto = Utils.option_of_list fdec.T.rets in
  let reto' = Utils.option_map snd reto in
  Fun (args', reto')

let fundec_env_check fdecs =
  Hashtbl.fold (fun fname fdec env ->
    let t = type_of_fdec fdec in
    Env.add_fun env fname t
  ) fdecs Env.empty

let check tpk =
  let env = fundec_env_check tpk.T.fundecs in
  check_blk env tpk.T.init;
  List.iter (check_fun env) (Utils.hashtbl_values tpk.T.fundecs)

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
  | Ptr t -> vars_of_typ t
  | Var ({contents = Unknown n}) -> [n]
  | Var ({contents = Instanciated t}) -> vars_of_typ t
  | Fun (args, ret) ->
          let ret_list = Utils.list_of_option ret in
          List.concat (List.map vars_of_typ (ret_list@args))

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

let occurs_check_failed ta tb =
  let sa = string_of_simple ta in
  let sb = string_of_simple tb in
  failwith ("Occurs check failed : cannot unify "^sa^" and "^sb)

let rec unify ta tb =
  let sta = shorten ta in
  let stb = shorten tb in
  match (sta, stb) with
  | ((Var ({contents = Unknown na} as ra)),
     (Var  {contents = Unknown nb})) ->
       begin
         if na <> nb then
           ra := Instanciated stb
       end
  | ((Var ({contents = Unknown n} as r)), t)
    ->
      begin
        if occurs n t then
          occurs_check_failed sta stb
        else
          r := Instanciated t
      end
  | (_, (Var ({contents = Unknown _}))) -> unify stb sta
  | _ when is_atomic_type sta && sta = stb -> ()

  | Ptr pa, Ptr pb -> unify pa pb
  | Fun (args_a, Some ret_a), Fun (args_b, Some ret_b) ->
      List.iter2 unify args_a args_b;
      unify ret_a ret_b
  | Fun (args_a, None), Fun (args_b, None) ->
      List.iter2 unify args_a args_b
    
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
  | T.AddrOfFun (fid, _) ->
      let (a, r) = extract_fun_type (Env.get_fun env fid) in
      let t = Ptr (Fun (a, r)) in
      (T.AddrOfFun (fid, (a, Utils.list_of_option r)), t)

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
  | T.Call (args, T.FunId fid, rets) ->
      (*
       * Warning : plumbing ahead.
       *
       * Nothing interesting here : we just construct a Fun type corresponding
       * to the actual arguments and unify it with the formal arguments from
       * the environment.
       *)
      let reto' =
        Utils.option_map
          (fun (lv, _) ->
            let lv' = infer_lv env lv
            in (lv', lval_type env lv')
          )
          (Utils.option_of_list rets)
      in
      let args' =
        List.map
          (fun (e, _) ->
            let (e', t) = infer_exp env e in
            ((e', t), t)
          ) args
      in
      let t_args = List.map snd args' in
      let t_ret = Utils.option_map snd reto' in
      let tfcall = Fun (t_args, t_ret) in
      let tf = Env.get_fun env fid in
      unify tf tfcall;
      let rets' = Utils.list_of_option reto' in
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
    let reto = Utils.option_of_list fdec.T.rets in
    let reto' = Utils.option_map make_new_type reto in
    let t = Fun (args', reto') in
    Env.add_fun env fname t
  ) fdecs Env.empty

let infer tpk =
  reset_unknowns ();
  let env = fundec_env_infer tpk.T.fundecs in

  let init = infer_blk env tpk.T.init in
  let fdecs = Utils.hashtbl_mapi (infer_fdec env) tpk.T.fundecs in

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

