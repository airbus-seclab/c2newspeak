(*
 * ptrtype: do finer typechecks on C pointers
 * Copyright (C) 2011-2012 Etienne Millon
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

module N = Newspeak
module T = Tyspeak

(*
 * Tyspeak builder.
 *
 * to_ty will be applied for every Newspeak.ty.
 * nil_ty will be added as a label for exery 'ty T.exp.
 *)
let convert to_ty nil_ty npk =
  let c_globals =
    Utils.hashtbl_map (fun t -> (t, to_ty t))
  in

  let c_fty (x, y) =
    ( List.map to_ty x
    , List.map to_ty y
    )
  in

  let rec c_lv = function
    | N.Local s -> T.Local s
    | N.Global s -> T.Global s
    | N.Deref (e, sz) -> T.Deref (c_exp e, sz)
    | N.Shift (lv, e) -> T.Shift (c_lv lv, c_exp e)
  and c_bexp = function
    | N.Const c -> T.Const c
    | N.Lval (lv, ty) -> T.Lval (c_lv lv, ty, nil_ty)
    | N.AddrOf lv -> T.AddrOf (c_lv lv)
    | N.AddrOfFun (s, fty) -> T.AddrOfFun (s, c_fty fty)
    | N.UnOp (op, e) -> T.UnOp (op, c_exp e)
    | N.BinOp (op, e1, e2) -> T.BinOp (op, c_exp e1, c_exp e2)
  and c_exp e = (c_bexp e, nil_ty)
  in

  let c_args =
    List.map (fun (e, ty) -> (c_exp e, ty))
  in

  let c_rets =
    List.map (fun (lv, ty) -> (c_lv lv, ty))
  in

  let c_funexp = function
    | N.FunId s -> T.FunId s
    | N.FunDeref e -> T.FunDeref (c_exp e)
  in

  let c_spec = function
    | N.SymbolToken c      -> T.SymbolToken c
    | N.IdentToken s       -> T.IdentToken s
    | N.LvalToken (lv, ty) -> T.LvalToken (c_lv lv, ty)
    | N.CstToken c         -> T.CstToken c
  in

  let c_assertion =
    List.map c_spec
  in

  let rec c_sk = function
    | N.Set (lv, e, st) -> T.Set (c_lv lv, c_exp e, st)
    | N.Copy (lv1, lv2, sz) -> T.Copy (c_lv lv1, c_lv lv2, sz)
    | N.Guard e -> T.Guard (c_exp e)
    | N.Decl (s, ty, blk) -> T.Decl (s, ty, to_ty ty, c_blk blk)
    | N.Select (blk1, blk2) -> T.Select (c_blk blk1, c_blk blk2)
    | N.InfLoop blk -> T.InfLoop (c_blk blk)
    | N.DoWith (blk, lbl) -> T.DoWith (c_blk blk, lbl)
    | N.Goto lbl -> T.Goto lbl
    | N.Call (args, fe, rets) -> T.Call (c_args args, c_funexp fe, c_rets rets)
    | N.UserSpec a -> T.UserSpec (c_assertion a)
  and c_blk blk =
    List.map (fun (sk, loc) -> (c_sk sk, loc)) blk
  in

  let c_fundec f =
    let c_argsrets =
      List.map (fun (s, ty) -> (s, ty, to_ty ty))
    in
    { T.args = c_argsrets f.N.args
    ; T.rets = c_argsrets f.N.rets
    ; T.body = c_blk f.N.body
    ; T.position = f.N.position
    ; T.fdectype = T.Forall ([], nil_ty)
    }
  in

  let c_fundecs =
    Utils.hashtbl_map c_fundec 
  in

  { T.globals  = c_globals npk.N.globals
  ; T.init     = c_blk     npk.N.init
  ; T.fundecs  = c_fundecs npk.N.fundecs
  ; T.ptr_sz   = npk.N.ptr_sz
  ; T.src_lang = npk.N.src_lang
  ; T.abi      = npk.N.abi
  }

let convert_unit = convert ignore ()
