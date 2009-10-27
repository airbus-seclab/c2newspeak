(*
  This file is part of npksolver, a solver for Newspeak,
  a minimal language framework well suited for static analysis.

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
 *)

(** @author Etienne Millon <etienne.millon@eads.net> *)
open Newspeak

let abort s =
  prerr_endline s;
  exit 3

let fail loc x = abort (string_of_loc loc ^ " : " ^ x)

let assert_int_typ loc = function
  | Scalar (Int (Signed, 32)) -> ()
  | Scalar (Int (Signed, sz)) ->
        fail loc ("Bad int size (" ^ string_of_int sz ^ ")")
  | Scalar (Int (Unsigned, _)) -> fail loc "Unsigned value"
  | Region _ | Array _  ->        fail loc "Not a scalar"
  | Scalar (Float _| FunPtr | Ptr) -> fail loc "Bad scalar"

let assert_int loc st =
  assert_int_typ loc (Scalar st)

let pcomp_var loc = function
  | Global s -> Prog.G s
  | Local  n -> Prog.L n
  | Deref _
  | Shift _ -> fail loc "Pointer dereference"

let pcomp_binop loc binop =
  match binop with
  | PlusI   -> Prog.Plus
  | MinusI  -> Prog.Minus
  | MultI   -> Prog.Mult
  | DivI    -> Prog.Div
  | Gt scal -> assert_int loc scal; Prog.Gt
  | Eq scal -> assert_int loc scal; Prog.Eq
  | PlusF _ | DivF _ | MultF _ | MinusF _
  | BXor  _ | BAnd _ | BOr _
  | MinusPP | PlusPI
  | Shiftrt | Shiftlt
  | Mod -> fail loc "Invalid binary operation"

let rec pcomp_exp loc = function
  | Const (CInt c) -> Prog.Const (Newspeak.Nat.to_int c)
  | Lval (lv, scal) -> assert_int loc scal; Prog.Var (pcomp_var loc lv)
  | UnOp (Not, e1) -> Prog.Not (pcomp_exp loc e1)
  | BinOp (binop, e1, e2) -> let op = pcomp_binop loc binop in
                             Prog.Op (op, (pcomp_exp loc e1)
                                        , (pcomp_exp loc e2))
  | UnOp (Coerce _ , e) -> pcomp_exp loc e
  | AddrOfFun _ | AddrOf _
  | UnOp (( Cast _ | IntToPtr _ | PtrToInt _
          | BNot _ | Belongs  _ | Focus _), _)
  | Const (CFloat _ | Nil) ->
      fail loc "Invalid expression"

let rec pcomp_stmt (sk, loc) =
  let sk' = match sk with
  | Set (lv, exp, scal) -> assert_int loc scal;
                           Some (Prog.Set (pcomp_var loc lv, pcomp_exp loc exp))
  | Guard e             -> Some (Prog.Guard (pcomp_exp loc e))
  | Select (b1, b2)     -> Some (Prog.Select ((pcomp_blk b1), (pcomp_blk b2)))
  | InfLoop b           -> Some (Prog.InfLoop (pcomp_blk b))
  | DoWith (b1, l, b2)  -> Some (Prog.DoWith (pcomp_blk b1, l, pcomp_blk b2))
  | Goto l              -> Some (Prog.Goto l)
  | UserSpec [IdentToken "widen"] -> Options.set_widening (); None
  | UserSpec [IdentToken "assert"
             ;IdentToken "false"] -> Some (Prog.Assert Prog.AFalse)
  | UserSpec [IdentToken "assert"
             ;IdentToken "bound"
             ;LvalToken (v,_)
             ;CstToken (CInt inf)
             ;CstToken (CInt sup)] -> let v' = pcomp_var loc v in
                                      let inf' = Newspeak.Nat.to_int inf in
                                      let sup' = Newspeak.Nat.to_int sup in
                                      Some (Prog.Assert (Prog.ABound (v', inf', sup')))
  | UserSpec [IdentToken "assert"
             ;IdentToken "eq"
             ;LvalToken (v,_)
             ;CstToken (CInt c)] -> let v' = pcomp_var loc v in
                                    let c' = Newspeak.Nat.to_int c in
                                    Some (Prog.Assert (Prog.AEq (v', c')))
  | Decl (_s, _t, blk)  -> Some (Prog.Decl (pcomp_blk blk))
  | _ -> fail loc "Invalid statement"
  in
    Utils.may (fun s -> (s, loc)) sk'

and pcomp_blk x = Utils.filter_map pcomp_stmt x

let compile npk =
  let globals =
  Hashtbl.fold (fun s (ty, loc) l ->
    assert_int_typ loc ty;
    s::l
  ) npk.globals []
  in
  let nfun, blko = Hashtbl.fold (fun fname blk (nfun, _) ->
    ( succ nfun
    , if fname = "main" then Some blk
                        else None)
  ) npk.fundecs (0, None) in
  if nfun > 1 then
    abort "Multiple functions";
  if npk.init <> [] then
    abort "Initialization block";
  match blko with
  | None -> abort "No 'main' function"
  | Some (_, b) -> (pcomp_blk b,globals)

module Print = struct
  open Prog

  let par x = " ("^x^") "

  let binop = function
    | Plus  -> "+"
    | Minus -> "-"
    | Mult  -> "*"
    | Div   -> "/"
    | Gt    -> ">"
    | Eq    -> "=="

  let var = function
    | Prog.L n -> ":"^string_of_int n
    | Prog.G x -> x

  let rec exp = function
    | Const c -> string_of_int c
    | Var v -> var v
    | Not e -> "!" ^par (exp e)
    | Op (op, e1, e2) -> par (exp e1) ^ (binop op) ^ par (exp e2)

end
