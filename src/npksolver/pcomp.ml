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

  Copyright 2009, 2010 Etienne Millon <etienne.millon@eads.net>

 *)

(** @author Etienne Millon <etienne.millon@eads.net> *)
open Lowspeak
module N = Newspeak

let print_loc chan loc =
  output_string chan (Newspeak.string_of_loc loc)

let fail loc x =
  Printf.printf "Compilation error : %a : %s\n" print_loc loc x;
  exit 3

let check_scalar_type loc = function
  | N.Int (N.Signed, 32) -> ()
  | N.Ptr -> ()
  | N.Int (N.Signed, sz)  -> fail loc (Printf.sprintf "Bad int size (%d)" sz)
  | N.Int (N.Unsigned, _) -> fail loc "Unsigned value"
  | _ -> fail loc "Bad scalar"

let rec check_type = function
  | N.Scalar s -> check_scalar_type (N.unknown_loc) s
  | N.Array (t, _sz) -> check_type t
  | N.Region _ -> fail (N.unknown_loc) "Not a scalar"

let pcomp_binop loc binop =
  match binop with
  | N.PlusI   -> Prog.Plus
  | N.MinusI  -> Prog.Minus
  | N.MultI   -> Prog.Mult
  | N.DivI    -> Prog.Div
  | N.Gt scal -> check_scalar_type loc scal; Prog.Gt
  | N.Eq scal -> check_scalar_type loc scal; Prog.Eq
  | N.PlusPI  -> Prog.PlusPtr loc
  | _ -> fail loc "Invalid binary operation"

let rec pcomp_type = function
  | N.Scalar (N.Int _ik) -> Prog.Int
  | N.Scalar N.Ptr -> Prog.Ptr
  | N.Array (ty, sz) -> Prog.Array(pcomp_type ty, sz)
  | N.Region _           -> invalid_arg "pcomp_type : region"
  | N.Scalar (N.Float _sz) -> invalid_arg "pcomp_type : float"
  | N.Scalar N.FunPtr      -> invalid_arg "pcomp_type : fptr"

let rec pcomp_exp loc = function
  | Const (N.CInt c) -> Prog.Const (Prog.CInt (Newspeak.Nat.to_int c))
  | Const N.Nil -> Prog.Const Prog.Nil
  | Lval (lv, scal) -> check_scalar_type loc scal;
                       Prog.Lval (pcomp_var loc lv, pcomp_type (N.Scalar scal)) (* XXX *)
  | UnOp (N.Not, e1) -> Prog.Not (pcomp_exp loc e1)
  | BinOp (binop, e1, e2) -> let op = pcomp_binop loc binop in
                             Prog.Op (op, (pcomp_exp loc e1)
                                        , (pcomp_exp loc e2))
  | UnOp (N.Belongs (a, b), e) -> Prog.Belongs (( N.Nat.to_int a
                                                , N.Nat.to_int b)
                                               , loc
                                               , (pcomp_exp loc e))
  | UnOp ((N.Focus _|N.Coerce _), e) -> pcomp_exp loc e
  | AddrOf lv -> Prog.AddrOf (pcomp_var loc lv)
  | e -> fail loc ("Invalid expression : " ^ Lowspeak.string_of_exp e)

and pcomp_var loc = function
  | Global s      -> Prog.G s
  | Local  n      -> Prog.L n
  | Shift (v, e)  -> Prog.Shift (pcomp_var loc v, pcomp_exp loc e, loc)
  | Deref (e, sz) -> Prog.Deref (pcomp_exp loc e, sz, loc)

let rec pcomp_stmt (sk, loc) =
  let (sk', ann) = match sk with
  | Set (lv, exp, scal) -> check_scalar_type loc scal;
      [Prog.Set (pcomp_var loc lv, pcomp_exp loc exp)],[]
  | Guard e             -> [Prog.Guard (pcomp_exp loc e)],[]
  | Select (b1, b2)     ->
      let (s1, a1) = pcomp_blk b1 in
      let (s2, a2) = pcomp_blk b2 in
      [Prog.Select (s1, s2)], a1@a2
  | InfLoop b           ->
      let (s, a) = pcomp_blk b in
      [Prog.InfLoop s], a
  | DoWith (b, l)  ->
      let (s, a) = pcomp_blk b in
      [Prog.DoWith (s, l)], a
  | Goto l              -> [Prog.Goto l],[]
  | UserSpec [IdentToken "widen"] -> [],[Prog.Widening]
  | UserSpec [IdentToken "domain"; IdentToken d] -> [],[Prog.Domain d]
  | UserSpec [IdentToken "assert"
             ;IdentToken "false"] -> [Prog.Assert (Prog.Const (Prog.CInt 0))],[]
  | UserSpec [IdentToken "assert"
             ;IdentToken "bound"
             ;LvalToken (v,t)
             ;CstToken (N.CInt inf)
             ;CstToken (N.CInt sup)] ->
     let v' = pcomp_var loc v in
     let inf' = Newspeak.Nat.to_int inf in
     let sup' = Newspeak.Nat.to_int sup in
     [Prog.Set ( v'
               , Prog.Belongs ( (inf', sup')
                              , loc
                              , Prog. Lval (v', pcomp_type t)
                              )
               )],[]
  | UserSpec [IdentToken "assert"
             ;IdentToken "eq"
             ;LvalToken (v,t)
             ;CstToken (N.CInt c)] ->
     let v' = pcomp_var loc v in
     let c' = Newspeak.Nat.to_int c in
     [Prog.Assert (Prog.Op
       (Prog.Eq, Prog.Lval (v',
       pcomp_type t),
       Prog.Const (Prog.CInt c')))],[]
  | Decl (_v, t, blk) ->
      let (s, a) = pcomp_blk blk in
      let t' = pcomp_type t in
      [Prog.Decl (s, t')], a
  | Call (FunId f) -> [Prog.Call f],[]
  | _ -> fail loc ("Invalid statement : " ^ Lowspeak.string_of_stmt (sk, loc))
  in
    (List.map (fun x -> (x, loc)) sk'), ann

and pcomp_blk x = List.fold_right (fun s (stmts, anns) ->
    let (s', ann) = pcomp_stmt s in
    s'@stmts, ann@anns) x ([],[])

let compile npk =
  let globals =
  Hashtbl.fold (fun s ty l ->
    check_type ty;
    (s, pcomp_type ty)::l
  ) npk.globals []
  in
  let typ = List.fold_left (fun m (n, t) -> Pmap.add n t m) (Pmap.create String.compare) globals in
  let (blk_init, ann_init) = pcomp_blk npk.init in
  let (func, anns) =
    Hashtbl.fold
      (fun fname declaration (map, anns) ->
         let (blk, anns') = pcomp_blk declaration.body in
         let block =
           if fname = "main" then blk_init@blk
           else blk
         in
         (Pmap.add fname block map, anns'@anns)
      ) npk.fundecs (Pmap.create String.compare, ann_init)
  in
  { Prog.func = func
  ;      anns = anns
  ;      typ  = typ
  }

let rec size_of_typ =
  function
  | Prog.Int     -> 32
  | Prog.Ptr     -> 32
  | Prog.Array (ty, n) -> n * size_of_typ ty

module Print = struct
  open Prog

  let binop = function
    | Plus  -> "+" | Minus -> "-" | Mult  -> "*"
    | Div   -> "/" | Gt    -> ">" | Eq    -> "=="
    | PlusPtr _ -> "+ptr"

  let rec lval = function
    | L n              -> Printf.sprintf ":%d" n
    | G x              -> x
    | Shift (v, e, _l) -> Printf.sprintf "%s[%s]" (lval v) (exp e)
    | Deref (e, s, _l) -> Printf.sprintf "*(%d)(%s)" s (exp e)

  and exp = function
    | Const (CInt c) -> string_of_int c
    | Const Nil -> "null"
    | AddrOf lv -> "&("^lval lv^")"
    | Lval (v,_) -> lval v
    | Not e -> "!" ^ exp e
    | Op (op, e1, e2) -> exp e1 ^ binop op ^ exp e2
    | Belongs ((a, b), _, e) ->
        Printf.sprintf "Belongs(%d;%d)[%s]" a b (exp e)

  let stmtk = function
    | Set (v, e) -> lval v ^ " = " ^ exp e
    | Guard    e -> "[" ^ exp e ^ "]"
    | Decl     _ -> "(decl)"
    | InfLoop  _ -> "(infloop)"
    | Select   _ -> "(select)"
    | DoWith   _ -> "(dowith)"
    | Goto     _ -> "(goto)"
    | Assert   e -> "Assert (" ^ exp e ^ ")"
    | Call     f -> "Call ("^f^")"

  let addr = function
    | Stack x -> "L"^string_of_int x
    | Heap  x -> x

  let rec typ = function
    | Int -> "Int"
    | Ptr -> "Ptr"
    | Array (ty, n) -> Printf.sprintf "%s[%d]" (typ ty) n 

end
