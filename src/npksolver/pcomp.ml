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

let check_scalar_type loc = function
  | Int (Signed, 32) -> ()
  | Ptr -> ()
  | Int (Signed, sz)  -> fail loc ("Bad int size (" ^ string_of_int sz ^ ")")
  | Int (Unsigned, _) -> fail loc "Unsigned value"
  | _ -> fail loc "Bad scalar"

let rec check_type loc = function
  | Scalar s -> check_scalar_type loc s
  | Array (t, _sz) -> check_type loc t
  | Region _ -> fail loc "Not a scalar"

let pcomp_binop loc binop =
  match binop with
  | PlusI   -> Prog.Plus
  | MinusI  -> Prog.Minus
  | MultI   -> Prog.Mult
  | DivI    -> Prog.Div
  | Gt scal -> check_scalar_type loc scal; Prog.Gt
  | Eq scal -> check_scalar_type loc scal; Prog.Eq
  | PlusPI  -> Prog.PlusPtr loc
  | _ -> fail loc "Invalid binary operation"

let rec pcomp_type = function
  | Scalar (Int _ik) -> Prog.Int
  | Scalar Ptr -> Prog.Ptr
  | Array (ty, sz) -> Prog.Array(pcomp_type ty, sz)
  | Region _           -> invalid_arg "pcomp_type : region"
  | Scalar (Float _sz) -> invalid_arg "pcomp_type : float"
  | Scalar FunPtr      -> invalid_arg "pcomp_type : fptr"

let rec pcomp_exp loc = function
  | Const (CInt c) -> Prog.Const (Prog.CInt (Newspeak.Nat.to_int c))
  | Const Nil -> Prog.Const Prog.Nil
  | Lval (lv, scal) -> check_scalar_type loc scal;
                       Prog.Lval (pcomp_var loc lv, pcomp_type (Scalar scal)) (* XXX *)
  | UnOp (Not, e1) -> Prog.Not (pcomp_exp loc e1)
  | BinOp (binop, e1, e2) -> let op = pcomp_binop loc binop in
                             Prog.Op (op, (pcomp_exp loc e1)
                                        , (pcomp_exp loc e2))
  | UnOp (Belongs (a, b), e) -> Prog.Belongs (( Newspeak.Nat.to_int a
                                              , Newspeak.Nat.to_int b)
                                             , loc
                                             , (pcomp_exp loc e))
  | UnOp ((Focus _|Coerce _), e) -> pcomp_exp loc e
  | AddrOf lv -> Prog.AddrOf (pcomp_var loc lv)
  | e -> fail loc ("Invalid expression : " ^ Newspeak.string_of_exp e)

and pcomp_var loc = function
  | Global s     -> Prog.G s
  | Local  n     -> Prog.L n
  | Shift (v, e) -> Prog.Shift (pcomp_var loc v, pcomp_exp loc e, loc)
  | Deref  _ -> fail loc "Pointer dereference"

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
  | DoWith (b1, l, b2)  ->
      let (s1, a1) = pcomp_blk b1 in
      let (s2, a2) = pcomp_blk b2 in
      [Prog.DoWith (s1, l, s2)], a1@a2
  | Goto l              -> [Prog.Goto l],[]
  | UserSpec [IdentToken "widen"] -> [],[Prog.Widening]
  | UserSpec [IdentToken "domain"; IdentToken d] -> [],[Prog.Domain d]
  | UserSpec [IdentToken "assert"
             ;IdentToken "false"] -> [Prog.Assert (Prog.Const (Prog.CInt 0))],[]
  | UserSpec [IdentToken "assert"
             ;IdentToken "bound"
             ;LvalToken (v,t)
             ;CstToken (CInt inf)
             ;CstToken (CInt sup)] ->
     let v' = pcomp_var loc v in
     let inf' = Newspeak.Nat.to_int inf in
     let sup' = Newspeak.Nat.to_int sup in
     [Prog.Set ( v'
               , Prog.Belongs ( (inf', sup')
                              , loc
                              , Prog. Lval (v', pcomp_type (Scalar t))
                              )
               )],[]
  | UserSpec [IdentToken "assert"
             ;IdentToken "eq"
             ;LvalToken (v,t)
             ;CstToken (CInt c)] ->
     let v' = pcomp_var loc v in
     let c' = Newspeak.Nat.to_int c in
     [Prog.Assert (Prog.Op
       (Prog.Eq, Prog.Lval (v',
       pcomp_type (Scalar t)),
       Prog.Const (Prog.CInt c')))],[]
  | Decl (_v, t, blk) ->
      let (s, a) = pcomp_blk blk in
      let t' = pcomp_type t in
      [Prog.Decl (s, t')], a
  | Call (FunId f) -> [Prog.Call f],[]
  | _ -> fail loc ("Invalid statement : " ^ Newspeak.string_of_stmt (sk, loc))
  in
    (List.map (fun x -> (x, loc)) sk'), ann

and pcomp_blk x = List.fold_right (fun s (stmts, anns) ->
    let (s', ann) = pcomp_stmt s in
    s'@stmts, ann@anns) x ([],[])

let compile npk =
  let globals =
  Hashtbl.fold (fun s (ty, loc) l ->
    check_type loc ty;
    (s, pcomp_type ty)::l
  ) npk.globals []
  in
  let typ = List.fold_left (fun m (n, t) -> Pmap.add n t m) (Pmap.create String.compare) globals in
  let (blk_init, ann_init) = pcomp_blk npk.init in
  let (func, anns) =
    Hashtbl.fold
      (fun fname (_,b) (map, anns) ->
         let (blk, anns') = pcomp_blk b in
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
    | L n              -> ":"^string_of_int n
    | G x              -> x
    | Shift (v, e, _l) -> lval v ^ "[" ^ exp e ^ "]"

  and exp = function
    | Const (CInt c) -> string_of_int c
    | Const Nil -> "null"
    | AddrOf lv -> "&("^lval lv^")"
    | Lval (v,_) -> lval v
    | Not e -> "!" ^ exp e
    | Op (op, e1, e2) -> exp e1 ^ binop op ^ exp e2
    | Belongs ((a, b), _, e) -> "Belongs(" ^ string_of_int a
                                        ^ ";" ^ string_of_int b
                                        ^ ")[" ^ exp e ^ "]"

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
    | Array (ty, n) -> typ ty ^ "[" ^ string_of_int n ^ "]"


end
