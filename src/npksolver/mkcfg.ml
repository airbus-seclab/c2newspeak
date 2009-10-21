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
open Prog

let (+:) = Newspeak.Nat.add
let (-:) = Newspeak.Nat.sub

module Lbl = struct
  let init = 0
  let next = succ
end

(* Abstract transfer functions *)

let nop x = x

let f_set v e x =
  if x = Box.bottom then Box.bottom
  else
    match e with
    | Op (Plus,  Var v', Const n) when v' = v -> Box.shift v     n  x
    | Op (Minus, Var v', Const n) when v' = v ->
        Box.shift v (Newspeak.Nat.neg n) x
    | Var v'  -> Box.assign_var ~src:v' ~dest:v x
    | Const n -> Box.set_var v n x
    | _ -> failwith "Unsupported set statement"

let f_guard e x =
  match e with
  |      Op (Gt, Var v, Const n)  -> Box.add_bound v ~min:(n +: Newspeak.Nat.one) x
  |      Op (Gt, Const n, Var v)  -> Box.add_bound v ~max:(n -: Newspeak.Nat.one) x
  |      Op (Eq, Var v, Const n)  -> Box.add_bound v ~min:n ~max:n x
  | Not (Op (Gt, Var v, Const n)) -> Box.add_bound v ~max:n x
  | Not (Op (Gt, Const n, Var v)) -> Box.add_bound v ~min:n x
  | Not (Op (Eq, Var _, Const _)) -> x (* FIXME ?? *)
  | _ -> failwith ( "Warning - unsupported guard statement : " ^ Pcomp.Print.exp e)

(**
 * lbl is the last label used.
 *)
let rec process_stmt (stmt, _) (lbl, alist, vertices, join) =
  let jnode = begin match join with
  | None   -> lbl
  | Some l -> l
  end in
  match stmt with
  | InfLoop b -> let btm = Lbl.next lbl in
                 let (top, vert) = process_blk b alist btm in
                 ( top
                 , alist
                 ,   (btm, top,   ("(reloop)", nop))
                   ::vert
                   @ vertices
                 , None)
  | Select  (b1, b2) -> let (l1, v1) = process_blk           b1 alist jnode in
                        let (l2, v2) = process_blk ~join:lbl b2 alist l1    in
                        let top = Lbl.next l2 in
                        ( top
                        , alist
                        ,  (top, l1, ("(select : 1)", nop))
                         ::(top, l2, ("(select : 2)", nop))
                         ::v1
                         @ v2
                         @ vertices
                        , None)
  | DoWith   (b1, lmid, b2) -> let (top_b2, v2) = process_blk b2 alist jnode in
                               let (top_b1, v1) =
                                 process_blk b1 ((lmid, top_b2)::alist) top_b2
                               in
                               (top_b1, alist, v1 @ v2 @ vertices, None)
  | Goto     l -> let lbl' = Lbl.next lbl in
                  let ljmp = List.assoc l alist in
                  (lbl', alist, (lbl', ljmp,  ("(jump)", nop))
                              ::vertices, None)
  | Set      (v, e) -> let lbl' = Lbl.next lbl in
                       ( lbl'
                       , alist
                       ,   (lbl', jnode, ("(stmt:set)", f_set v e))
                         ::vertices
                       , None)
  | Guard    (e)    -> let lbl' = Lbl.next lbl in
                       ( lbl'
                       , alist
                       ,   (lbl', jnode, ("(stmt:guard)", f_guard e))
                         ::vertices
                       , None)

and process_blk ?join blk al l0 =
  let (lastnode, _, vertices, _) =
    List.fold_right process_stmt blk (l0, al, [], join)
  in (lastnode, vertices)

let process blk = process_blk blk [] Lbl.init

let dump_yaml (n, v) =
    "---\n"
  ^ "lastnode: "
  ^ string_of_int n ^ "\n"
  ^ "vertices:"
    ^ (if v = [] then " []\n" else
      "\n"
    ^ (String.concat "" (List.map (fun (a, b, (s, _)) ->
      "  - [" ^ string_of_int a ^ ", " ^ string_of_int b ^ "]"
      ^ (if s = "" then "" else " # " ^ s)
      ^ "\n") v))
    )
  ^ "...\n"

let dump_dot (_, v) =
    "digraph G {\n"
  ^ String.concat "" (List.map (fun (a, b, (s, _)) ->
       "  "  ^ string_of_int a
    ^ " -> " ^ string_of_int b
    ^ " [label=\"" ^ s ^ "\"]"
  ) v)
  ^ "\n}\n"
