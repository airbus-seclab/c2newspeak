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
open Domain

let (+:) = Newspeak.Nat.add
let (-:) = Newspeak.Nat.sub

module Lbl = struct
  let init = 0
  let next = succ
end

(* Abstract domain *)

let dom = Range.dom

let nop x = x

let f_set v e x =
  let lookup v = Box.get_var v x in
  if x = Box.bottom
    then Box.bottom
    else Box.set_var v (dom.eval lookup e) x

let f_guard e (x: Box.t) =
  match dom.guard e with
  | None -> x
  | Some (v, f) -> Box.guard v f x

(**
 * The type used to fold over blocks.
 *
 * lbl is the last label used.
 *)
type stmt_context =
  { lbl      : int (* XXX *)
  ; alist    : (int*int) list (* XXX *)
  ; vertices : Cfg.vertex list
  ; join     : int option   (* XXX *)
  ; wp       : Newspeak.location list
  }

let rec process_stmt (stmt, loc) c =
  let jnode = begin match c.join with
  | None   -> c.lbl
  | Some l -> l
  (* XXX with_default *)
  end in
  match stmt with
  | InfLoop b -> let btm = Lbl.next c.lbl in
                 let c' = process_blk b c.alist btm in
                 { c with lbl      = c'.lbl
                 ;        vertices = (btm, c'.lbl, ("(reloop)", nop))
                                   ::c'.vertices
                                    @c.vertices
                 ;        join     = None
                 }
  | Select (b1, b2) -> let c1 = process_blk b1 c.alist jnode in
                       let c2 = process_blk ~join:c.lbl b2 c.alist c1.lbl in
                       let top = Lbl.next c2.lbl in
                       { c with lbl      = top
                       ;        vertices =  (top, c1.lbl, ("(select : 1)", nop))
                                          ::(top, c2.lbl, ("(select : 2)", nop))
                                          ::c1.vertices
                                           @c2.vertices
                                           @c.vertices
                       ; join = None
                       }
  | DoWith (b1, lmid, b2) -> let c2(*top_b2, v2 XXX *) =
                               process_blk b2 c.alist jnode in
                             let c1(* top_b1, v1 XXX*) =
                               process_blk b1 ((lmid, c2.lbl)::c.alist) c2.lbl
                             in
                             { c with lbl      = c1.lbl
                             ;        vertices = c1.vertices
                                               @ c2.vertices
                                               @  c.vertices
                             ;        join     = None
                             }
  | Goto l -> let lbl' = Lbl.next c.lbl in
              let ljmp = List.assoc l c.alist in
              { c with lbl = lbl'
              ;        vertices = (lbl', ljmp,  ("(jump)", nop))
                                ::c.vertices
              ; join = None
              }
  | Set (v, e) -> let lbl' = Lbl.next c.lbl in
                  { c with lbl = lbl'
                  ; vertices =(lbl', jnode, ("(stmt:set)", f_set v e))
                             ::c.vertices
                  ; join = None
                  }
  | Guard e -> let lbl' = Lbl.next c.lbl in
               { c with lbl = lbl'
               ; vertices =  (lbl', jnode, ("(stmt:guard)", f_guard e))
                           ::c.vertices
               ; join = None
               }
  | Decl b -> let c' = process_blk b c.alist jnode in
                       { c with lbl = c'.lbl
                       ; vertices = c'.vertices@c.vertices
                       ; join = None
                       }
  | AssertFalse -> { c with wp = loc::c.wp }

and process_blk ?join blk al l0 =
    List.fold_right process_stmt blk
      { lbl = l0
      ; alist = al
      ; vertices = []
      ; join = join
      ; wp = []
      }

let process blk =
  let c = process_blk blk [] Lbl.init in
  (c.lbl, c.vertices), c.wp

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
