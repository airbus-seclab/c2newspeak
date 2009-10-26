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
  }

let rec process_stmt (stmt, _) c =
  let jnode = begin match c.join with
  | None   -> c.lbl
  | Some l -> l
  (* XXX with_default *)
  end in
  match stmt with
  | InfLoop b -> let btm = Lbl.next c.lbl in
                 let (top, vert) = process_blk b c.alist btm in
                 { c with lbl      = top
                 ;        vertices = (btm, top, ("(reloop)", nop))
                                   ::vert
                                    @c.vertices
                 ;        join     = None
                 }
  | Select  (b1, b2) -> let (l1, v1) = process_blk           b1 c.alist jnode in
                        let (l2, v2) = process_blk ~join:c.lbl b2 c.alist l1    in
                        let top = Lbl.next l2 in
                        { c with lbl      = top
                        ;        vertices =  (top, l1, ("(select : 1)", nop))
                                           ::(top, l2, ("(select : 2)", nop))
                                           ::v1
                                            @v2
                                            @c.vertices
                        ; join = None
                        }
  | DoWith   (b1, lmid, b2) -> let (top_b2, v2) = process_blk b2 c.alist jnode in
                               let (top_b1, v1) =
                                 process_blk b1 ((lmid, top_b2)::c.alist) top_b2
                               in
                               { c with lbl      = top_b1
                               ;        vertices = v1 @ v2 @ c.vertices
                               ;        join     = None
                               }
  | Goto     l -> let lbl' = Lbl.next c.lbl in
                  let ljmp = List.assoc l c.alist in
                  { c with lbl = lbl'
                  ;        vertices = (lbl', ljmp,  ("(jump)", nop))
                                    ::c.vertices
                  ; join = None
                  }
  | Set      (v, e) -> let lbl' = Lbl.next c.lbl in
                       { c with lbl = lbl'
                       ; vertices =(lbl', jnode, ("(stmt:set)", f_set v e))
                                  ::c.vertices
                       ; join = None
                       }
  | Guard    (e)    -> let lbl' = Lbl.next c.lbl in
                       { c with lbl = lbl'
                       ; vertices =  (lbl', jnode, ("(stmt:guard)", f_guard e))
                                   ::c.vertices
                       ; join = None
                       }
  | Decl b -> let (top, vert) = process_blk b c.alist jnode in
                       { c with lbl = top
                       ; vertices = vert@c.vertices
                       ; join = None
                       }

and process_blk ?join blk al l0 =
  let c =
    List.fold_right process_stmt blk
      { lbl = l0
      ; alist = al
      ; vertices = []
      ; join = join
      }
  in (c.lbl, c.vertices)

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
