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

let nop x = x

let f_set dom v e x =
  let lookup v = Box.get_var dom v x in
  if x = Box.bottom
    then Box.bottom
    else Box.set_var dom v (dom.eval lookup e) x

let f_guard dom e x =
  match dom.guard e with
  | None -> x
  | Some (v, f) -> Box.guard dom v f x

(**
 * The type used to fold over blocks.
 *
 * lbl is the last label used.
 *)
type 'a stmt_context =
  { lbl      : int                (* XXX *)
  ; alist    : (int*int) list     (* XXX *)
  ; vertices : 'a Cfg.vertex list
  ; join     : int option         (* XXX *)
  ; wp       : (Newspeak.location * int * Prog.check) list
  ; dom      : 'a Domain.t
  }

let rec process_stmt (stmt, loc) c =
  let jnode = begin match c.join with
  | None   -> c.lbl
  | Some l -> l
  (* XXX with_default *)
  end in
  match stmt with
  | InfLoop b -> let btm = Lbl.next c.lbl in
                 let c' = process_blk c.dom b c.alist btm in
                 { c with lbl      = c'.lbl
                 ;        vertices = (btm, c'.lbl, "(reloop)", nop)
                                   ::c'.vertices
                                    @c.vertices
                 ;        join     = None
                 ; wp = c'.wp @ c.wp
                 }
  | Select (b1, b2) -> let c1 = process_blk c.dom b1 c.alist jnode in
                       let c2 = process_blk c.dom ~join:c.lbl b2 c.alist c1.lbl in
                       let top = Lbl.next c2.lbl in
                       { c with lbl      = top
                       ;        vertices =  (top, c1.lbl, "", nop)
                                          ::(top, c2.lbl, "", nop)
                                          ::c1.vertices
                                           @c2.vertices
                                           @c.vertices
                       ; join = None
                       ; wp = c1.wp @ c2.wp @ c.wp
                       }
  | DoWith (b1, lmid, b2) -> let c2 =
                               process_blk c.dom b2 c.alist jnode in
                             let c1 =
                               process_blk c.dom b1 ((lmid, c2.lbl)::c.alist) c2.lbl
                             in
                             { c with lbl      = c1.lbl
                             ;        vertices = c1.vertices
                                               @ c2.vertices
                                               @  c.vertices
                             ;        join     = None
                             ; wp = c1.wp @ c2.wp @ c.wp
                             }
  | Goto l -> let lbl' = Lbl.next c.lbl in
              let ljmp = List.assoc l c.alist in
              { c with lbl = lbl'
              ;        vertices = (lbl', ljmp, "(jump)", nop)
                                ::c.vertices
              ; join = None
              }
  | Set (v, e) -> let lbl' = Lbl.next c.lbl in
                  { c with lbl = lbl'
                  ; vertices = ( lbl'
                               , jnode
                               , Pcomp.Print.stmtk stmt
                               , f_set c.dom v e)
                               ::c.vertices
                  ; join = None
                  }
  | Guard e -> let lbl' = Lbl.next c.lbl in
               { c with lbl = lbl'
               ; vertices =  (lbl', jnode, Pcomp.Print.stmtk stmt, f_guard c.dom e)
                           ::c.vertices
               ; join = None
               }
  | Decl b -> let l_pop = Lbl.next jnode in
              let c' = process_blk c.dom b c.alist l_pop in
              let l_push = Lbl.next c'.lbl in
                       { c with lbl = l_push
                       ; vertices =   (l_push, c'.lbl, "(push)", Box.push c.dom)
                                    ::(l_pop, jnode  , "(pop)" , Box.pop  c.dom)
                                    ::c'.vertices
                                     @c.vertices
                       ; join = None
                       ; wp = c'.wp@c.wp
                       }
  | Assert ck ->
      let l = Lbl.next c.lbl in
      { c with lbl = l
      ;   vertices = (l,c.lbl, "(watchpoint)", nop)::c.vertices
      ;         wp = (loc, l, ck)::c.wp }

and process_blk ?join dom blk al l0 =
    List.fold_right process_stmt blk
      { lbl = l0
      ; alist = al
      ; vertices = []
      ; join = join
      ; wp = []
      ; dom = dom
      }

let init dom vars x =
  List.fold_left (fun r v ->
      Box.meet dom (Box.singleton (Prog.G v) (dom.from_val 0)) r
    ) x vars

let process blk vars dom =
  let c = process_blk dom blk [] Lbl.init in
  let lbl' = Lbl.next c.lbl in
  (lbl', (lbl',c.lbl,"(init)", init dom vars)::c.vertices), c.wp

let dump_yaml (n, v) =
    "---\n"
  ^ "lastnode: "
  ^ string_of_int n ^ "\n"
  ^ "vertices:"
    ^ (if v = [] then " []\n" else
      "\n"
    ^ (String.concat "" (List.map (fun (a, b, s, _) ->
      "  - [" ^ string_of_int a ^ ", " ^ string_of_int b ^ "]"
      ^ (if s = "" then "" else " # " ^ s)
      ^ "\n") v))
    )
  ^ "...\n"

let dump_dot ?(results=[||]) (_, v) =
      "digraph G {\nnode [shape=box]\n"
    ^ String.concat "" (Array.to_list (Array.mapi (fun i s ->
      string_of_int i^"[label=\""^s^"\"]\n"
    ) results))
    ^ String.concat "" (List.map (fun (a, b, s, _) ->
       "  "  ^ string_of_int a
    ^ " -> " ^ string_of_int b
    ^ " [label=\"" ^ s ^ "\"]"
  ) v)
  ^ "\n}\n"
