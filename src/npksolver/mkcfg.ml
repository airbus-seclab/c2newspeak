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

module Lbl = struct
  let init = 0
  let next = succ
end

let print_stmt = function
  | Cfg.Nop           -> "(nop)"
  | Cfg.Set (lv, exp) -> Pcomp.Print.lval lv ^ " <- " ^ Pcomp.Print.exp exp
  | Cfg.Guard exp     -> "[ " ^ Pcomp.Print.exp exp ^ " ]"
  | Cfg.Push          -> "(push)"
  | Cfg.Pop           -> "(pop)"
  | Cfg.Init _vars    -> "(init)"

(**
 * The type used to fold over blocks.
 *
 * lbl is the last label used.
 *)
type 'a stmt_context =
  { lbl   : int                (* XXX *)
  ; alist : (int*int) list     (* XXX *)
  ; edges : Cfg.edge list
  ; join  : int option         (* XXX *)
  ; wp    : (Newspeak.location * int * Prog.check) list
  }

(* Edge builders *)
(* a --<l>--> b means (a, b, l  ) *)
(* a --<>-->  b means (a, b, Nop) *)
let (--<) src lbl = (src,lbl)
let (>-->) (src,lbl) dst = (src,dst,lbl)
let (--<>-->) src dst = (src,dst,Cfg.Nop)

let rec process_stmt (stmt, loc) c =
  let jnode = begin match c.join with
  | None   -> c.lbl
  | Some l -> l
  (* XXX with_default *)
  end in
  match stmt with
  | InfLoop b -> let btm = Lbl.next c.lbl in
                 let c' = process_blk b c.alist btm in
                 { c with lbl   = c'.lbl
                 ;        edges = (btm --<>--> c'.lbl)
                                  ::c'.edges
                                   @c.edges
                 ;        join    = None
                 ; wp = c'.wp @ c.wp
                 }
  | Select (b1, b2) -> let c1 = process_blk b1 c.alist jnode in
                       let c2 = process_blk ~join:c.lbl b2 c.alist c1.lbl in
                       let top = Lbl.next c2.lbl in
                       { c with lbl   = top
                       ;        edges = (top --<>--> c1.lbl)
                                      ::(top --<>--> c2.lbl)
                                      ::c1.edges
                                       @c2.edges
                                       @c.edges
                       ; join = None
                       ; wp = c1.wp @ c2.wp @ c.wp
                       }
  | DoWith (b1, lmid, b2) -> let c2 =
                               process_blk b2 c.alist jnode in
                             let c1 =
                               process_blk b1 ((lmid, c2.lbl)::c.alist) c2.lbl
                             in
                             { c with lbl   = c1.lbl
                             ;        edges = c1.edges
                                               @ c2.edges
                                               @  c.edges
                             ;        join  = None
                             ; wp = c1.wp @ c2.wp @ c.wp
                             }
  | Goto l -> let lbl' = Lbl.next c.lbl in
              let ljmp = List.assoc l c.alist in
              { c with lbl = lbl'
              ;        edges = (lbl' --<>--> ljmp)
                                ::c.edges
              ; join = None
              }
  | Set (v, e) -> let lbl' = Lbl.next c.lbl in
                  { c with lbl = lbl'
                  ; edges = ( lbl' --<Cfg.Set (v, e)>--> jnode )
                               ::c.edges
                  ; join = None
                  }
  | Guard e -> let lbl' = Lbl.next c.lbl in
               { c with lbl = lbl'
               ; edges = (lbl' --<Cfg.Guard e>--> jnode)
                          ::c.edges
               ; join = None
               }
  | Decl b -> let l_pop = Lbl.next jnode in
              let c' = process_blk b c.alist l_pop in
              let l_push = Lbl.next c'.lbl in
                       { c with lbl = l_push
                       ; edges = (l_push --<Cfg.Push>--> c'.lbl)
                                  ::(l_pop  --<Cfg.Pop >--> jnode )
                                  ::c'.edges
                                   @c.edges
                       ; join = None
                       ; wp = c'.wp@c.wp
                       }
  | Assert ck ->
      let l = Lbl.next c.lbl in
      { c with lbl = l
      ;   edges = (l --<>--> c.lbl)::c.edges
      ;         wp = (loc, l, ck)::c.wp }

and process_blk ?join blk al l0 =
    List.fold_right process_stmt blk
      { lbl = l0
      ; alist = al
      ; edges = []
      ; join = join
      ; wp = []
      }

let process blk vars =
  let c = process_blk blk [] Lbl.init in
  let lbl' = Lbl.next c.lbl in
  (lbl', (lbl'--<Cfg.Init vars>--> c.lbl)::c.edges), c.wp

let dump_yaml (n, e) =
    "---\n"
  ^ "lastnode: "
  ^ string_of_int n ^ "\n"
  ^ "edges:"
    ^ (if e = [] then " []\n" else
      "\n"
    ^ (String.concat "" (List.map (fun (a, b, stmt) ->
      let s = print_stmt stmt in
      "  - [" ^ string_of_int a ^ ", " ^ string_of_int b ^ "]"
      ^ (if s = "" then "" else " # " ^ s)
      ^ "\n") e))
    )
  ^ "...\n"

let dump_dot ?(results=[||]) (_, e) =
      "digraph G {\nnode [shape=box]\n"
    ^ String.concat "" (Array.to_list (Array.mapi (fun i s ->
      string_of_int i^"[label=\""^s^"\"]\n"
    ) results))
    ^ String.concat "" (List.map (fun (a, b, stmt) ->
      let s = print_stmt stmt in
       "  "  ^ string_of_int a
    ^ " -> " ^ string_of_int b
    ^ " [label=\"" ^ s ^ "\"]"
  ) e)
  ^ "\n}\n"
