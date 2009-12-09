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
  | Cfg.Assert_true e -> "(assert:"^Pcomp.Print.exp e^")"

type edge = Cfg.node * Cfg.node * (Cfg.stmt * Newspeak.location)

(**
 * The type used to fold over blocks.
 *
 * lbl is the last label used.
 *)
type 'a stmt_context =
  { lbl   : int                (* XXX *)
  ; alist : (int*int) list     (* XXX *)
  ; edges : edge list
  ; join  : int option         (* XXX *)
  }

(* Edge builders *)
(* a --<l>--> b means (a, b, l  ) *)
(* a --<>-->  b means (a, b, Nop) *)
let (>--<) src lbl = (src,lbl)
let (>-->) (src,lbl) dst = (src,dst,lbl)
let (>--<>-->) src dst = (src,dst,(Cfg.Nop, Newspeak.unknown_loc))

(* Reduce edges *)
let reduce_edges e =
    List.fold_left (fun map (src, dest, (stmt, loc)) ->
      let lst =
        try Cfg.NodeMap.find src map
        with Not_found -> []
      in
        Cfg.NodeMap.add src ((dest,stmt,loc)::lst) map
    ) (Cfg.NodeMap.empty) e

let update ?(new_edges=[]) ~label c =
  { c with lbl = label
  ; join = None
  ; edges = new_edges@c.edges
  }

let rec process_stmt (stmt, loc) c =
  let jnode = Utils.with_default c.lbl c.join in
  match stmt with
  | InfLoop b ->
      let btm = Lbl.next c.lbl in
      let c' = process_blk b c.alist btm in
      update c ~new_edges:((btm >--<>--> c'.lbl) ::c'.edges)
               ~label:c'.lbl
  | Select (b1, b2) ->
      let c1 = process_blk b1 c.alist jnode in
      let c2 = process_blk ~join:c.lbl b2 c.alist c1.lbl in
      let top = Lbl.next c2.lbl in
      update c ~label:top
               ~new_edges:((top >--<>--> c1.lbl)
                         ::(top >--<>--> c2.lbl)
                         ::c1.edges
                          @c2.edges)
  | DoWith (b1, lmid, b2) ->
      let c2 = process_blk b2 c.alist jnode in
      let c1 = process_blk b1 ((lmid, c2.lbl)::c.alist) c2.lbl in
      update c ~label:c1.lbl
               ~new_edges:(c1.edges@c2.edges)
  | Goto l ->
      let lbl' = Lbl.next c.lbl in
      let ljmp = List.assoc l c.alist in
      update c ~label:lbl'
               ~new_edges:[lbl' >--<>--> ljmp]
  | Set (v, e) ->
      let lbl' = Lbl.next c.lbl in
      update c ~label:lbl'
               ~new_edges:[lbl' >--<(Cfg.Set (v, e), loc)>--> jnode]
  | Guard e ->
      let lbl' = Lbl.next c.lbl in
      update c ~label:lbl'
               ~new_edges:[lbl' >--<(Cfg.Guard e, loc)>--> jnode]
  | Decl b ->
      let l_pop = Lbl.next jnode in
      let c' = process_blk b c.alist l_pop in
      let l_push = Lbl.next c'.lbl in
      update c ~label:l_push
               ~new_edges:( (l_push >--<(Cfg.Push, loc)>--> c'.lbl)
                          ::(l_pop  >--<(Cfg.Pop , loc)>--> jnode )
                          ::c'.edges)
  | Assert e ->
      let l = Lbl.next c.lbl in
      update c ~label:l
               ~new_edges:[l >--<(Cfg.Assert_true e, loc)>--> c.lbl]

and process_blk ?join blk al l0 =
    List.fold_right process_stmt blk
      { lbl = l0
      ; alist = al
      ; edges = []
      ; join = join
      }

let process blk vars =
  let c = process_blk blk [] Lbl.init in
  let lbl' = Lbl.next c.lbl in
  let edges = (lbl' >--<(Cfg.Init vars, Newspeak.unknown_loc)>--> c.lbl)::c.edges in
  let map = reduce_edges edges in
  (lbl', map)

let dump_yaml (n, e) =
    "---\n"
  ^ "lastnode: "
  ^ string_of_int n ^ "\n"
  ^ "edges:"
    ^ (if Cfg.NodeMap.is_empty e then " []\n" else
      "\n"
    ^ (Cfg.NodeMap.fold
        (fun src es s ->
          s^"  - {id: "^string_of_int src^ ", s: [" ^ 
          (String.concat ", "
            ( List.map
              ( fun (dest, stmt, _loc) ->
                let s' = print_stmt stmt in
                   "{n: "^ string_of_int dest ^ ", "
                   ^"stmt: \"" ^ s' ^"\"}"
              ) es
            )
          )
          ^"]}\n"
        ) e ""
      )
    )
  ^ "...\n"

let dump_dot ?(results=[||]) (_, e) =
      "digraph G {\nnode [shape=box]\n"
    ^ String.concat "" (Array.to_list (Array.mapi (fun i s ->
      string_of_int i^"[label=\""^s^"\"]\n"
    ) results))

    ^ (Cfg.NodeMap.fold (fun src es s ->
      s^
        (String.concat ""
          (List.map (fun (dest, stmt, _loc) ->
          let s' = print_stmt stmt in
          "  "  ^ string_of_int src
          ^ " -> " ^ string_of_int dest
          ^ " [label=\"" ^ s' ^ "\"]"
          ) es )
        )
    ) e "")
  ^ "\n}\n"
