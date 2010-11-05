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
open Prog
open Domain

open Utils.Maybe

let print_stmt = function
  | Cfg.Nop             -> "(nop)"
  | Cfg.Reloop          -> "(reloop)"
  | Cfg.Set (lv, exp,_) -> Pcomp.Print.lval lv ^ " <- " ^ Pcomp.Print.exp exp
  | Cfg.Guard exp       -> "[ " ^ Pcomp.Print.exp exp ^ " ]"
  | Cfg.Push  _sz       -> "(push)"
  | Cfg.Pop             -> "(pop)"
  | Cfg.Init _vars      -> "(init)"
  | Cfg.Assert_true e   -> "(assert:"^Pcomp.Print.exp e^")"
  | Cfg.Call (f,id)     -> "(fcall:"^f^"@"^string_of_int id^")"

type lbl =
  | Lbl   of string * int
  | Entry of string

let lbl_next = function
  | Lbl (f, n) -> Lbl (f, succ n)
  | Entry f -> invalid_arg ("lbl_next : "^f)

let lbl_build ?entries = function
  | Lbl (f, n) -> (f, n)
  | Entry f    ->
      match entries with
      | None -> invalid_arg "lbl_build : no map provided"
      | Some map ->
          try
            (f, Pmap.find f map)
          with Not_found -> invalid_arg ("Can not find function "^f)

type edge = lbl * lbl * (Cfg.stmt * Newspeak.location)

(**
 * The type used to fold over blocks.
 *
 * lbl is the last label used.
 *)
type 'a stmt_context =
  { lbl   : lbl
  ; alist : (int * lbl) list
  ; edges : edge list
  ; join  : lbl option
  }

(* Edge builders *)
(* a --<l>--> b means (a, b, l  ) *)
(* a --<>-->  b means (a, b, Nop) *)
let (>--<) src lbl = (src,lbl)
let (>-->) (src,lbl) dst = (src,dst,lbl)
let (>--<>-->) src dst = (src,dst,(Cfg.Nop, Newspeak.unknown_loc))

(* Reduce edges *)

(* add to a list of values *)
let pmap_add_list : 'k -> 'v -> ('k, 'v list) Pmap.t -> ('k, 'v list) Pmap.t =
  fun k v m ->
  let old_value =
    try
      Pmap.find k m
    with Not_found -> []
  in
  Pmap.add k (v::old_value) m

let reduce_edges entries e =
  List.fold_left (fun map (src, dest, (stmt, loc)) ->
    pmap_add_list (lbl_build ~entries src)
                  (lbl_build ~entries dest
                  ,stmt
                  ,loc)
                  map
  ) (Pmap.empty) e

let update ?(new_edges=[]) ~label c =
  { c with lbl = label
  ; join = None
  ; edges = new_edges@c.edges
  }

let rec process_stmt (stmt, loc) c =
  let jnode = from_maybe c.lbl c.join in
  match stmt with
  | InfLoop b ->
      let btm = lbl_next c.lbl in
      let c' = process_blk b c.alist btm in
      update c ~new_edges:((btm >--<(Cfg.Reloop,Newspeak.unknown_loc)>--> c'.lbl) ::c'.edges)
               ~label:c'.lbl
  | Select (b1, b2) ->
      let c1 = process_blk b1 c.alist jnode in
      let c2 = process_blk ~join:c.lbl b2 c.alist c1.lbl in
      let top = lbl_next c2.lbl in
      update c ~label:top
               ~new_edges:((top >--<>--> c1.lbl)
                         ::(top >--<>--> c2.lbl)
                         ::c1.edges
                          @c2.edges)
  | DoWith (b1, lmid) ->
      let c1 = process_blk b1 ((lmid, jnode)::c.alist) jnode in
      update c ~label:c1.lbl
               ~new_edges:(c1.edges)
  | Goto l ->
      let lbl' = lbl_next c.lbl in
      let ljmp = List.assoc l c.alist in
      update c ~label:lbl'
               ~new_edges:[lbl' >--<>--> ljmp]
  | Set (v, e) ->
      let lbl' = lbl_next c.lbl in
      update c ~label:lbl'
               ~new_edges:[lbl' >--<(Cfg.Set (v, e, loc), loc)>--> jnode]
  | Guard e ->
      let lbl' = lbl_next c.lbl in
      update c ~label:lbl'
               ~new_edges:[lbl' >--<(Cfg.Guard e, loc)>--> jnode]
  | Decl (b, sz) ->
      let l_pop = lbl_next jnode in
      let c' = process_blk b c.alist l_pop in
      let l_push = lbl_next c'.lbl in
      update c ~label:l_push
               ~new_edges:( (l_push >--<(Cfg.Push sz, loc)>--> c'.lbl)
                          ::(l_pop  >--<(Cfg.Pop    , loc)>--> jnode )
                          ::c'.edges)
  | Assert e ->
      let l = lbl_next c.lbl in
      update c ~label:l
               ~new_edges:[l >--<(Cfg.Assert_true e, loc)>--> c.lbl]
  | Call f ->
      let l = lbl_next c.lbl in
      let stmt = (Cfg.Call (lbl_build c.lbl), loc) in
      update c ~label:l
               ~new_edges:[l >--<(stmt)>-->(Entry f)]

and process_blk ?join blk al l0 =
    List.fold_right process_stmt blk
      { lbl = l0
      ; alist = al
      ; edges = []
      ; join = join
      }

let process_fun typs fname blk =
  let c = process_blk blk [] (Lbl (fname, 0)) in
  if fname = "main" then
    let lbl' = lbl_next c.lbl in
    let edges = (lbl' >--<(Cfg.Init typs, Newspeak.unknown_loc)>--> c.lbl)::c.edges in
    (snd (lbl_build lbl'), edges)
  else
    (snd (lbl_build c.lbl), c.edges)

let process prg =
  let funcs = Pmap.mapi (process_fun prg.typ) prg.func in
  let entries = Pmap.map fst funcs in
  Pmap.map (fun (ent, e) -> (ent, reduce_edges entries e)) funcs

let dump_yaml prg =
  let (n, e) =
    try
      Pmap.find "main" prg
    with Not_found -> prerr_endline "No 'main' function was found, exiting."; exit 3
  in
  let edges = 
    (if Pmap.is_empty e then [] else
      (Pmap.foldi
        (fun (fname, src) es s ->
         let succ = 
           List.map
             ( fun ((fdest, dest), stmt, _loc) ->
               let s' = print_stmt stmt in
               Yaml.Dict
                 [ "n"    , Yaml.String (fdest ^ ":" ^ string_of_int dest)
                 ; "stmt" , Yaml.String s'
                 ]
             ) es
         in
          (Yaml.Dict
            [ "id", Yaml.String (fname ^ ":" ^ string_of_int src)
            ; "s" , Yaml.List succ
            ]
          )::s
        ) e []
      )
    )
  in
    (*
  let sort_by_id a b =
    match (a, b) with
    | Yaml.Dict la, Yaml.Dict lb ->
        let key l =
          try List.assoc "id" l
          with Not_found -> failwith "sort_by_id"
        in
        compare (key la) (key lb)
    | _ -> failwith "sort_by_id"
  in
    *)
  Yaml.Dict
    [ "lastnode", Yaml.Int n 
    ; "edges"   , Yaml.List edges
    ]

let dump_dot ?results prg =
  let header =
    match results with
    | None -> ""
    | Some res ->
      Resultmap.fold (fun fn i str s ->
        s ^ fn ^string_of_int i^"[label=\""^str^"\"]\n"
      ) res ""
  in
  let fname = "main" in
  let (_, e) = Pmap.find fname prg in
      "digraph G {\nnode [shape=box]\n"
    ^ header
    ^ (Pmap.foldi (fun (fsrc, src) es s ->
      s^
        (String.concat ""
          (List.map (fun ((fdest, dest), stmt, _loc) ->
          let s' = print_stmt stmt in
            "  "   ^ fsrc  ^ string_of_int src
          ^ " -> " ^ fdest ^ string_of_int dest
          ^ " [label=\"" ^ s' ^ "\"]"
          ) es )
        )
    ) e "")
  ^ "\n}\n"

let to_string prg =
  let nodeid (f,i) = f ^ "+" ^ string_of_int i in
  let loc l =
    if l = Newspeak.unknown_loc then "(unknown loc)"
    else Newspeak.string_of_loc l
  in
  let stmt = function
    | Cfg.Nop -> "Nop"
    | Cfg.Reloop -> "Reloop"
    | Cfg.Set (l, e, _loc) -> "S " ^ Pcomp.Print.lval l ^ " <- " ^ Pcomp.Print.exp e
    | Cfg.Guard e -> "G " ^ Pcomp.Print.exp e
    | Cfg.Push ty -> "Push " ^ Pcomp.Print.typ ty
    | Cfg.Pop -> "Pop"
    | Cfg.Init _ -> "Init (...)"
    | Cfg.Assert_true e -> "Assert " ^ Pcomp.Print.exp e
    | Cfg.Call ni -> "Call, @ret = " ^ nodeid ni
  in
  let func_t (maxv, map) =
      "\tmaxv = " ^ string_of_int maxv ^ "\n"
    ^ "\tmap  =\n"
    ^ ( Pmap.foldi
          ( fun ni edges s ->
            s ^ ( String.concat ""
                    ( List.map
                        ( fun (nj, st, l) ->
                          "\t\t" ^ nodeid ni ^ " --> " ^ nodeid nj
                            ^ "[ (" ^ loc l ^ ") "
                            ^ stmt st ^ "]\n"
                        )
                        edges
                    )
                )
          ) map ""
      )
  in
   "=== CFG ===\n"
  ^ ( Pmap.foldi
        (fun fn ft s ->
            s
          ^ " -= " ^ fn ^ " =- \n"
          ^ func_t ft
        ) prg ""
    )

