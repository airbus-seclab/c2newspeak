open Prog

module Lbl = struct
  let init = 0
  let next = succ
end

let nop (x:Range.t) :Range.t = x

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
  | Set      _
  | Guard    _ -> let lbl' = Lbl.next lbl in
                  (lbl', alist, (lbl', jnode, ("(stmt)", nop))::vertices, None)

and process_blk ?join blk al l0 =
  let (lastnode, _, vertices, _) =
    List.fold_right process_stmt blk (l0, al, [], join)
  in (lastnode, vertices)

let process blk = process_blk blk [] Lbl.init

let dump (n, v) =
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
