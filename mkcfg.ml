open Prog

module Lbl = struct
  let init = 0
  let next = succ
end

(**
 * lbl is the last label used.
 *)
let rec process_stmt ?join (stmt,_) (lbl, alist, vertices) =
  match stmt with
  | InfLoop b -> let btm = Lbl.next lbl in
                 let (top, vert) = process_blk b alist btm in
                 (top, alist, (btm, top, ())::(btm, lbl, ())::vert@vertices)
  | Select  (b1, b2) -> let (l1, v1) = process_blk b1 alist lbl in
                        let (l2, v2) = process_blk ~join:lbl b2 alist l1 in
                        let top = Lbl.next l2 in
                        (top, alist, (top,l1,())::(top,l2,())::v1@v2@vertices)

  | DoWith   (b1, lmid, b2) -> let (top_b2, v2) = process_blk b2 alist lbl in
                               let (top, v1) =
                                 prerr_endline ("label "^string_of_int lmid^" = node "
                                              ^(string_of_int top_b2));
                                 process_blk b1 ((lmid, top_b2)::alist) top_b2
                               in
                               (top, alist, v1@v2@vertices)
  | Goto     l -> let lbl' = Lbl.next lbl in
                  let ljmp = List.assoc l alist in
                  (lbl', alist, (lbl', ljmp, ())::(lbl', lbl, ())::vertices)
  | Set      _
  | Guard    _ -> let lbl' = Lbl.next lbl in
                  let join_node = begin match join with
                  | None   -> lbl
                  | Some l -> l
                  end in
                  (lbl', alist, (lbl', join_node, ())::vertices)

and process_blk ?join blk al l0 =
  let (lastnode, _, vertices) =
    List.fold_right (fun x y -> process_stmt ?join x y) blk (l0, al, [])
  in (lastnode, vertices)

let process blk = process_blk blk [] Lbl.init

let dump (n, v) =
    "---\n"
  ^ "lastnode: "
  ^ string_of_int n^"\n"
  ^ "vertices:"
    ^ (if v = [] then " []\n" else 
      "\n"
    ^ (String.concat "" (List.map (fun (a, b, _) ->
      "  - ["^string_of_int a^", "^string_of_int b^"]\n") v))
    )
  ^ "...\n"
