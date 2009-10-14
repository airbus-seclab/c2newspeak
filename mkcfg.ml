open Prog

module Lbl = struct
  let init = 0
  let next = succ
end

(**
 * lbl is the last label used.
 *)
let rec process_stmt ?join (stmt,_) (lbl, vertices) =
  match stmt with
  | InfLoop b -> let (top, vert) = process_blk b lbl in
                 (top, (lbl, top, ())::vert@vertices)
  | Select  (b1, b2) -> let (l1, v1) = process_blk b1 lbl in
                        let (l2, v2) = process_blk ~join:lbl b2 l1 in
                        let top = Lbl.next l2 in
                        (top, (top,l1,())::(top,l2,())::v1@v2@vertices)
  | Set      _
  | Guard    _
  | DoWith   _
  | Goto     _ -> let lbl' = Lbl.next lbl in
                  let join_node = begin match join with
                  | None   -> lbl
                  | Some l -> l
                  end in
                  (lbl', (lbl', join_node, ())::vertices)

and process_blk ?join blk l0 =
  List.fold_right (fun x y -> process_stmt ?join x y) blk (l0, [])

let process blk = process_blk blk Lbl.init

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
