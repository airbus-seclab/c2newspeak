
module Lbl = struct
  let init = 0
  let next = succ
end

let process_stmt stmt (lbl, vertices) =
  let lbl' = Lbl.next lbl in
  ignore stmt;
  (lbl', (lbl, lbl', ())::vertices)

let process blk =
  let l0 = Lbl.init in
  List.fold_right process_stmt blk (l0, [])

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
