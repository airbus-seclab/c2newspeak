
module Lbl = struct
  let init = 0
  let next = succ
end

let process_stmt stmt (nodes, vertices, lbl) =
  let lbl' = Lbl.next lbl in
  ignore stmt;
  (lbl' :: nodes, (lbl, lbl', ())::vertices, lbl')

let process blk =
  let (n, v, _ ) =
    let l0 = Lbl.init in
    List.fold_right process_stmt blk ([l0], [], l0)
  in
  (n, v)

let dump (n, v) =
    "---\n"
  ^ "nodes:\n"
  ^ String.concat "" (List.map (fun n -> "  - "^string_of_int n^"\n") n)
  ^ "vertices:"
    ^ (if v = [] then " []\n" else 
      "\n"
    ^ (String.concat "" (List.map (fun (a, b, _) ->
      "  - ["^string_of_int a^", "^string_of_int b^"]\n") v))
    )
  ^ "...\n"
