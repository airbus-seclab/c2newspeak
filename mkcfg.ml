open Prog

module Lbl = struct
  let init = 0
  let next = succ
end

let process_stmt (stmt,_) (lbl, vertices) =
  match stmt with
  | InfLoop _ -> (lbl, (lbl, lbl, ())::vertices)
  | Set      _
  | Guard    _
  | Select   _
  | DoWith   _
  | Goto     _ -> let lbl' = Lbl.next lbl in
                  (lbl', (lbl, lbl', ())::vertices)

let process blk =
  List.fold_right process_stmt blk (Lbl.init, [])

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
