open Prog

module Lbl = struct
  let init = 0
  let next = succ
end

(* Abstract transfer functions *)

let nop (x:Range.t) :Range.t = x

let f_set = function
  | Const n -> let n' = Newspeak.Nat.to_int n in
               (fun _ -> Range.from_bounds n' n')
  | Op (Plus, Var _, Const n) -> let n' = Newspeak.Nat.to_int n in
                                 Range.shift n'
  (* Unsupported statements *)
  | Op (Plus, (Op _|Not _|Const _), _)
  | Op (Plus, Var _, _)
  | Op ((Eq|Gt|Div|Minus|Mult), _, _)
  | Not _ | Var _
      -> (fun _ -> failwith "Unsupported set statement")

let f_guard _ =
  prerr_endline "Warning : unsupported guard statement";
  nop

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
  | Set      (_, e) -> let lbl' = Lbl.next lbl in
                       ( lbl'
                       , alist
                       ,   (lbl', jnode, ("(stmt:set)", f_set e))
                         ::vertices
                       , None)
  | Guard    (e)    -> let lbl' = Lbl.next lbl in
                       ( lbl'
                       , alist
                       ,   (lbl', jnode, ("(stmt:guard)", f_guard e))
                         ::vertices
                       , None)

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
