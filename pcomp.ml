open Newspeak

let abort s =
  prerr_endline s;
  exit 3

let fail loc x = abort (string_of_loc loc^" : "^x)

let assert_int_typ loc t =
  if (t <> Scalar (Int (Unsigned, 32))) then
    fail loc "Not an int"

let assert_int loc st =
  assert_int_typ loc (Scalar st)

let pcomp_var loc = function
  | Global s -> s
  | _ -> fail loc "Not a global variable"

let pcomp_binop loc binop =
  match binop with
  | PlusI   -> Prog.Plus
  | MinusI  -> Prog.Minus
  | MultI   -> Prog.Mult
  | DivI    -> Prog.Div
  | Gt scal -> assert_int loc scal; Prog.Gt
  | Eq scal -> assert_int loc scal; Prog.Eq
  | _ -> fail loc "Invalid binary operation"

let rec pcomp_exp loc = function
  | Const (CInt c) -> Prog.Const c
  | Lval (lv, scal) -> assert_int loc scal; Prog.Var (pcomp_var loc lv)
  | UnOp (Not, e1) -> Prog.Not (pcomp_exp loc e1)
  | BinOp (binop, e1, e2) -> let op = pcomp_binop loc binop in
                             Prog.Op (op, (pcomp_exp loc e1),
                                          (pcomp_exp loc e2))
  | _ -> fail loc "Invalid expression"

let rec pcomp_stmt (sk, loc) =
  let sk' = match sk with
  | Set (lv, exp, scal) -> assert_int loc scal;
                           Prog.Set (pcomp_var loc lv, pcomp_exp loc exp)
  | Guard e             -> Prog.Guard (pcomp_exp loc e)
  | Decl (s, ty, b)     -> assert_int_typ loc ty; Prog.Decl (s, pcomp_blk b)
  | Select (b1, b2)     -> Prog.Select ((pcomp_blk b1), (pcomp_blk b2))
  | InfLoop b           -> Prog.InfLoop (pcomp_blk b)
  | DoWith (b1, l, b2)  -> Prog.DoWith (pcomp_blk b1, l, pcomp_blk b2)
  | Goto l              -> Prog.Goto l
  | UserSpec asrt       -> Prog.UserSpec asrt
  | _ -> fail loc "Invalid statement"
  in
    (sk', loc)

and pcomp_blk x = List.map pcomp_stmt x

let compile npk =
  let (_, main_blk) =
    try Hashtbl.find npk.fundecs "main"
    with Not_found -> abort "No 'main' function"
  in
  pcomp_blk main_blk
