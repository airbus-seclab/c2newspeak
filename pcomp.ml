open Newspeak

let abort s =
  prerr_endline s;
  exit 3

let fail loc x = abort (string_of_loc loc ^ " : " ^ x)

let assert_int_typ loc = function
  | Scalar (Int (Signed, 32)) -> ()
  | Scalar (Int (Signed, sz)) ->
        fail loc ("Bad int size (" ^ string_of_int sz ^ ")")
  | Scalar (Int (Unsigned, _)) -> fail loc "Unsigned value"
  | Region _ | Array _  ->        fail loc "Not a scalar"
  | Scalar (Float _| FunPtr | Ptr) -> fail loc "Bad scalar"

let assert_int loc st =
  assert_int_typ loc (Scalar st)

let pcomp_var loc = function
  | Global s -> s
  | Local _
  | Deref _
  | Shift _ -> fail loc "Not a global variable"

let pcomp_binop loc binop =
  match binop with
  | PlusI   -> Prog.Plus
  | MinusI  -> Prog.Minus
  | MultI   -> Prog.Mult
  | DivI    -> Prog.Div
  | Gt scal -> assert_int loc scal; Prog.Gt
  | Eq scal -> assert_int loc scal; Prog.Eq
  | PlusF _ | DivF _ | MultF _ | MinusF _
  | BXor  _ | BAnd _ | BOr _
  | MinusPP | PlusPI
  | Shiftrt | Shiftlt
  | Mod -> fail loc "Invalid binary operation"

let rec pcomp_exp loc = function
  | Const (CInt c) -> Prog.Const c
  | Lval (lv, scal) -> assert_int loc scal; Prog.Var (pcomp_var loc lv)
  | UnOp (Not, e1) -> Prog.Not (pcomp_exp loc e1)
  | BinOp (binop, e1, e2) -> let op = pcomp_binop loc binop in
                             Prog.Op (op, (pcomp_exp loc e1)
                                        , (pcomp_exp loc e2))
  | UnOp (Coerce _ , e) -> pcomp_exp loc e
  | AddrOfFun _ | AddrOf _
  | UnOp (( Cast _ | IntToPtr _ | PtrToInt _
          | BNot _ | Belongs  _ | Focus _), _)
  | Const (CFloat _ | Nil) ->
      fail loc "Invalid expression"


let rec pcomp_stmt (sk, loc) =
  let sk' = match sk with
  | Set (lv, exp, scal) -> assert_int loc scal;
                           Prog.Set (pcomp_var loc lv, pcomp_exp loc exp)
  | Guard e             -> Prog.Guard (pcomp_exp loc e)
  | Select (b1, b2)     -> Prog.Select ((pcomp_blk b1), (pcomp_blk b2))
  | InfLoop b           -> Prog.InfLoop (pcomp_blk b)
  | DoWith (b1, l, b2)  -> Prog.DoWith (pcomp_blk b1, l, pcomp_blk b2)
  | Goto l              -> Prog.Goto l
  | UserSpec _
  | Call _
  | Copy _
  | Decl _              -> fail loc "Invalid statement"
  in
    (sk', loc)

and pcomp_blk x = List.map pcomp_stmt x

let compile npk =
  (* Check that all variables are int *)
  Hashtbl.iter (fun _ (ty, loc) ->
    assert_int_typ loc ty
  ) npk.globals;
  let nfun, blko = Hashtbl.fold (fun fname blk (nfun, _) ->
    ( succ nfun
    , if fname = "main" then Some blk
                        else None)
  ) npk.fundecs (0, None) in
  if nfun > 1 then
    abort "Multiple functions";
  match blko with
  | None -> abort "No 'main' function"
  | Some (_, b) -> pcomp_blk b

