open Newspeak

let visit_unop x =
  match x with
      Belongs _ -> Npkstats.count Npkstats.array
    | Coerce _ | Not | BNot _ | Cast _ | PtrToInt _ -> ()

let visit_binop x =
  match x with
      PlusPI -> Npkstats.count Npkstats.pointer_arith
    | PlusI | MinusI | MultI | DivI | Mod
    | PlusF _ | MinusF _ | MultF _ | DivF _
    | BOr _ | BAnd _ | BXor _ | Shiftlt | Shiftrt 
    | MinusPP | Gt _ | Eq _ -> ()

let rec visit_lval x =
  match x with
      Deref (ptr, _) -> 
	Npkstats.count Npkstats.pointer_deref;
	visit_exp ptr
    | Shift (lv, off) -> 
	visit_lval lv; 
	visit_exp off
    | Local _ | Global _ -> ()

and visit_exp x =
  match x with
      Lval (lv, _) -> visit_lval lv
    | AddrOf (lv, _) -> visit_lval lv
    | UnOp (op, e) -> visit_unop op; visit_exp e
    | BinOp (op, e1, e2) -> visit_binop op; visit_exp e1; visit_exp e2
    | Const _ | AddrOfFun _ -> ()

let visit_fn x =
  match x with
      FunId f -> Npkstats.count_call f
    | FunDeref _ -> Npkstats.count Npkstats.fpointer

let rec visit_stmt (x, _) = 
  match x with
      Set (lv, e, _) -> visit_lval lv; visit_exp e
    | Copy (dst, src, _) -> visit_lval dst; visit_lval src
    | Decl (_, _, body) -> visit_blk body
    | Call fn -> visit_fn fn
    | ChooseAssert x -> List.iter visit_choice x
    | InfLoop body -> 
	Npkstats.count Npkstats.loop;
	visit_blk body
    | Label _ | Goto _ -> ()

and visit_choice (cond, body) = 
  List.iter visit_exp cond;
  visit_blk body

and visit_decl (_, _, init) =
  match init with
      Zero -> ()
    | Init x -> List.iter visit_init_val x

and visit_init_val (_, _, e) = visit_exp e

and visit_blk x = List.iter visit_stmt x

let visit_fundec (_, x) =
  match x with
      None -> ()
    | Some body -> 
	Npkstats.count Npkstats.funct;
	visit_blk body

let count (globs, fundecs) =
  List.iter visit_decl globs;
  Hashtbl.iter (fun _ f -> visit_fundec f) fundecs
