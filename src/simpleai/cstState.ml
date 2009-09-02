(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.
  
  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org
*)

open Simple

module Map = Map.Make(struct type t = Simple.vid let compare = compare end)

let read s x = 
  try Map.find x s 
  with Not_found -> invalid_arg ("CstState.read: variable "^x^" not declared")


type cst = 
    Top
  | Val of Int32.t

let cst_join x y =
  match (x, y) with
      (Val x, Val y) when x = y -> Val x
    | _ -> Top

let apply_not x =
  match x with
      Val i when i = Int32.zero -> Val Int32.one
    | Val _ -> Val Int32.zero
    | Top -> Top

let apply_gt x y =
  match (x, y) with
      (Val x, Val y) -> 
	if Int32.compare x y > 0 then Val Int32.one
	else Val Int32.zero
    | _ -> Top

let add x y =
  match (x, y) with
      (Val x, Val y) -> 
	let z = Int32.add x y in
	  if (Int32.compare z y < 0) || (Int32.compare z x < 0) then Top
	  else Val z
    | _ -> Top

let apply_binop op x y =
  match op with
      PlusI -> add x y
    | Gt -> apply_gt x y
    | _ -> Top

let cst_is_safe_binop x =
  match x with
      (PlusI, Val x, Val y) ->
	let z = Int32.add x y in
	  (Int32.compare z y >= 0) && (Int32.compare z x >= 0)
    | ((Eq|Gt), _, _) -> true
    | _ -> false
	
let cst_implies _ = false

(* None is emptyset *)
type t = cst Map.t option

let universe = Some Map.empty

let contains s1 s2 =
  match (s1, s2) with
      (_, None) -> true
    | (None, _) -> false
    | (Some s1, Some s2) ->
(* for each constraint in s1 there should be a stricter constraint in s2 *)
	try
	  let has_constraint x v1 =
	    let v2 = read s2 x in
	      match (v1, v2) with
		  (Val i, Val j) when i <> j -> raise Exit
		| _ -> ()
	  in
	    Map.iter has_constraint s1;
	    true
	with Exit -> false
    

let join s1 s2 = 
  match (s1, s2) with
      (None, s) | (s, None) -> s
    | (Some s1, Some s2) -> 
	let res = ref Map.empty in
	let join_info x v1 =
	  let v2 = read s2 x in
	  let v = cst_join v1 v2 in
	    res := Map.add x v !res
	in
	  Map.iter join_info s1;
	  Some !res

let add_var x s = 
  match s with
      Some s -> Some (Map.add x Top s)
    | None -> None

let eval_lval lv =
  match lv with
      Global x -> x

let rec eval_exp s e =
  match e with
      Const CInt i -> Val i
    | Lval Global x -> read s x
    | UnOp (Not, e) -> apply_not (eval_exp s e)
    | BinOp (op, e1, e2) -> 
	let v1 = eval_exp s e1 in
	let v2 = eval_exp s e2 in
	  apply_binop op v1 v2

let assign lv e s = 
  match s with
      None -> None
    | Some s -> 
	let x = eval_lval lv in
	let v = eval_exp s e in
	  Some (Map.add x v s)

let guard e s = 
  match s with
      None -> None
    | Some s -> 
	let v = eval_exp s e in
	  match v with
	      Val i when i = Int32.zero -> None
	    | _ -> Some s

let implies s (lv, cmp, c) = 
  match s with
      None -> true
    | Some s -> 
	let x = eval_lval lv in
	let v = read s x in
	let c =
	  match c with
	      CInt i -> i
	in
	  cst_implies (v, cmp, c)

let is_safe_binop s (op, e1, e2) = 
  match s with
      None -> true
    | Some s -> 
	let v1 = eval_exp s e1 in
	let v2 = eval_exp s e2 in
	  cst_is_safe_binop (op, v1, v2)
		
let to_string s = 
  match s with
      None -> "{}"
    | Some s -> 
	let res = ref "" in
	let string_of_info x v =
	  let v = 
	    match v with
		Val i -> Int32.to_string i
	      | Top -> "?"
	  in
	    res := !res^x^" -> "^v^" "
	in
	  Map.iter string_of_info s;
	  !res
