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

open Newspeak
module P = PtrSpeak

type info = (string list * PtrSpeak.blk)

(* TODO: should rather have a language and do a preprocessing phase *)
(* TODO: should maybe hoist the variables in newspeak, would eliminate the 
   problem with gotos *)
let hoist_variable_declarations fundec =
  let local_variables = ref [] in
  let env = ref [] in
  let var_cnt = ref 0 in
  let gen_var () = 
    let fresh_variable = "!local"^string_of_int !var_cnt in
      incr var_cnt;
      local_variables := fresh_variable::!local_variables;
      fresh_variable
  in
  let rec process_stmt x =
    match x with
	Set (lv, e, _) -> 
(* TODO: factor combination of translate_lval (process_lval ...)) *)
	  let lv = PtrSpeak.translate_lval (process_lval lv) in
	  let e = PtrSpeak.translate_exp (process_exp e) in
	    P.Set (lv, e)
      | Copy (lv1, lv2, _) -> 
	  let lv1 = PtrSpeak.translate_lval (process_lval lv1) in
	  let lv2 = PtrSpeak.translate_lval (process_lval lv2) in
	  let lv2 = P.Access lv2 in
	    P.Set (lv1, lv2)
      | Guard e -> P.Guard (PtrSpeak.translate_exp (process_exp e))
      | Select (blk1, blk2) -> P.Select (process_blk blk1, process_blk blk2)
      | InfLoop blk -> P.InfLoop (process_blk blk)
      | DoWith (blk, lbl) -> P.DoWith (process_blk blk, lbl)
      | Goto lbl -> P.Goto lbl
      | Call (args, f, rets) -> 
	  let args = process_args args in
	  let rets = process_rets rets in
	    P.Call (args, process_funexp f, rets)
      | UserSpec _ -> 
	  invalid_arg "BottomUp.hoist_variable_declaration: not implemented yet"
      | Decl _ -> invalid_arg "BottomUp.hoist_variable_declaration: dead_code"

  and process_exp x =
    match x with
	Const _ | AddrOfFun _ -> x
      | Lval (lv, t) -> Lval (process_lval lv, t)
      | AddrOf lv -> AddrOf (process_lval lv)
      | UnOp (op, e) -> UnOp (op, process_exp e)
      | BinOp (op, e1, e2) -> BinOp (op, process_exp e1, process_exp e2)

  and process_lval x =
    match x with
(* TODO: make a new language, in which both local and global would be together
   as variables *)
	Local x -> Local (List.assoc x !env)
      | Global _ -> x
      | Deref (e, sz) -> Deref (process_exp e, sz)
      | Shift (lv, e) -> Shift (process_lval lv, process_exp e)

  and process_args x = 
    List.map (fun (x, _) -> PtrSpeak.translate_exp (process_exp x)) x

  and process_funexp x =
    match x with
	FunId x -> x
      | FunDeref _ -> 
	  invalid_arg "Preprocessor.process_funexp: not implemented yet"

  and process_rets x = 
    List.map (fun (x, _) -> PtrSpeak.translate_lval (process_lval x)) x

  and process_blk x = 
    match x with
	(Decl (x, _, blk), _)::tl -> 
	  let backup = !env in
	    env := (x, gen_var ())::!env;
	    let blk = process_blk blk in
	      env := backup;
	      blk @ process_blk tl
      | (x, loc)::tl -> (process_stmt x, loc)::process_blk tl
      | [] -> []

  in

  let parameter_counter = ref (-1) in
  let gen_parameter () =
    incr parameter_counter;
    (* TODO: potential source of bugs: disconnect between this formal 
       definition and the one at function call *)
    "!formal"^string_of_int !parameter_counter
  in
  let register_parameter (x, _) = env := (x, gen_parameter ())::!env in
    List.iter register_parameter fundec.rets;
    List.iter register_parameter fundec.args;
    let body = process_blk fundec.body in
      (!local_variables, body)

let prepare prog = 
  let result = Hashtbl.create 100 in
  let process_fun f declaration =
    let info = hoist_variable_declarations declaration in
      Hashtbl.add result f info
  in
    Hashtbl.iter process_fun prog.fundecs;
    result

