(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans, Olivier Levillain
  
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

module S = Smallspeak

let debug = ref false

let speclist = 
  [("--debug", Arg.Set debug, 
    "prints information about intermediate computations");
  ]

let input = ref ""

let anon_fun file =
  if !input = ""
  then input := file
  else invalid_arg "You can only analyse one file at a time."

let usage_msg = Sys.argv.(0)^" [options] [-help|--help] file.npk"

let translate (globdecs, fundecs, _) = 
  let vars = ref [] in
  let prog = ref [] in

  let rec translate_lval lv =
    match lv with
	Global x -> S.Var x
      | Deref (e, _) -> S.Deref (translate_exp e)
      | Shift (lv, _) -> translate_lval lv
      | _ -> 
	  invalid_arg ("Npkpointer.translate_lval: "
		       ^"left value not implemented yet")
	    
  and translate_exp e =
    match e with
	Const _ -> S.Const
      | Lval (lv, _) -> S.Deref (translate_lval lv)
      | AddrOf (lv, _) -> S.var_of_exp (translate_lval lv)
      | _ -> 
	  invalid_arg "Npkpointer.translate_exp: expression not implemented yet"
  in
    
  let rec translate_stmt (x, _) =
    match x with
	Set (lv, e, _) ->
	  let lv = translate_lval lv in
	  let e = translate_exp e in
	    prog := (lv, e)::!prog
      | ChooseAssert choices -> List.iter translate_choice choices
      | InfLoop body -> translate_blk body
      | DoWith (body, _, action) -> 
	  translate_blk body;
	  translate_blk action
      | Goto _ -> ()
      | _ -> 
	  invalid_arg "Npkpointer.translate_stmt: statement not implemented yet"

  and translate_choice (_, body) = translate_blk body

  and translate_blk x = List.iter translate_stmt x in

  let translate_init_list x init =
    let rec translate init =
      match init with
	  [] -> ()
	| (_, _, e)::tl -> 
	    let e = translate_exp e in
	      prog := (S.Var x, e)::!prog;
	      translate tl
    in
      translate init
  in

  let translate_global x (_, init) =
    vars := x::!vars;
    match init with
	Zero -> ()
      | Init init_list -> translate_init_list x init_list
  in

  let translate_fundec _ (ftyp, body) =
    if ftyp <> ([], None) then begin
      invalid_arg ("Npkpointer.translat_fundec: "
		   ^"only void -> void functions supported right now")
    end;
    translate_blk body
  in

    Hashtbl.iter translate_global globdecs;
    Hashtbl.iter translate_fundec fundecs;
    (!vars, !prog)
      
let join p1 p2 = List.merge compare p1 p2
let singleton x = x::[]
let emptyset = []
let rec is_subset x y =
  match (x, y) with
      (p1::_, p2::_) when p1 <> p2 -> false
    | (_::x, _::y) -> is_subset x y
    | _ -> x = []

let analyse (vars, prog) = 
  let g = Hashtbl.create 100 in
  let changed = ref false in
  let init x = Hashtbl.add g x [] in
  let deref p = Hashtbl.find g p in
  (* TODO: could be slightly optimized by using a update function *)
  let add_pointsto x y = 
    let p = Hashtbl.find g x in
      if not (is_subset y p) then begin
	Hashtbl.replace g x (join p y);
	changed := true
      end
  in
    
  let rec eval_exp e =
    match e with
	S.Const -> emptyset
      | S.Var x -> singleton x
      | S.Deref e ->
	  let p = eval_exp e in
	  let p = List.map deref p in
	  let res = ref [] in
	  let add x = res := join !res x in
	    List.iter add p;
	    !res
  in

  let process_stmt (e1, e2) =
    let a = eval_exp e1 in
    let p = eval_exp e2 in
      List.iter (fun x -> add_pointsto x p) a
  in

    List.iter init vars;
    changed := true;
    while !changed do
      changed := false;
      List.iter process_stmt prog
    done;
    g

let dump g =
  let dump_rel x l =
    if l <> [] then begin
      let l = List_utils.to_string (fun x -> x) ", " l in
	print_endline (x^" -> { "^l^" }")
    end
  in
    print_endline "Points-to relations:";
    Hashtbl.iter dump_rel g

let _ = 
  try
    Arg.parse speclist anon_fun usage_msg;

    if !input = "" 
    then invalid_arg ("no file specified. Try "^Sys.argv.(0)^" --help");

    let (_, prog, _) = Newspeak.read !input in
    let prog = translate prog in
      if !debug then S.print prog;
    let graph = analyse prog in
      dump graph
  with Invalid_argument s -> 
    print_endline ("Fatal error: "^s);
    exit 0
      
