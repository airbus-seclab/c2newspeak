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
open BareSyntax

module B = Csyntax
module C = Cir

(** TODO: remove these globals, by putting them as argument of the lexer ??
    and then passing them through the tokens *)
(* TODO: simplify, the actual value of the type should not be needed!! *)
let typedefs = Hashtbl.create 100

(* TODO: not nice, try to remove this init_tbl!!! and the global typedefs *)
let init_tbls () =
  Hashtbl.clear typedefs;
(* initialize table of predefined types *)
(* GNU C predefined types *)
(* TODO: clean up put in gnuc.ml and think about architecture *)
  if !Npkcontext.accept_gnuc 
  then Hashtbl.add typedefs "_Bool" (B.Int (Newspeak.Unsigned, 1))

(* TODO: think about this, is this call necessary?? *)
let _ = 
  init_tbls ()

(* TODO: try to remove this syntactic hack by rewriting the parser,
   but is it even possible? *)
(* TODO: this is a dummy type, because it should not be necessary => remove alltogether *)
let define_type x = Hashtbl.add typedefs x (B.Int (Newspeak.Unsigned, 1))

(* TODO: dead code? *)
let is_type x = Hashtbl.mem typedefs x

let define_enum e =
  let rec define_enum e n =
    match e with
	(x, v)::tl ->
	  let n = 
	    match v with
		None -> n
	      | Some n -> n
	  in
	  let n' = B.Binop (B.Plus, n, B.exp_of_int 1) in
	    (x, B.EDecl n)::(define_enum tl n')
      | [] -> []
  in
    define_enum e (B.exp_of_int 0)

let rec normalize_base_typ t =
  let _ =
    match t with
	Integer _ | Float _ | Void | Va_arg | Name _ | Enum None 
      | Typeof _ -> []
      | Composite v -> normalize_compdef v
      | Enum Some _f -> []
  in
  let t = 
    match t with
	Integer k -> B.Int k
      | Float n -> B.Float n
      | Void -> B.Void
      | Va_arg -> B.Va_arg
      | Name x -> begin
	  try Hashtbl.find typedefs x
	  with Not_found -> 
	    Npkcontext.report_error "Synthack.normalize_base_typ" 
	      ("unknown type "^x)
	end
      | Composite (_, (n, _)) -> B.Comp n
      | Typeof v -> B.Typeof (B.Var v)
      | Enum _ -> B.Int C.int_kind
  in
    t

and normalize_compdef (_, (_, f)) =
  match f with
      None -> []
    | Some f -> 
	let _ = normalize_fields f in
	  []

and normalize_fields f =
  match f with
      (b, v, _)::tl ->
	let x = normalize_decl (b, v) in
	let x = 
	  match x with
	      Some x -> x
	    | None -> "!anonymous_field"
	in
	let f = normalize_fields tl in
	  x::f
    | [] -> []

and normalize_var_modifier b (_, v) =
  match v with
      Abstract -> None
    | Variable (x, _) -> Some x
    | Function (x, _) -> normalize_var_modifier (B.Fun (None, b)) x
	    (* TODO: this value is incorrect, because it is unused => remove alltogether *)
    | Array (v, _) -> normalize_var_modifier b v
	(*normalize_var_modifier (B.Array (b, n)) v*)

and normalize_decl (b, v) =
  let t = normalize_base_typ b in
    normalize_var_modifier t v
