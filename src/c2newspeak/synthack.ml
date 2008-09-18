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

module B = Csyntax
module C = Cir

module String_set = Set.Make(String)

type base_typ =
    | Void 
    | Integer of ikind
    | Float of int
    | Struct of (string * field list option)
    | Union of (string * field list option)
    | Name of string
    | Enum of ((string * B.exp option) list) option
    | Va_arg

and var_modifier = 
    | Abstract
    | Variable of (string * location)
    | Function of (var_modifier * decl list)
    | Array of (var_modifier * B.exp option)
    | Pointer of var_modifier

and decl = (base_typ * var_modifier)

and field = (base_typ * var_modifier * B.exp option)

type vdecl = (B.typ * string option * location)
type sdecls = (B.enumdecl list * B.compdecl list)

(** TODO: remove these globals, by putting them as argument of the lexer ??
    and then passing them through the tokens *)
let typedefs = Hashtbl.create 100
let fnames = ref String_set.empty

let init_tbls () =
  Hashtbl.clear typedefs;
  fnames := String_set.empty;
(* initialize table of predefined types *)
(* GNU C predefined types *)
  if !Npkcontext.gnuc then begin
    Hashtbl.add typedefs "_Bool" (B.Int (Newspeak.Unsigned, 1))
  end

let _ = 
  init_tbls ()

let add_fname x =
  fnames := String_set.add x !fnames

let get_fnames () = String_set.elements !fnames

let define_type x t = Hashtbl.add typedefs x t

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
	    (x, n)::(define_enum tl n')
      | [] -> []
  in
    define_enum e (B.exp_of_int 0)

let rec normalize_base_typ t =
  let sdecls =
    match t with
	Integer _ | Float _ | Void | Va_arg | Name _ | Enum None -> ([], [])
      | Struct (n, f) -> normalize_compdef (n, true, f)
      | Union (n, f) -> normalize_compdef (n, false, f)
      | Enum Some f -> (define_enum f, [])
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
	    Npkcontext.error "Synthack.normalize_base_typ" ("unknown type "^x)
	end
      | Struct (n, _) | Union (n, _) -> B.Comp n
      | Enum _ -> B.Int C.int_kind
  in
    (sdecls, t)

and normalize_compdef (n, is_struct, f) =
  match f with
      None -> ([], [])
    | Some f -> 
	let ((edecls, sdecls), f) = normalize_fields f in
	  (edecls, sdecls@(n, is_struct, f)::[])

and normalize_fields f =
  match f with
      (b, v, bits)::tl ->
	let ((edecls, sdecls), (t, x, loc)) = normalize_decl (b, v) in
	let t =
	  match (bits, t) with
	      (None, _) -> t
	    | (Some n, B.Int k) -> B.Bitfield (k, n)
	    | _ -> 
		Npkcontext.error "Synthack.normalize_field" 
		  "bit-fields allowed only with integer types"
	in
	let x = 
	  match x with
	      Some x -> x
	    | None -> "!anonymous_field"
	in
	let ((edecls', sdecls'), f) = normalize_fields tl in
	  ((edecls@edecls', sdecls@sdecls'), (t, x, loc)::f)
    | [] -> (([], []), [])

and normalize_var_modifier b v =
  match v with
      Abstract -> (b, None, Newspeak.unknown_loc)
    | Variable (x, loc) -> (b, Some x, loc)
    | Function (Variable (f, loc), args) -> 
	(B.Fun (List.map normalize_arg args, b), Some f, loc)
    | Function (Pointer v, args) -> 
	let args = List.map normalize_arg args in
	  normalize_var_modifier (B.Ptr (B.Fun (args, b))) v
    | Array (v, n) -> normalize_var_modifier (B.Array (b, n)) v
    | Pointer v -> normalize_var_modifier (B.Ptr b) v
    | Function _ -> 
	Npkcontext.error "Synthack.normalize_var_modifier" 
	  "case not implemented yet"
	  
and normalize_arg a = 
  let (symbdecls, (t, x, _)) = normalize_decl a in
  let x = 
    match x with
	Some x -> x
      | None -> "silent argument"
  in
    if (symbdecls <> ([], [])) then begin
      Npkcontext.error "Synthack.normalize_arg" 
	"symbol definition not allowed in argument"
    end;
    (t, x)

and normalize_decl (b, v) =
  let (symbdecls, t) = normalize_base_typ b in
  let d = normalize_var_modifier t v in
    (symbdecls, d)
