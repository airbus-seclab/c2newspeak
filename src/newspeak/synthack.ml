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

(* TODO: this is a bit of a hack... *)
let undefined = "!undefined"

open Newspeak

module B = Bare_csyntax
module C = Csyntax

type base_typ =
    | Void 
    | Integer of ikind
    | Float of int
    | Struct of (string * decl list option)
    | Union of (string * decl list option)
    | Name of string
    | Enum of (string * Int64.t option) list option

and var_modifier = 
    | Abstract
    | Variable of (string * location)
(* true if variable argument list *)
    | Function of (var_modifier * decl list * bool)
    | Array of (var_modifier * B.exp option)
    | Pointer of var_modifier

and decl = (base_typ * var_modifier)

let typedefs = Hashtbl.create 100
let enumdefs = Hashtbl.create 100
let compdefs = ref []

let get_compdefs () = 
  let res = List.rev !compdefs in
    Hashtbl.clear typedefs;
    Hashtbl.clear enumdefs;
    compdefs := [];
    res

let define_type x t = Hashtbl.add typedefs x t

let is_type x = Hashtbl.mem typedefs x

let define_comp n is_struct f = 
  compdefs := (n, is_struct, f)::(!compdefs)

let define_enum e =
  let rec define_enum e n =
    match e with
	(x, v)::tl ->
	  if Hashtbl.mem enumdefs x then begin
	    Npkcontext.error "Synthack.define_enum"
	      ("Enum "^x^" already defined")
	  end;
	  let n = 
	    match v with
		None -> n
	      | Some n -> n
	  in
	  Hashtbl.add enumdefs x n;
	  define_enum tl (Int64.succ n)
      | [] -> ()
  in
    define_enum e Int64.zero

let find_enum x = Hashtbl.find enumdefs x

let int64_to_int x =
  if Int64.compare x (Int64.of_int max_int) > 0 
  then Npkcontext.error "Firstpass.int64_to_int" "integer too big";
  if Int64.compare x (Int64.of_int max_int) > 0 
  then Npkcontext.error "Firstpass.int64_to_int" "expecting positive integer";
  Int64.to_int x

let rec normalize_base_typ t =
  match t with
      Integer k -> B.Int k
    | Float n -> B.Float n
    | Struct (n, f) -> 
	normalize_compdef n true f;
	B.Struct n
    | Union (n, f) -> 
	normalize_compdef n false f;
	B.Union n
    | Void -> B.Void
    | Enum f ->
	begin match f with
	    None -> ()
	  | Some f -> define_enum f
	end; 
	B.Int C.int_kind
    | Name x -> 
	try Hashtbl.find typedefs x
	with Not_found ->
	  Npkcontext.error "Synthack.normalize_base_typ" ("Unknown type "^x)

and normalize_compdef n is_struct f =
  match f with
      None -> ()
    | Some f -> 
	let f = List.map normalize_arg f in
	  define_comp n is_struct f

and normalize_var_modifier b v =
  match v with
      Abstract -> (b, undefined, Newspeak.dummy_loc "")
    | Variable (x, loc) -> (b, x, loc)
    | Function (Variable (f, loc), args, va_list) -> 
	(B.Fun (List.map normalize_arg args, va_list, b), f, loc)
    | Function (Pointer v, args, va_list) -> 
	let args = List.map normalize_arg args in
	  normalize_var_modifier (B.Ptr (B.Fun (args, va_list, b))) v
    | Array (v, n) -> normalize_var_modifier (B.Array (b, n)) v
    | Pointer v -> normalize_var_modifier (B.Ptr b) v
    | Function _ -> 
	Npkcontext.error "Synthack.normalize_var_modifier" 
	  "case not implemented yet"
	  
and normalize_arg a = 
  let (t, x, _) = normalize_decl a in
    (t, x)

and normalize_decl (b, v) =
  let t = normalize_base_typ b in
    normalize_var_modifier t v
