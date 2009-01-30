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

type prog = (global * location) list * assertion list

and assertion = spec_token list

and spec_token = 
  | SymbolToken of char
  | IdentToken of string
  | LvalToken of exp
  | CstToken of cst
      
and global = 
    (* true if static *)
  | FunctionDef of (string * typ * bool * blk)
      (* true for extern *)
  | GlbVDecl of vardecl
      (* enum declaration *)
  | GlbEDecl of enumdecl
(* struct or union: composite *)
  | GlbCDecl of compdecl
  
and enumdecl = string * exp

and compdecl = string * bool * declaration list

and extern = bool

and vardecl = string * typ * static * extern * init option

and declaration = (typ * string * location)

and ftyp = (typ * string) list option * typ

and typ =
  | Void
  | Int of ikind
  | Bitfield of (ikind * exp)
  | Float of int
  | Ptr of typ
  | Array of (typ * exp option)
(* true for structure *)
  | Comp of (string * bool)
  | Fun of ftyp
  | Va_arg
  | Typeof of string
      
and init = 
  | Data of exp
  | Sequence of (string option * init) list

and stmt = (stmtkind * location)

and blk = stmt list

and stmtkind =
    EDecl of enumdecl
  | CDecl of compdecl
  | VDecl of vardecl
  | If of (exp * blk * blk)
  | CSwitch of (exp * (exp * blk * location) list * blk)
      (* init, while exp is true do blk and then blk, 
	 continue jumps before the second blk 
	 init may cotain break or continue stmt!
      *)
  | For of (blk * exp * blk * blk)
  | Exp of exp
  | Break
  | Continue
  | Return of exp option
  | Block of blk
  | Goto of lbl
  | Label of lbl
  | UserSpec of assertion

and lbl = string

and static = bool

and exp = 
    | Cst of cst
    | Var of string
    | Field of (exp * string)
    | Index of (exp * exp)
    | Deref of exp
    | AddrOf of exp
    | Unop of (unop * exp)
    | IfExp of (exp * exp * exp)
    | Binop of (binop * exp * exp)
    | Call of (exp * exp list)
    | Sizeof of typ
    | SizeofE of exp
    | Str of string
    | FunName
    | Cast of (exp * typ)
(* None is a regular assignment *)
    | Set of (exp * binop option * exp)
(* boolean is true if the operation is appled after the evaluation of the 
   expression *)
    | OpExp of (binop * exp * bool)
    | BlkExp of blk

and cst = (Cir.cst * typ)

and unop = Neg | Not | BNot

and binop =
    | Plus
    | Minus
    | Mult
    | Div
    | Mod
    | Gt
    | Eq
    | BAnd
    | BXor
    | BOr
    | Shiftl
    | Shiftr

let char_kind = (Signed, Config.size_of_char)

let char_typ = Int char_kind

let int_typ = Int (Signed, Config.size_of_int)

let long_typ = Int (Signed, Config.size_of_long)

let uint_typ = Int (Unsigned, Config.size_of_int)

let exp_of_int i = Cst (Cir.CInt (Nat.of_int i), int_typ)

let exp_of_char c = Cst (Cir.CInt (Nat.of_int (Char.code c)), char_typ)

let nat_of_lexeme base x =
  let read_digit c = (int_of_char c) - (int_of_char '0') in
  let read_hex_digit c =
    if ('0' <= c) && (c <= '9') then (int_of_char c) - (int_of_char '0')
    else if  ('a' <= c) && (c <= 'f') 
    then (int_of_char c) - (int_of_char 'a') + 10
    else (int_of_char c) - (int_of_char 'A') + 10
  in
  let (read_digit, base) =
    match base with
	None -> (read_digit, 10)
      | Some "0" -> (read_digit, 8)
      | Some "0x" -> (read_hex_digit, 16)
      | _ -> Npkcontext.report_error "Csyntax.nat_of_lexeme" "invalid base"
  in
  let v = ref Nat.zero in
  let add_digit c =
    let d = read_digit c in
      v := Nat.mul_int base !v;
      v := Nat.add_int d !v
  in
    String.iter add_digit x;
    !v

let ikind_tbl =
  [(Signed, Config.size_of_int); (Unsigned, Config.size_of_int); 
   (Signed, Config.size_of_long); (Unsigned, Config.size_of_long); 
   (Signed, Config.size_of_longlong); (Unsigned, Config.size_of_longlong)
  ]    

(* See C standard ANSI 6.4.4 *)
let int_cst_of_lexeme (base, x, sign, min_sz) = 
  let x = nat_of_lexeme base x in
  let possible_signs = 
    match (base, sign) with
(* TODO: not in conformance with standard. strange *)
	(None, None) -> [Signed; Unsigned]
      | (Some _, None) -> [Signed; Unsigned]
      | (_, Some ('u'|'U')) -> Unsigned::[]
      | _ -> 
	  Npkcontext.report_error "Csyntax.int_cst_of_lexeme" 
	    "unreachable statement"
  in
  let min_sz =
    match min_sz with
	None -> Config.size_of_int
      | Some ("L"|"l") -> Config.size_of_long
      | Some "LL" -> Config.size_of_longlong
      | _ -> 
	  Npkcontext.report_error "Csyntax.int_cst_of_lexeme" 
	    "unreachable statement"
  in
  let is_kind (sign, sz) =
    ((sz >= min_sz)
      && (List.mem sign possible_signs)
      && (Newspeak.belongs x (Newspeak.domain_of_typ (sign, sz))))
  in
  let k = List.find is_kind ikind_tbl in
    (Cir.CInt x, Int k)

let char_cst_of_lexeme x = (Cir.CInt (Nat.of_int x), char_typ)

let comp_of_typ t =
  match t with
      Comp (n, _)-> n
    | _ -> 
	Npkcontext.report_error "Csyntax.comp_of_typ" 
	  "struct or union type expected"

(* ANSI C: 6.4.4.2 *)
let float_cst_of_lexeme (value, suffix) =
  let f = 
    try float_of_string value 
    with Failure "float_of_string" -> 
      Npkcontext.report_error "Csyntax.float_cst_of_lexeme" 
	"float not representable"
  in
(* TODO: should really think about floating points, I don't know whether it
   is really necessary to keep the suffix on the bare string representation of
   the float??? *)
  let (lexeme, sz) = 
    match suffix with
	None -> (value, Config.size_of_double)
      | Some 'F' -> (value^"F", Config.size_of_float)
      | _ -> 
	  Npkcontext.report_error "Csyntax.float_cst_of_lexeme" 
	    "unknown suffix for float"
  in
    (Cir.CFloat (f, lexeme), Float sz)

let rec string_of_typ t =
  match t with
      Void -> "void"
    | Int _ -> "int??"
    | Ptr t -> "*"^(string_of_typ t)
    | Array (t, None) -> (string_of_typ t)^"[?]"
    | Array (t, Some x) -> (string_of_typ t)^"["^(string_of_exp x)^"]"
    | Bitfield _ -> "Bitfield"
    | Float _ -> "Float"
    | Comp _ -> "Comp"
    | Fun _ -> "Fun"
    | Va_arg -> "..."
    | Typeof _ -> "typeof"

and string_of_exp e =
  match e with
      Cst _ -> "Cst"
    | Var x -> x
    | Field (e, f) -> (string_of_exp e)^"."^f
    | Index (e1, e2) -> "("^(string_of_exp e1)^")["^(string_of_exp e2)^"]"
    | Deref e -> "*("^(string_of_exp e)^")"
    | AddrOf _ -> "AddrOf"
    | Unop _ -> "Unop"
    | IfExp _ -> "IfExp"
    | Binop _ -> "Binop"
    | Call _ -> "Call"
    | Sizeof _ -> "Sizeof"
    | SizeofE _ -> "SizeofE"
    | Str _ -> "Str"
    | FunName -> "FunName"
    | Cast _ -> "Cast"
    | Set _ -> "Set"
    | OpExp _ -> "OpExp"
    | BlkExp _ -> "BlkExp"

let string_of_ftyp (args_t, _) =
  let string_of_arg (t, _) = string_of_typ t in
  let args =
    match args_t with
	None -> "? -> ret_t"
      | Some args_t -> List_utils.to_string string_of_arg ", " args_t
  in
    "("^args^") -> ret_t"

let ftyp_of_typ t =
  match t with
      Fun t -> t
    | _ -> 
	Npkcontext.report_error "Csyntax.ftyp_of_typ" "function type expected"

let array_of_typ t =
  match t with
      Array a -> a
    | _ -> Npkcontext.report_error "Csyntax.array_of_typ" "array type expected"

let min_ftyp (args_t1, ret_t1) (args_t2, ret_t2) =  
  let equals (t1, _) (t2, _) = t1 = t2 in
(* TODO???
  let equals (t1, _) (t2, _) =  
    match (t1, t2) with  
      | (Ptr Fun _, Ptr Fun _) -> true  
      | (Ptr _, Ptr _) -> true  
      | _ -> t1 = t2  
  in  
    *)
  let args_t =  
    match (args_t1, args_t2) with  
        (None, args_t) | (args_t, None) -> args_t  
      | (Some args_t1, Some args_t2) ->
          if not (List.for_all2 equals args_t1 args_t2) then begin
            Npkcontext.report_error "Csyntax.min_ftyp"
              "different argument types for function"
          end;
          Some args_t1
  in
    if (ret_t1 <> ret_t2) then begin
      Npkcontext.report_error "Csyntax.min_ftyp" 
	"different return types for function"
    end;
    (args_t, ret_t1)  
     
