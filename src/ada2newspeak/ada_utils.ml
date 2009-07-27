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

  Jasmine Duchon
  email : jasmine . duchon AT free . fr

*)


open Syntax_ada
module Nat = Newspeak.Nat
module  T  = Ada_types

exception NonStaticExpression
exception AmbiguousTypeException

type verbose_level =
  | Silent
  | Debug
  | Warning
  | Error

(** Generic error. *)
let mkerror lev modulename = match lev with
  | Silent  -> (fun _ -> ())
  | Debug   -> Npkcontext.print_debug
  | Warning -> Npkcontext.report_warning modulename
  | Error   -> Npkcontext.report_error   modulename

(* fonction propre a Ada *)

let nat_of_bool b =
  if b then Newspeak.Nat.one
       else Newspeak.Nat.zero

let ikind_of_range inf sup = (Newspeak.Signed,
                              Ada_config.size_of_range inf sup)

let check_compil_unit_name compil_unit file_name =
  let expected_name = Filename.chop_extension file_name in
  let subprog_name spec = match spec with
    | Function(name,_,_) -> name
    | Procedure(name,_) -> name in
  let (_,lib_item,_) = compil_unit in
  let name =
    match lib_item with
      | Spec(SubProgramSpec(spec))     -> subprog_name spec
      | Body(SubProgramBody(spec,_,_)) -> subprog_name spec
      | Spec(PackageSpec(name,_))   -> name
      | Body(PackageBody(name,_,_)) -> name
  in
    name = expected_name

let with_default (opt:'a option) (def_value:'a):'a = match opt with
    | None   -> def_value
    | Some x -> x

let list_to_string l to_string sep crochet =
  let r = String.concat sep (List.map to_string l) in
  if crochet then "["^r^"]" else r

let ident_list_to_string l =
  list_to_string l (fun x -> x) "." false

let name_to_string (package, ident) =
  match package with
    | None   -> ident
    | Some p -> ident_list_to_string (p::ident::[])

let make_operator_name op =
  let ada2npk_operator_prefix = "__ada2npk_operator_" in
  ada2npk_operator_prefix^
   (match op with
   | And| AndThen -> "logical_and"
   | Or | OrElse  -> "logical_and"
   | Xor          -> "xor"
   | Eq           -> "equals"
   | Neq          -> "not_equals"
   | Lt           -> "lt"
   | Le           -> "le"
   | Gt           -> "gt"
   | Ge           -> "ge"
   | Plus         -> "plus"
   | Minus        -> "minus"
   | Mult         -> "times"
   | Div          -> "div"
   | Mod          -> "mod"
   | Rem          -> "rem"
   | Power        -> "pow"
   )

let operator_of_string s = match s with
  | "and" -> And
  | "or"  -> Or
  | "xor" -> Xor
  | "="   -> Eq
  | "/="  -> Neq
  | "<"   -> Lt
  | "<="  -> Le
  | ">"   -> Gt
  | ">="  -> Ge
  | "+"   -> Plus
  | "-"   -> Minus
  | "*"   -> Mult
  | "/"   -> Div
  | "mod" -> Mod
  | "rem" -> Rem
  | "**"  -> Power
  |_ -> Npkcontext.report_error "operator_of_string"
         ("\""^s^"\" does not name an operator")

let may f = function
  | None -> None
  | Some v -> Some (f v)

let typ_to_adatyp : Syntax_ada.typ -> Ada_types.t = function
  | Integer            -> T.integer
  | IntegerConst       -> T.universal_integer
  | Float              -> T.std_float
  | Boolean            -> T.boolean
  | Character          -> T.character
  | Declared (_,_,t,_) -> t

let subtyp_to_adatyp gtbl st =
  match st with
  | Unconstrained t      -> (*T.new_unconstr *) (typ_to_adatyp t)
  | Constrained (_,_,_,t) -> t
  | SubtypName  n          -> try
                                snd(Symboltbl.find_type gtbl n)
                              with Not_found ->
                                begin
                                  Npkcontext.report_warning "ST2AT"
                                    ("Cannot find type '"
                                    ^name_to_string n
                                    ^"'");
                                  T.unknown;
                                end

let merge_types gtbl (tp, cstr, _) =
  let t = subtyp_to_adatyp gtbl tp in
  if (T.is_unknown t) then
    Npkcontext.report_warning "merge_types"
    ("merged subtype indication into unknown type ("^T.get_reason t^")");
  match cstr with
  | None -> t
  | Some (IntegerRangeConstraint (a,b)) -> T.new_constr t (T.(@...) a b)
  | Some (  FloatRangeConstraint _) -> Npkcontext.report_error "merge_types"
                                                      "FloatRangeConstraint"
  | Some (       RangeConstraint _) -> Npkcontext.report_error "merge_types"
                                                           "RangeConstraint"

