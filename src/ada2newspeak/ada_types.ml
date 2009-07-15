(*
  Ada2Newspeak: compiles Ada code into Newspeak. Newspeak is a minimal language
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

  Etienne Millon
  email: etienne.millon AT gmail . com

*)

(*********************************
 * Module definitions/inclusions *
 *********************************)

let (%+) = Newspeak.Nat.add
let (%-) = Newspeak.Nat.sub


(********************
 * Type definitions *
 ********************)

type data_t =
  | IntVal   of Newspeak.Nat.t
  | FloatVal of float
  | BoolVal  of bool

let data_eq x y =
  match (x, y) with
    |   IntVal v1,   IntVal v2 -> v1 = v2
    |  BoolVal v1,  BoolVal v2 -> v1 = v2
    | FloatVal v1, FloatVal v2 -> v1 = v2
    | _ ->
        Npkcontext.report_error "Ada_types.data_eq"
          "Incompatible types in '=' (data_t)"

let data_lt x y =
  match (x, y) with
    |   IntVal v1,   IntVal v2 -> (Newspeak.Nat.compare v1 v2) < 0
    |  BoolVal v1,  BoolVal v2 -> v1 < v2
    | FloatVal v1, FloatVal v2 -> v1 < v2
    | _ ->
        Npkcontext.report_error "Ada_types.data_lt"
          "Incompatible types in '<' (data_t)"

(* effective type + wrapped data *)
type value = t*data_t

and t = {
  parent  : t option; (* None indicates a root type *)
  trait   : trait_t;
  uid     : int;      (* Used to make derived types look different *)
}

and range =
  | NullRange
  | Range of Newspeak.Nat.t*Newspeak.Nat.t (* min <= max *)

(* type for "traits" (type of types...) *)
and trait_t =
  | Unknown
  | Signed of range option            (* Range constraint *)
  | Enumeration of (string*int) list  (* Name-index *)
  | Float of int                      (* Digits *)
  | Array of t*t                      (* Component-index *)


(******************
 * Pretty-printer *
 ******************)

let rec print t =
  let p_hash t  = Printf.sprintf "%08x" (Hashtbl.hash t) in
  let p_parent = function
    | None -> "<root>"
    | Some t -> "parent:<"^p_hash t^">"
  in
  let p_range = function
    | None      -> "<unlimited>"
    | Some NullRange     -> "{}"
    | Some (Range (a,b)) ->  "["
                            ^   Newspeak.Nat.to_string a
                            ^";"
                            ^   Newspeak.Nat.to_string b
                            ^"]"
  in
  let p_trait = function
    | Unknown -> "Unknown"
    | Signed  r -> "Signed " ^p_range r
    | Float   d -> "Float "  ^string_of_int d
    | Enumeration v -> "Enum (length = "^string_of_int (List.length v)^")"
    | Array (c,i) -> "Array {{ component = "
                  ^print c^"; index = "
                  ^print i
                  ^"}}"
  in
   "{"
  ^"H="^p_hash t
  ^",parent = "^p_parent t.parent
  ^(if t.uid =0 then "" else "U="^string_of_int t.uid)
  ^",trait = "^p_trait t.trait
  ^"}"

let print_data = function
  | IntVal   i -> "IV ("^Newspeak.Nat.to_string i^")"
  | FloatVal s -> "FV ("^string_of_float        s^")"
  | BoolVal  b -> "BV ("^string_of_bool         b^")"

(**********
 * Ranges *
 **********)

let null_range = NullRange

let (@...) min max =
  if min>max then NullRange
  else Range (min,max)

let (@..) min max =
  (Newspeak.Nat.of_int min) @... (Newspeak.Nat.of_int max)

let sizeof = function
  | NullRange       -> Newspeak.Nat.zero
  | Range (min,max) -> (max %- min %+ Newspeak.Nat.one)

(*****************
 * Symbol tables *
 *****************)

(*
 * Create a new type uid.
 * /!\ Side-effects.
 *)
let uid = object
  val mutable count = 0;
  method gen =
    count <- count + 1;
    count
end

(*
 * The minimal data for a type.
 * For declaring a new type (inside constructors...), one should write
 * [{type_stub with ..}]
 *)
let type_stub = {
  parent  = None;
  uid     = 0;
  trait   = Signed None;
}

let unknown = {type_stub with trait = Unknown}

let universal_integer = { type_stub with trait = Signed None; }

let universal_real = { type_stub with trait = Float 100; }

let new_enumerated values =
    let rec with_indices vals offset = match vals with
    |   []  -> []
    | n::tl -> (n,offset)::with_indices tl (offset+1)
    in
    let ivalues = with_indices values 0 in
    {
      type_stub with
      trait = Enumeration ivalues;
    }

let new_derived old =
    { old with uid = uid#gen }

(* Parent of parent of parent.  *)
let rec root_parent typ =
    match typ.parent with
        | None -> typ
        | Some p -> root_parent p

let new_unconstr parent =
    {
      parent = Some (root_parent parent);
      trait  = Signed None;
      uid = parent.uid;
    }

let new_constr parent r =
    {
      parent = Some (root_parent parent);
      trait = Signed (Some r);
      uid = parent.uid;
    }

let new_range r =
  new_constr universal_integer r

let new_float digits =
    {
      type_stub with
      trait = Float digits
    }

let new_array component ind =
    {
      type_stub with
      trait = Array (component,ind)
    }

let is_compatible one another =
  let p1 = root_parent one     in
  let p2 = root_parent another in
       (p1 = p2 && one.uid = another.uid)
    || (match (one.trait, another.trait) with
          | Signed  None   , Signed (Some _) -> true
          | Signed (Some _), Signed  None    -> true
          | Float   100    , Float _         -> true
          | Float     _    , Float 100       -> true
          | _ -> false)

(*****************
 * Builtin types *
 *****************)

(* TODO hook it with Ada_config *)
let integer_first = min_int
and integer_last  = max_int

let integer  = new_constr universal_integer
                          (integer_first @.. integer_last)

let boolean = new_enumerated ["true" ; "false"]

let std_float = new_float 6

(*
 * This declaration is special because we don't want to make the character
 * litterals (named __char_nnn) public.
 * TODO : make them public with a reserved name like __ada2npk_char_nnn and at
 * parse time change 'x' to the correct variable lookup.
 *)
let character =
  let rec all_bytes_from x =
    if x<0 then []
    else x::all_bytes_from (x-1)
    in
  new_enumerated (List.rev_map
                  (fun x -> "__char_"^(string_of_int x))
                  (all_bytes_from 255)
                 )

(* Number of values in a type *)
let length_of typ = match typ.trait with
| Signed (Some r) -> sizeof r
| Enumeration vals -> Newspeak.Nat.of_int (List.length vals)
| Array _
| Float _
| Unknown
| Signed None -> Npkcontext.report_error "length_of" "Type with no size"

let rec attr_get typ attr =
  match typ.trait, attr with
    | Signed (Some Range(a,_)),"first" -> typ, IntVal a
    | Signed (Some Range(_,b)),"last"  -> typ, IntVal b
    | Enumeration values, "first"      -> typ, IntVal (Newspeak.Nat.of_int (snd
                                                             (List.hd  values)))
    | Enumeration values, "last"       -> typ, IntVal (Newspeak.Nat.of_int (snd
                                                      (List_utils.last values)))
    | Array (_,ind) , ("first"|"last"|"length")  ->
        begin
          if attr="length" then
            universal_integer, IntVal (length_of ind)
          else
            attr_get ind attr
        end
    | Float digits , "digits" -> universal_integer,
                                    IntVal (Newspeak.Nat.of_int digits)
    | Unknown, _
    | Array _, _
    | Float _ , _
    | Enumeration _ , _
    | Signed (Some Range(_,_)),  _
    | Signed (None)           ,  _
    | Signed (Some NullRange) ,  _
                 -> raise ( Invalid_argument ("No such attribute : '"^attr^"'"))

(****************
 * Traits tests *
 ****************)

let is_boolean typ =
  root_parent typ == boolean

let is_integer typ =
  match typ.trait with
  | Unknown       -> false
  | Array       _ -> false
  | Enumeration _ -> false
  | Float       _ -> false
  | Signed      _ -> true

let is_discrete typ =
  match typ.trait with
  | Unknown       -> false
  | Array       _ -> false
  | Enumeration _ -> true
  | Float       _ -> false
  | Signed      _ -> true

let is_numeric typ =
  match typ.trait with
  | Unknown       -> false
  | Array       _ -> false
  | Enumeration _ -> false
  | Float       _ -> true
  | Signed      _ -> true

let is_scalar typ =
  match typ.trait with
  | Unknown       -> false
  | Array       _ -> false
  | Float       _ -> true
  | Enumeration _ -> true
  | Signed      _ -> true

let is_float typ =
  match typ.trait with
  | Unknown       -> false
  | Array       _ -> false
  | Float       _ -> true
  | Enumeration _ -> false
  | Signed      _ -> false

let is_unknown typ =
  typ.trait = Unknown

(****************
 *  Translator  *
 ****************)

let minimal_size _a _b =
  32 (* FIXME *)

let float_size d =
  if d < 6 then
    Npkcontext.report_error "translate"
      "'digits attribute too small for floating point type"
  else if (d < 11) then
    16
  else
    32

let rec translate t =
  let rec translate_trait = function
  | Signed (Some Range (a,b)) -> Cir.Scalar (Newspeak.Int (Newspeak.Signed,minimal_size a b))
  | Enumeration      v        -> translate_trait (Signed(
                                    Some(Range ( Newspeak.Nat.one
                                               , Newspeak.Nat.of_int (List.length v)))))
  | Float            d        -> Cir.Scalar (Newspeak.Float (float_size d))
  | Array          (c,i)      -> Cir.Array  (translate c, Some (Newspeak.Nat.to_int (length_of i)))
  | Unknown                   -> Npkcontext.report_error "Ada_types.translate"
                                     "Type of unknown trait remaining at translate time"
  | Signed (None|Some NullRange) ->  Npkcontext.report_error "Ada_types.translate"
                    "Trying to translate <Signed None>"
  in translate_trait t.trait
  
(**
 * Subprogram parameters.
 *)

type f_param = { fp_name : string
               ; fp_in   : bool
               ; fp_out  : bool
               ; fp_type : t
               }

let   to_fparam (a,b,c,d) = {  fp_name = a;fp_in = b;fp_out = c;fp_type = d}
let from_fparam     f     = (f.fp_name , f.fp_in , f.fp_out , f.fp_type)
