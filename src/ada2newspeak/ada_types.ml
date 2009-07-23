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
  Newspeak.Nat.t*Newspeak.Nat.t (* min <= max *)

(* type for "traits" (type of types...) *)
and trait_t =
  | Unknown     of string            (* Reason *)
  | Signed      of range option      (* Range constraint *)
  | Float       of int               (* Digits           *)
  | Array       of t*t               (* Component-index  *)
  | Record      of (string * t) list (* Fields           *)
  | Enumeration of (string*int) list (* Name-index       *)

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
    | Some (a,b) ->  "["
                   ^ Newspeak.Nat.to_string a
                   ^ ";"
                   ^ Newspeak.Nat.to_string b
                   ^ "]"
  in
  let p_trait = function
    | Unknown _ -> "Unknown"
    | Signed  r -> "Signed " ^p_range r
    | Float   d -> "Float "  ^string_of_int d
    | Enumeration v -> "Enum (length = "^string_of_int (List.length v)^")"
    | Array (c,i) -> "Array {{ component = "
                  ^print c^"; index = "
                  ^print i
                  ^"}}"
    | Record flds -> "Record {"^String.concat ", " (List.map fst flds)^"}"
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

let sizeof (min,max) =
  max %- min %+ Newspeak.Nat.one

let (@...) x y = (x, y)

let (@..)  x y = ( Newspeak.Nat.of_int x
                 , Newspeak.Nat.of_int y)

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


let mk_unknown reason = {type_stub with trait = Unknown reason}

let unknown = mk_unknown "<No reason provided>"

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

let new_record fields =
  { type_stub with
    trait = Record fields
  }

let handle_representation_clause _t _l =
  invalid_arg ("handle_representaion_clause")

let extract_array_types t =
  match t.trait with
  | Array (c, i) -> Some (c,i)
  | _            -> None

let get_reason t =
  match t.trait with
  | Unknown r -> r
  | _ -> invalid_arg "get_reason"

(*****************
 * Builtin types *
 *****************)

let integer_first = Newspeak.Nat.of_string "-2147483648"
let integer_last  = Newspeak.Nat.of_string  "2147483647"

let integer  = new_constr universal_integer (integer_first, integer_last)

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

(****************
 * Traits tests *
 ****************)

let is_boolean typ =
  root_parent typ == boolean

let is_integer typ =
  match typ.trait with
  | Unknown     _ -> false
  | Array       _ -> false
  | Record      _ -> false
  | Enumeration _ -> false
  | Float       _ -> false
  | Signed      _ -> true

let is_discrete typ =
  match typ.trait with
  | Unknown     _ -> false
  | Array       _ -> false
  | Record      _ -> false
  | Enumeration _ -> true
  | Float       _ -> false
  | Signed      _ -> true

let is_numeric typ =
  match typ.trait with
  | Unknown     _ -> false
  | Array       _ -> false
  | Record      _ -> false
  | Enumeration _ -> false
  | Float       _ -> true
  | Signed      _ -> true

let is_scalar typ =
  match typ.trait with
  | Unknown     _ -> false
  | Array       _ -> false
  | Record      _ -> false
  | Float       _ -> true
  | Enumeration _ -> true
  | Signed      _ -> true

let is_float typ =
  match typ.trait with
  | Unknown     _ -> false
  | Array       _ -> false
  | Record      _ -> false
  | Float       _ -> true
  | Enumeration _ -> false
  | Signed      _ -> false

let is_unknown typ =
  match typ.trait with
  | Unknown     _ -> true
  | Array       _ -> false
  | Record      _ -> false
  | Float       _ -> false
  | Enumeration _ -> false
  | Signed      _ -> false

(* Number of values in a type *)
let length_of typ = match typ.trait with
| Signed (Some r) -> sizeof r
| Enumeration vals -> Newspeak.Nat.of_int (List.length vals)
| Array _
| Record _
| Float _
| Unknown _
| Signed None -> Npkcontext.report_error "length_of" "Type with no size"

let rec attr_get typ attr =
  match typ.trait, attr with
    | Signed (Some (a,_)),"first" -> typ, IntVal a
    | Signed (Some (_,b)),"last"  -> typ, IntVal b
    | Enumeration values, "first" -> typ, IntVal (Newspeak.Nat.of_int (snd
                                                    (List.hd  values)))
    | Enumeration values, "last"  -> typ, IntVal (Newspeak.Nat.of_int (snd
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
    | Float _ , "safe_small"  -> universal_real, FloatVal (min_float)
    | Float _ , "safe_large"  -> universal_real, FloatVal (max_float)
    | _ , "succ" when is_scalar typ -> failwith "succ"
    | Unknown     _ , _
    | Array       _ , _
    | Record      _ , _
    | Float       _ , _
    | Enumeration _ , _
    | Signed      _ , _
                 -> raise ( Invalid_argument ("No such attribute : '"^attr^"'"))

(****************
 *  Translator  *
 ****************)

(**
   Minimal integer n such that
       -(2^(n-1)) <= a <= b <= 2^(n-1) - 1
 *)
let minimal_size_signed a b =
  let (i,two_pow_i) = ref 0, ref Newspeak.Nat.one in
  let finished _ =
       (Newspeak.Nat.compare (Newspeak.Nat.neg (!two_pow_i))
                             a  <= 0)
    && (Newspeak.Nat.compare b (Newspeak.Nat.add_int (-1) (!two_pow_i)) <= 0)
  in
  while (not (finished ())) do
    incr i;
    two_pow_i := Newspeak.Nat.mul_int 2 !two_pow_i;
  done;
  !i + 1

(**
   Minimal integer n such that
       0 <= b <= 2^n - 1
 *)
let minimal_size_unsigned b =
  let (i,two_pow_i) = ref 0, ref 1 in
  let finished _ =
    b <= !two_pow_i - 1
  in
  while (not (finished ())) do
    incr i;
    two_pow_i := 2 * !two_pow_i;
  done;
  !i

let float_size _d =
  (* FIXME *)
  32

let rec translate t =
  let rec translate_trait = function
  | Signed (Some (a,b)) -> Cir.Scalar
                             (Newspeak.Int
                               ( Newspeak.Signed
                               , minimal_size_signed a b
                               ))
  | Enumeration    v    -> Cir.Scalar
                            (Newspeak.Int
                              ( Newspeak.Unsigned
                              , minimal_size_unsigned
                                 (snd (List_utils.last v))))
  | Float          d    -> Cir.Scalar (Newspeak.Float (float_size d))
  | Array        (c,i)  -> Cir.Array  ( translate c
                                      , Some (Newspeak.Nat.to_int
                                                        (length_of i)))
  | Record        flds  ->
      let build_offset (cflds, start_off) (id, st) =
        let ctyp = translate st in
          (id, (start_off, ctyp))::cflds, (start_off + Cir.size_of_typ ctyp)
      in
        Cir.Struct (List.fold_left build_offset ([], 0) flds)
  | Unknown _ -> Npkcontext.report_error "Ada_types.translate"
               "Type of unknown trait remaining at translate time"
  | Signed None ->  Npkcontext.print_debug "translating univ_int as Integer";
                    translate_trait (integer.trait)
  in translate_trait t.trait

let check_exp t exp =
  match t.trait with
  | Signed (Some (a,b)) ->
      Cir.Unop ( Npkil.Belongs_tmp (a, Npkil.Known (Newspeak.Nat.add_int 1 b))
               , exp
               )
  | _ -> exp

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

let coerce_types a b = match (a.trait,b.trait) with
  | Signed (Some _), Signed None -> a
  | Signed None, Signed (Some _) -> b
  | _ -> a

let is_compatible one another =
  if (is_unknown one || is_unknown another) then
    Npkcontext.report_warning "is_compatible"
        "testing compatibility against unknown type";
  let p1 = root_parent one     in
  let p2 = root_parent another in
       (p1 = p2 && one.uid = another.uid)
    || (match (one.trait, another.trait) with
          | Signed  None   , Signed (Some _) -> true
          | Signed (Some _), Signed  None    -> true
          | Float   100    , Float _         -> true
          | Float     _    , Float 100       -> true
          | _ -> false)
