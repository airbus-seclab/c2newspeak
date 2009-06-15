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
  | Int of int
  | Nat of Newspeak.Nat.t

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
  | Array of t*(t list)               (* Component-indexes list *)


(***************************
 * Value (de-)constructors *
 ***************************)

let from_int t x = (t, Int x)
let from_nat t x = (t, Nat x)

let to_int = function
  | (_, Int v) -> Some v
  | _          -> None

let to_nat = function
  | (_, Nat v) -> Some v
  | _          -> None

(******************
 * Pretty-printer *
 ******************)

let print t =
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
    | Enumeration v -> "Enum (length ="^string_of_int (List.length v)^")"
    | Array (c,i) -> "Array {{ component.hash = "
                  ^p_hash c^"; indexes.hash = "
                  ^String.concat "," (List.map p_hash i)
                  ^"}}"
  in
   "{"
  ^"H="^p_hash t
  ^",parent = "^p_parent t.parent
  ^"U="^string_of_int t.uid
  ^",trait = "^p_trait t.trait
  ^"}"

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
    match (one.parent,another.parent) with
      | Some p1, Some p2 -> (p1 = p2 && one.uid = another.uid)
      | _                -> false

(*****************
 * Builtin types *
 *****************)

(* TODO hook it with Ada_config *)
let integer_first = min_int
and integer_last  = max_int

let integer  = new_constr universal_integer
                          (integer_first @.. integer_last)

let natural  = new_constr integer
                          (0 @.. integer_last)

let positive = new_constr integer
                          (1 @.. integer_last)

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
| Signed None -> raise (Invalid_argument "Type with no size")

let rec attr_get typ attr =
  match typ.trait, attr with
    | Signed (Some Range(a,_)),"first" -> from_nat typ a
    | Signed (Some Range(_,b)),"last"  -> from_nat typ b
    | Enumeration values, "first"      -> from_int typ (snd (List.hd  values))
    | Enumeration values, "last"       -> from_int typ (snd (List_utils.last
                                                                      values))
    | Array (_,ind) , ("first"|"last"|"length")  ->
        begin
          if attr="length" then
            from_nat universal_integer
              (length_of (List.hd ind ))
          else
            attr_get (List.hd ind) attr
        end
    | Float digits , "digits" -> from_int universal_integer digits
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

