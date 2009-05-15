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

let error x =
  if 0=1 then
    Npkcontext.report_warning "Ada_types" x

(**
 * The [string] type with primitives to
 * handle it in a case-insensitive way.
 *)
module CaseInsensitiveString =
  struct
    type t = string list*string

    let equal_s s1 s2 =
      String.compare (String.lowercase s1) (String.lowercase s2) = 0

    let equal (p1,i1) (p2,i2) =
      equal_s i1 i2 &&
      try List.for_all2 equal_s p1 p2
      with Invalid_argument _ -> false

    let hash_s s =
      Hashtbl.hash (String.lowercase s)

    let hash (p,i) =
      List.fold_left (fun x y -> x+hash_s y) (hash_s i) p

    let to_string x = x
  end

(** A (string list*string) hash table insensitive to keys' case.  *)
module IHashtbl = Hashtbl.Make(CaseInsensitiveString)

(**
 * A universal type.
 *
 * It is used to make an heterogeneous container by wrapping polymorphism into
 * itself, much like enum+union structures or Object containers in Java (before
 * generics).
 *
 * Each call to [embed ()] yields a couple of functions [inject,project] :
 *   - [inject x] creates a [Univ.t] wrapped value from data [x], which may
 *   have any type.
 *   - [project u] is used to unwrap the value back. It returns an option type ;
 *   more precisely, [None] is used to indicate that a bad unwrapper has been
 *   used to retrive the wrapped value.
 *
 * The current implementation is not thread-safe and may create memory leaks.
 *)
module Univ :
sig
  type t

  val embed: unit -> ('a -> t) * (t -> 'a option)
end = struct
  type t = {
    id : unit ref;
    store : unit -> unit;
  }

  let embed () =
    let id = ref () in
    let r = ref None in
    let put a =
      let o = Some a in
      { id = id; store = (fun () -> r := o); }
      in
      let get t =
        if id == t.id then (t.store
        (); let a = !r in r := None;
        a) else None
        in
        (put, get)
end

(********************
 * Type definitions *
 ********************)

(* effective type + wrapped data *)
type value = t*Univ.t

and t = {
  parent  : t option; (* None indicates a root type *)
  trait   : trait_t;
  uid     : int;      (* Used to make derived types look different *)
  limited : bool;
}

and range =
  | NullRange
  | Range of Newspeak.Nat.t*Newspeak.Nat.t (* min <= max *)

(* type for "traits" (type of types...) *)
and trait_t =
  | Signed of range option            (* Range constraint *)
  | Modular of int                    (* Modulus *)
  | Enumeration of (string*int) list  (* Name-index *)
  | Float of int                      (* Digits *)
  | Array of t*(t list)               (* Component-indexes list *)

(*
 * A symbol table.
 * t_var and t_type hold respectively data related
 * to variables and returning their type and to types.
 *
 * TODO : However it is not type-safe, as different information is typed the same way.
 *)
type table = {
  t_var  : t IHashtbl.t;
  t_type : t IHashtbl.t;
}

(*********************************
 * Value injector and projectors *
 *********************************)

let (inject_int ,project_int )  = Univ.embed ()
let (inject_bool,project_bool)  = Univ.embed ()
let (inject_nat ,project_nat )  = Univ.embed ()

let from_int  typ ival = typ,(inject_int  ival)
and to_int  v          = project_int  (snd v)

let from_bool typ ival = typ,(inject_bool ival)
and to_bool v          = project_bool (snd v)

let from_nat  typ ival = typ,(inject_nat  ival)
and to_nat  v          = project_nat  (snd v)

(******************
 * Pretty-printer *
 ******************)

let print t =
  let p_hash t  = Printf.sprintf "%08x" (Hashtbl.hash t) in
  let p_limited x = if x then "Limited," else "" in
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
    | Signed  r -> "Signed " ^p_range r
    | Modular m -> "Modular "^string_of_int m
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
  ^p_limited t.limited
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

let print_table tbl =
  let pad width str =
    let x = String.length str in
    let b = (width - x) / 2   in
    let a =  width - x - b    in
    (String.make a ' ')^str^(String.make b ' ')
  in
  let line_printer t (p,i) s_t =
    List.iter print_string
    ["|"
    ;pad 6 t
    ;"|"
    ;pad 15 (String.concat "." (p@[i]))
    ;"| "
    ;print s_t
    ];
    print_newline ()
  in
  print_endline "+--------------------------- . . .";
  print_endline "|           Symbol table          ";
  print_endline "+------+---------------+---- . . .";
  print_endline "| type |      name     |  Contents";
  print_endline "+------+---------------+---- . . .";
  IHashtbl.iter (line_printer "Var") tbl.t_var;
  IHashtbl.iter (line_printer "Typ") tbl.t_type;
  print_endline "+------+---------------+---- . . .";
  print_newline ()

let create_table size = {t_var  = IHashtbl.create size;
                         t_type = IHashtbl.create size;
                        }

(*
 * Private global symbol table.
 * It is made available to the rest of the world by
 * builtin_type and builtin_variable. *)
let builtin_table :table = create_table 10

let add_variable tbl package id v   =
  let descr p =
    if p = [] then "as a local variable"
    else "in package "^String.concat "." p
  in
  Npkcontext.print_debug ("SYMBTBL --> adding variable "
    ^id
    ^" "^descr package);
  IHashtbl.add tbl.t_var (package,id) (fst v)

let add_type tbl package id typ =
  Npkcontext.print_debug ("SYMBTBL --> adding type "^id);
  IHashtbl.add tbl.t_type (package,id) typ

let remove_type tbl id = IHashtbl.remove tbl.t_type id

let find_information hashtbl ?context package id =
  if package != [] then IHashtbl.find hashtbl (package,id)
  else (* no package : try it as a local variable,
        *              or within current package
        *              (if provided)
        *)
    try IHashtbl.find hashtbl ([],id)
    with Not_found ->
      begin
        match context with
        | None   -> raise Not_found
        | Some package_list ->
          let p = List.find (fun pkg -> IHashtbl.mem hashtbl (pkg,id))
                            package_list
            (* List.find throws Not_found if     *
             * no package matches the predicate. *
             * This is the expected behaviour.   *)
          in
          IHashtbl.find hashtbl (p,id)
      end

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
  limited = false;
}

let universal_integer = { type_stub with trait = Signed None; }

let universal_real = { type_stub with trait = Float 100; }


let find_type     tbl = find_information tbl.t_type

let find_variable tbl ?context package id =
  try find_information tbl.t_var ?context package id
  with Not_found -> begin
                      error ("Cannot find variable "^id);
                      universal_integer
                    end

(*
 * Add (or not) a type to a symbol table.
 *)
let maybe_add ?symboltable ?name typ =
  begin match symboltable,name with
  | None    ,None   -> ()
  | Some _  ,None   -> raise (Invalid_argument  "No name provided")
  | None    ,Some _ -> raise (Invalid_argument "No table provided")
  | Some tbl,Some n -> add_type tbl [] n typ
  end;
  typ

let new_enumerated ?symboltable ?name values =
    let rec with_indices vals offset = match vals with
    |   []  -> []
    | n::tl -> (n,offset)::with_indices tl (offset+1)
    in
    let ivalues = with_indices values 0 in
    let rec new_type = {
      type_stub with
      trait = Enumeration ivalues;
    } in
    begin match symboltable with
    | None -> ()
    | Some tbl -> List.iter (fun (id,ival) ->
                              add_variable tbl
                                           []
                                           id
                                           (from_int new_type ival)
                            )
                            ivalues
    end;
    maybe_add ?symboltable ?name new_type

let new_derived ?symboltable ?name old =
    maybe_add ?symboltable ?name { old with uid = uid#gen }

(* Parent of parent of parent.  *)
let rec root_parent typ =
    match typ.parent with
        | None -> typ
        | Some p -> root_parent p

let new_unconstr ?symboltable ?name parent =
    maybe_add ?symboltable ?name
    {
      type_stub with
      parent = Some (root_parent parent);
      trait  = Signed None;
      uid = parent.uid;
    }

let new_constr ?symboltable ?name parent r =
    maybe_add ?symboltable ?name
    {
      type_stub with
      parent = Some (root_parent parent);
      trait = Signed (Some r);
      uid = parent.uid;
    }

let new_range ?symboltable ?name r =
  new_constr ?symboltable ?name universal_integer r

let new_modular ?symboltable ?name size =
    maybe_add ?symboltable ?name
    {
      type_stub with
      trait = Modular size
    }

let new_float ?symboltable ?name digits =
    maybe_add ?symboltable ?name
    {
      type_stub with
      trait = Float digits
    }

let new_array ?symboltable ?name component ind =
    maybe_add ?symboltable ?name
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

let integer  = new_constr ~symboltable:builtin_table
                          ~name:"integer"
                          universal_integer
                          (integer_first @.. integer_last)

let natural  = new_constr ~symboltable:builtin_table
                          ~name:"natural"
                          integer
                          (0 @.. integer_last)

let positive = new_constr ~symboltable:builtin_table
                          ~name:"positive"
                          integer
                          (1 @.. integer_last)

let boolean = new_enumerated ~symboltable:builtin_table
                             ~name:"boolean"
                             ["true" ; "false"]

let _float = new_float ~symboltable:builtin_table
                       ~name:"float"
                       6

let builtin_type x = find_type builtin_table [] x

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
| Modular m -> Newspeak.Nat.of_int m
| Array _
| Float _
| Signed None -> raise (Invalid_argument "Type with no size")

let rec attr_get typ attr args =
  match typ.trait, attr with
    | Signed (Some Range(a,_)),"first" -> from_nat typ a
    | Signed (Some Range(_,b)),"last"  -> from_nat typ b
    | Enumeration values, "first"      -> from_int typ (snd (List.hd  values))
    | Enumeration values, "last"       -> from_int typ (snd (List_utils.last
                                                                      values))
    | Array (_,ind) , ("first"|"last"|"length")  ->
        begin
        match args with
        | [] -> attr_get typ attr [from_int integer 1]
        | [nv] -> begin match (to_int nv) with
                        | None -> failwith "Unreachable code"
                        | Some n -> if attr="length" then
                            from_nat universal_integer
                             (length_of (List.nth ind (n-1)))
                          else
                            attr_get (List.nth ind (n-1)) attr []
                  end
        | _ -> raise ( Invalid_argument
                      "Too many arguments")
        end
    | Modular _,      "first" -> from_int typ 0
    | Modular size,    "last" -> from_int typ (size-1)
    | Modular size, "modulus" -> from_int typ size
    | Float digits , "digits" -> from_int universal_integer digits
    | Array _, _
    | Modular _, _
    | Float _ , _
    | Enumeration _ , _
    | Signed (Some Range(_,_)),  _
    | Signed (None)           ,  _
    | Signed (Some NullRange) ,  _
                 -> raise ( Invalid_argument ("No such attribute : '"^attr^"'"))

let (@.) typ attr = attr_get typ attr []

(****************
 * Traits tests *
 ****************)

let is_limited typ =
  typ.limited

let is_integer typ =
  match typ.trait with
  | Array       _
  | Float       _
  | Enumeration _ -> false
  | Modular     _
  | Signed      _ -> true

let is_modular typ =
  match typ.trait with
  | Modular     _ -> true
  | Array       _
  | Enumeration _
  | Float       _
  | Signed      _ -> false

let is_boolean typ =
  root_parent typ == boolean

let is_discrete typ =
  match typ.trait with
  | Signed      _
  | Enumeration _
  | Modular     _ -> true
  | Array       _
  | Float       _ -> false

let whose_component predicate typ = match typ.trait with
  | Array (component,_) -> predicate component
  | _ -> false

let is_numeric typ =
  match typ.trait with
  | Array       _
  | Enumeration _ -> false
  | Modular     _
  | Float       _
  | Signed      _ -> true

let is_one_dim_array typ =
  match typ.trait with
  | Array (_,ind) -> List.length ind = 1
  | Enumeration _
  | Float       _
  | Modular     _
  | Signed      _ -> false

let is_scalar typ =
  match typ.trait with
  | Array       _ -> false
  | Float       _
  | Enumeration _
  | Modular     _
  | Signed      _ -> true

let some_eq o1 o2 =
  match (o1,o2) with
  | Some x, Some y -> x = y
  | _, None
  | None, _ -> false

let typeof(t,_) = t

(* TODO : implement it in Univ ? *)
let (@=?) (_,v1) (_,v2) =
       some_eq (project_int  v1) (project_int  v2)
    || some_eq (project_bool v1) (project_bool v2)
    || some_eq (project_nat  v1) (project_nat  v2)

let (@=) v1 v2 =
     (typeof v1  =  typeof v2)
  && (       v1 @=?        v2)

let _ =
  add_type builtin_table [] "character" character

