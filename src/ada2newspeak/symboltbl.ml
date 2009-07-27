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

module T = Ada_types

let error x =
  Npkcontext.report_error "Symbol table" x

type scope = Lexical | In_package of string

exception Parameterless_function of T.t
exception Variable_no_storage    of T.t * T.data_t

module Table = struct

  module Symset : sig
    type 'a t

    val empty : 'a t
    val iter : ('a -> unit) -> 'a t -> unit
    val elements : 'a t -> 'a list
    val add : 'a -> 'a t -> 'a t
    val filter : ('a -> bool) -> 'a t -> 'a t

  end = struct

    type 'a t = 'a list

    let add e sy =
      if (List.mem e sy) then
        sy
      else
        e::sy


    let iter = List.iter

    let empty = []

    let filter = List.filter

    let elements x = List.rev x

    end

  (**
   * Symbols.
   *)
  type symbol =
    | Type       of T.t
    | Subprogram of ((Syntax_ada.param list)*T.t option)
    | Unit       of table
    | Variable   of T.t*(T.data_t option)*bool (* if true, no storage *
                                                * will be allocated   *)
                                         *bool (* R/O *)

  and table = { mutable t_renaming : (string*Syntax_ada.name) list
              ; mutable t_tbl      : elt Symset.t
              ;         t_desc     : string option
              ;         t_loc      : Newspeak.location
              ; mutable t_use_list : string list
              ;         t_scope    : scope
              }
  and elt = scope*string*Newspeak.location*symbol

  let tbl_add_use t p =
    if (not (List.mem p t.t_use_list)) then
      t.t_use_list <- p::(t.t_use_list)

  let get_use t =
    t.t_use_list

  let print_symbol = function
    | Variable   (t,None,_,_)   -> "V",T.print t
    | Variable   (t,Some v,_,_) -> "V *","("^T.print_data v^")"^ T.print t
    | Type         t   -> "T",T.print t
    | Subprogram (p,r) -> "S",(
                               let pdesc = " with "
                                         ^ string_of_int (List.length p)
                                         ^ " parameter(s)"
                               in match r with None   -> "procedure"^pdesc
                                             | Some t -> "function"
                                                         ^pdesc
                                                         ^" and return type "
                                                         ^T.print t
                              )
    | Unit         _   -> "U","<unit>"

  let print_symbol_join s =
    let (s1,s2) = print_symbol s in
    s1^" "^s2

  let print_table tbl =
    let out = Buffer.create 0 in
    let print_string x = Buffer.add_string out x in
    let pad width str =
      let x = String.length str in
      let b = (width - x) / 2   in
      let a =  width - x - b    in
      let b = max b 0 in
      let a = max a 0 in
      (String.make a ' ')^str^(String.make b ' ')
    in
    let line_printer (_,i,_,sym) =
      let (t,sym_d) = print_symbol sym in
      List.iter print_string ["|" ; pad 6 t ; "|" ; pad 15 i ; "| " ; sym_d];
      print_string "\n"
    in
    begin match tbl.t_desc with
      | None -> ()
      | Some desc -> print_string ("(" ^ desc ^ ")\n"
                                  ^(if tbl.t_loc = Newspeak.unknown_loc then ""
                                    else "@ "^ Newspeak.string_of_loc tbl.t_loc)
                                  ^"\n")
    end;
    print_string "+------+---------------+---- . . .\n";
    Symset.iter line_printer tbl.t_tbl;
    print_string "+------+---------------+---- . . .\n";
    print_string "\n";
    Buffer.contents out

  let create_table scope ?desc _ = { t_renaming = []
                                   ; t_tbl      = Symset.empty
                                   ; t_desc     = desc
                                   ; t_loc      = Npkcontext.get_loc ()
                                   ; t_use_list = []
                                   ; t_scope    = scope
                                   }

(******************************************************************************
 *                                                                            *
 *                               add_xxx functions                            *
 *                                                                            *
 ******************************************************************************)

  let adder (mksym:'a -> symbol) desc (tbl:table) (n:string)
            (loc:Newspeak.location) (t:'a) where =
    tbl.t_tbl <- Symset.add (where, n, loc, mksym t) tbl.t_tbl;
    Npkcontext.print_debug ("Adding "^desc^" '"^n^"', now Set = {"
      ^ String.concat ", " (List.map (fun (_,x,_,_) -> x)
                           (Symset.elements tbl.t_tbl))
      ^"}")

  let add_variable =
    adder (fun (t,v,ns,r) -> Variable (t,v,ns,r))
          "variable"

  let add_type =
    adder (fun t -> Type t)
          "type"

  let add_subprogram tbl n loc params ret =
    adder (fun (params,ret) -> Subprogram (params,ret))
          "subprogram"
          tbl
          n
          loc
          (params,ret)

(******************************************************************************
 *                                                                            *
 *                              cast_xxx functions                            *
 *                                                                            *
 ******************************************************************************)

  (* ('a -> 'b option) -> ?filter:('b->bool) -> 'a list -> 'b option *)
  let rec extract_unique p ?(filter:'b->bool=fun _ -> true) l =
    let p' x =
      match p x with
        | None   -> None
        | Some y -> if filter y then Some y else None
    in
    match l with
    |  []  -> raise Not_found
    | h::t -> begin
                match p' h with
                  | None -> extract_unique p' t
                  | Some r -> if List.exists (fun x -> p' x <> None) t
                              then None
                              else Some r
              end

  let mkcast desc fn  ?(filter=fun _ -> true) lst  =
    if (List.length lst > 1) then
      begin
        Npkcontext.print_debug ("Multiple interpretations for "^desc^" :");
        List.iter (fun (_,x) -> Npkcontext.print_debug ("\t"
                                                   ^print_symbol_join x
                                                   )) lst
      end;
    let fn' (wh,x) =
      match fn x with
      | None -> None
      | Some y -> Some (wh,y)
    in
    match extract_unique ~filter fn' lst with
      | None   -> error ("Ambiguous "^desc^" name")
      | Some x -> x

  let cast_v ?filter = mkcast "variable"
                      (function
                        | Variable (x,v     ,false,r) -> Some (x,v,r)
                        | Variable (x,Some v,true ,_) ->
                            raise (Variable_no_storage (x,v))
                        | Subprogram ([],Some rt) ->
                            raise (Parameterless_function rt)
                        |_ -> None
                      )
                      ?filter

  let cast_t ?filter = mkcast "type"
                       (function Type x -> Some x
                               | _      -> None)
                       ?filter

  let cast_s ?filter = mkcast "subprogram"
                       (function Subprogram x -> Some x
                               | _            -> None)
                       ?filter

(******************************************************************************
 *                                                                            *
 *                              find_xxx functions                            *
 *                                                                            *
 ******************************************************************************)

  let rec find_symbols t id =
    List.map (fun (wh,_,_,x) -> wh,x) (Symset.elements
                                  (Symset.filter (fun (_,m,_,_) -> m = id)
                                  t.t_tbl))

  let tbl_find_type tbl n =
    cast_t (find_symbols tbl n)

  let tbl_find_subprogram tbl n =
      (fun (wh,(x,y)) -> wh,(x,y))
          (cast_s (find_symbols tbl n))

  let tbl_find_variable tbl ?expected_type n :scope*(T.t*T.data_t option*bool)=
    let ovl_predicate = match expected_type with
      | Some t when t <> T.unknown
          -> fun x ->    T.is_compatible x t
      | _ -> fun _ -> true
    in
    begin
    Npkcontext.print_debug ("Find_variable "
                           ^n
                           ^" : expected type is "
                           ^match expected_type with None -> "None"
                                    | Some t -> T.print t)
    end;
    let s,(t,v,r) =
      cast_v ~filter:(fun (_,(x,_,_)) -> ovl_predicate x) (find_symbols tbl n)
    in
    s,(t,v,(match v with Some _ -> true | None -> r))


  let tbl_find_unit t id =
    match (find_symbols t id) with
    | [_,Unit x] -> Some x
    | _ -> None

(******************************************************************************
 *                                                                            *
 *                            builtin_table                                   *
 *                                                                            *
 ******************************************************************************)

  let builtin_table = create_table Lexical ~desc:"builtin" Newspeak.unknown_loc

  let _ =
    List.iter (fun (n,t) -> add_type builtin_table n
                                     Newspeak.unknown_loc
                                     t (In_package "std"))
    [ "integer"  , T.integer
    ; "float"    , T.std_float
    ; "boolean"  , T.boolean
    ; "character", T.character
    ]

end

(******************************************************************************
 *                                                                            *
 *                           Symbol table tree                                *
 *                                                                            *
 ******************************************************************************)

module SymMake(TR:Tree.TREE) = struct
  open Table

  type t = {         s_stack  : table TR.t
           ; mutable s_cpkg   : string option
           ; mutable s_with   : string list
           }

  type context = table

  let top s = TR.top s.s_stack

  let print s =
    let res = Buffer.create 0 in
    Buffer.add_string res "Stack : (starting at top)\n";
    TR.iter (fun t -> Buffer.add_string res (print_table t)) s.s_stack;
    Buffer.contents res

  let create _ =
    let s = TR.create () in
    TR.push builtin_table s;
    let library = create_table  Lexical ~desc:"library" () in
    TR.push library s;
    { s_stack  = s
    ; s_cpkg   = None
    ; s_with   = []
    }

  let set_current s x = s.s_cpkg <- Some x

  let reset_current s = s.s_cpkg <- None

  let current s = s.s_cpkg

  let add_with s x = s.s_with <- x::s.s_with

  let is_with  s x = List.mem x s.s_with

  let add_use s p =
    if (is_with s p || current s = Some p) then
      tbl_add_use (top s) p
    else
      Npkcontext.report_error "Symboltbl.add_use"
        (p^" is undefined")

  let s_get_use s =
    let ctx = TR.fold (fun r t -> (get_use t)@r) [] s.s_stack in
    Npkcontext.print_debug ("s_get_use : context = {"
                           ^String.concat "," ctx
                           ^"}");
    match s.s_cpkg with
    | None   ->    ctx
    | Some p -> p::ctx

  (**
   * Find some data in a stack.
   * Applies f to every table of s, from top to bottom.
   * The first x such as f t = Some x is returned.
   * If forall t in s, f t = None, Not_found is raised.
   *)
  let find_rec s f =
    match TR.lookup f s with
      | Some x -> x
      | None -> raise Not_found

  (**
   * Algorithm for add_rd (A,B)  (A -> B)
   *
   * If A = B, detect circular dependency.
   * If B is already a pointer to some C, we have something like
   *      A -> B -> C
   * So its equivalent to call (A -> C).
   *
   * Note that in the particular case where C = A (cycle), the recursive
   * call is (A -> A) and the circular dependency will be detected.
   *)
  let rec add_renaming_decl s new_name old_name =
    if ((None,new_name) = old_name) then
      Npkcontext.report_error "add_renaming_decl"
                    ("Circular declaration detected for '"
                    ^new_name^"'.")
    else begin
      try
        let pointee = List.assoc new_name (top s).t_renaming in
        add_renaming_decl s new_name pointee
      with Not_found ->
        (top s).t_renaming <- (new_name,old_name)::(top s).t_renaming
    end


  let enter_context ?name ?desc (s:t) =
          Npkcontext.print_debug (">>>>>>> enter_context ("
                                 ^(match name with
                                     | Some n -> n
                                     | _      -> "")^")");
          let create_context _ =
            begin
              let new_context = create_table ?desc (match name with None ->
                Lexical | Some p -> In_package p) () in
              begin match name with
                | None   -> ()
                | Some n -> begin
                              if (TR.height s.s_stack <> 2) then
                                error "Adding some unit outside the library";
                                let top = TR.top s.s_stack in
                                top.t_tbl <- Symset.add (Lexical
                                                        ,n
                                                        ,Newspeak.unknown_loc
                                                        ,Unit new_context)
                                                        top.t_tbl;
                            end
              end;
              new_context;
            end
          in
          let context =
            match name with
              | None   -> create_context ()
              | Some n -> begin
                            match (tbl_find_unit (TR.top s.s_stack) n) with
                              | None   -> create_context ()
                              | Some u -> u
                          end
          in
          TR.push context s.s_stack

  let exit_context s =
    Npkcontext.print_debug "<<<<<<< exit_context ()";
    Npkcontext.print_debug (print s);
    if(TR.height s.s_stack > 2) then
      TR.pop s.s_stack
    else
      Npkcontext.report_error "exit_context" "Stack too small"

  let push_saved_context s ctx =
    TR.push ctx s.s_stack

  let extract_variables t =
    let filter = function
      | _,x, loc, Variable (y, _, false, _) -> [x,y,loc]
      | _ -> []
    in
    List.flatten (List.map filter (Symset.elements t.t_tbl))

  let library s =
    TR.nth s 2

  let s_find_abs desc f s p n =
    match tbl_find_unit (library s.s_stack) p with
      | Some tbl -> f tbl n
      | None     -> error ("No such package "^p^" when resolving a "^desc)

  (* raise Not_found *)
  let s_find desc finder s ?package n =
    match package with
      | Some p -> s_find_abs desc finder s p n
      | None   ->
          begin
           try
             find_rec s.s_stack (fun t ->
               try Some (finder t n)
               with Not_found -> None
             )
           with Not_found ->
             begin
               let context = ref (s_get_use s) in
               let res = ref None in
               while (!context <> [] && !res = None) do
                 try
                   let p = (List.hd !context) in
                   res := Some (s_find_abs desc finder s p n);
                 with Not_found -> ();
                 context := List.tl !context;
               done;
               match !res with
               | None -> raise Not_found
               | Some (p,v) -> (p,v)
             end
         end

  let find_variable_value s ?expected_type (package,n) =
    try
      s_find "variable" (fun tbl n -> tbl_find_variable tbl ?expected_type n)
              s ?package n
    with Not_found -> error ("Cannot find variable '"^n^"'"
                            ^(match expected_type with
                              | None   -> ""
                              | Some _ -> " with this expected type"
                             ))

  let find_variable s ?expected_type name =
    (fun (x,(y,_,z)) -> (x,(y,z))) (find_variable_value s ?expected_type name)

  let find_type s (package,n) =
    try
      s_find "type" tbl_find_type s ?package n
    with Not_found -> error ("Cannot find type '"^n^"'")

  let find_subprogram s ?(silent=false) (package,n) =
    try
      s_find "subprogram" tbl_find_subprogram s ?package n
    with Not_found -> if silent then raise Not_found
                                else error ("Cannot find subprogram '"^n^"'")

  let is_operator_overloaded s n =
    try begin
      ignore (s_find "overloaded operator" tbl_find_subprogram s n);
      true
    end
    with Not_found -> false

  let scope t =
    t.t_scope

  let add_variable s n loc ?value ?(no_storage=false) ?(ro=false) t =
    Npkcontext.print_debug ("s_add_variable : adding '"
                           ^n
                           ^"' with "
                           ^(match value with
                            | None   -> "no value"
                            | Some v -> "value "^T.print_data v
                            )
                           );
    add_variable (top s) n loc (t,value,no_storage,ro) (scope (top s))

  let add_type s n loc v =
    add_type (top s) n loc v (scope (top s))

  let add_subprogram s n loc v rt =
    add_subprogram (top s) n loc v rt (scope (top s))

  let type_ovl_intersection s n1 n2 =
    let inter l1 l2 =
      let sym_eq (_,x) (_,y) = match (x,y) with
        | Variable (t1,_,_,_), Variable (t2,_,_,_) -> t1 = t2
        | a, b -> a = b
      in
      List.filter (fun x -> List.exists (fun y -> sym_eq x y) l1) l2
    in
    let s1 = find_symbols (top s) n1 in
    let s2 = find_symbols (top s) n2 in
    let inte = inter s1 s2 in
    let print_set set =
      "{"^
      String.concat ", " (List.map (fun (_,x) -> print_symbol_join x ) set)
      ^"}"
    in
    Npkcontext.print_debug ("Type_ovl_intersection : result = "
                           ^print_set inte
                           ^(if inte <> [] then ""
                             else
                               " , L = "^print_set s1^
                               " , R = "^print_set s2
                            )
                           );
    if inte = [] then T.unknown else
      let (_,(r,_,_)) = cast_v inte in
      r

  let first_child  x = TR.first_child  x.s_stack
  let next_sibling x = TR.next_sibling x.s_stack

end

include SymMake (Tree.FCNSTree)
