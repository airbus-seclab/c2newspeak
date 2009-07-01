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

(* debug control *)

let debug_use_new = false

let error x =
  if (debug_use_new) then
    Npkcontext.report_warning "Symbol table" x
  else
    Npkcontext.print_debug ("ERROR : Symbol table"^x)

let debug_dont_push = not debug_use_new

class package_manager :
object
    method add_use    :string -> unit
    method remove_use :string -> unit
    method get_use    :string list
    method is_extern :bool
    method as_extern_do :(unit->unit)->unit
    method add_renaming_decl : Syntax_ada.name -> Syntax_ada.name -> unit
end = object (self)
      val             context:(string,int) Hashtbl.t  = Hashtbl.create 3
      val mutable     extflag:bool                    = false
      val mutable    renaming:( Syntax_ada.name
                              * Syntax_ada.name) list = []

      method add_use p =
        let old_count = try Hashtbl.find context p with Not_found -> 0 in
        Hashtbl.replace context p (old_count + 1)

      method remove_use p =
        try
          let old_count = Hashtbl.find context p in
          if old_count = 1 then Hashtbl.remove context p
          else Hashtbl.replace context p (old_count - 1)
        with Not_found -> ()

      method get_use =
        Hashtbl.fold (fun pkg _ res -> pkg::res) context []

      method is_extern =
        extflag

      method as_extern_do (f:unit->unit) =
        extflag <- true;
        f ();
        extflag <- false

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
      method add_renaming_decl new_name old_name =
        Npkcontext.print_debug ("add_rd "
                               ^Ada_utils.name_to_string new_name
                               ^" --> "
                               ^Ada_utils.name_to_string old_name
        );
        if (new_name=old_name) then
          Npkcontext.report_error "add_renaming_decl"
                        ("Circular declaration detected for '"
                        ^Ada_utils.name_to_string new_name^"'.")
        else begin
          try
            let pointee = List.assoc new_name renaming in
            self#add_renaming_decl new_name pointee
          with Not_found ->
            renaming <- (new_name,old_name)::renaming
        end
  end

(**
 * The [string] type with primitives to
 * handle it in a case-insensitive way.
 *)
module CaseInsensitiveString =
  struct
    type t = string

    let equal_s s1 s2 =
      String.compare (String.lowercase s1) (String.lowercase s2) = 0

    let equal i1 i2 =
      equal_s i1 i2

    let hash_s s =
      Hashtbl.hash (String.lowercase s)

    let hash i =
      hash_s i

    let to_string x = x
  end

(** A hash table insensitive to keys' case.  *)
module IHashtbl = Hashtbl.Make(CaseInsensitiveString)

(**
 * Abstract specification for function/procedure parameters.
 *)
type f_param = { fp_name : string
               ; fp_in   : bool
               ; fp_out  : bool
               ; fp_type : T.t
               }

let   to_fparam (a,b,c,d) = {  fp_name = a;fp_in = b;fp_out = c;fp_type = d}
let from_fparam     f     = (f.fp_name , f.fp_in , f.fp_out , f.fp_type)

(**
 * Symbols.
 *)
type symbol =
  | Variable   of T.t*(T.value option)
  | Type       of T.t
  | Subprogram of ((f_param list)*T.t option)
  | Unit       of table

(**
 * A symbol table.
 * t_tbl holds pairs of symbol and bool :
 * "false" (weak) symbols may be redefined.
 * Ex : a xxx_spec will yield weak symbols than may be redefined.
 *)

and table = { mutable renaming : (string*string) list
            ; t_tbl            : (symbol*bool) IHashtbl.t
            ; pkgmgr           : package_manager
            ; t_desc           : string option
            ; t_loc            : Newspeak.location
            }

let print_symbol = function
  | Variable   (t,_) -> "V",T.print t
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

exception ParameterlessFunction of T.t

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

let mkcast desc fn  ?(filter=fun _ -> true) fallback lst  =
  if (List.length lst > 1) then
    begin
      Npkcontext.print_debug ("Multiple interpretations for "^desc^" :");
      List.iter (fun x -> Npkcontext.print_debug ("\t"
                                                 ^print_symbol_join x
                                                 )) lst
    end;
  match extract_unique ~filter fn lst with
    | None   -> error ("Symbol cast ("^desc^") : Ambiguous name");fallback
    | Some x -> x

let print_table tbl =
  let out = Buffer.create 0 in
  let print_string x = Buffer.add_string out x in
  let pad width str =
    let x = String.length str in
    let b = (width - x) / 2   in
    let a =  width - x - b    in
    (String.make a ' ')^str^(String.make b ' ')
  in
  let line_printer i (sym,_) =
    let (t,sym_d) = print_symbol sym in
    List.iter print_string ["|" ; pad 6 t ; "|" ; pad 15 i ; "| " ; sym_d];
    print_string "\n"
  in
  begin match tbl.t_desc with
    | None -> ()
    | Some desc -> print_string ("(" ^ desc ^ ")\n"
                                ^ (if tbl.t_loc = Newspeak.unknown_loc then "" 
                                   else "@ "^ Newspeak.string_of_loc tbl.t_loc)
                                ^ "\n")
  end;
  print_string "+------+---------------+---- . . .\n";
  IHashtbl.iter line_printer tbl.t_tbl;
  print_string "+------+---------------+---- . . .\n";
  print_string "\n";
  Buffer.contents out

let create_table ?desc _ =  { renaming = []
                            ; t_tbl    = IHashtbl.create 0
                            ; pkgmgr   = new package_manager
                            ; t_desc   = desc
                            ; t_loc    = Npkcontext.get_loc ()
                            }

(**
 * Private global symbol table.
 *)
let builtin_table :table = create_table ~desc:"builtin" Newspeak.unknown_loc

let add_variable ?(strongly=true) tbl n t =
  try begin match IHashtbl.find tbl.t_tbl n with
      | (Variable (tp,_),true) when t=tp -> error "Homograph variable"
      | (_sym,false) -> begin
                          Npkcontext.print_debug ("Replacing weak symbol '"^n^"'");
                          IHashtbl.replace tbl.t_tbl n (Variable (t,None),strongly);
                        end
      | (_  , true) -> raise Not_found
      end
  with Not_found -> begin
                      Npkcontext.print_debug ("Adding variable "^n);
                      IHashtbl.add tbl.t_tbl n (Variable (t,None),strongly)
                    end

let add_type tbl n typ =
  Npkcontext.print_debug ("Adding type "^n);
  IHashtbl.add tbl.t_tbl n (Type typ,true)

let add_subprogram tbl name params rettype =
  IHashtbl.add tbl.t_tbl name (Subprogram (List.map to_fparam params,rettype),true)

(******** Cast functions ********)

let cast_v ?filter = mkcast "variable"
                    (function Variable (x,_) -> Some x
                            | Subprogram ([],Some rt) ->
                                raise (ParameterlessFunction rt)
                            |_ -> None
                    )
                    T.unknown
                    ?filter

let cast_t ?filter = mkcast "type"
                     (function Type x -> Some x
                             | _      -> None)
                     T.unknown
                     ?filter

let cast_s ?filter = mkcast "subprogram"
                     (function Subprogram x -> Some x
                             | _            -> None)
                     ([],None)
                     ?filter

(**
 * Find matching symbols in a table.
 *)
let rec find_symbols t id =
  try find_symbols t (List.assoc id t.renaming)
  with Not_found -> fst (List.split (IHashtbl.find_all t.t_tbl id))

let find_type tbl n =
  cast_t (find_symbols tbl n)

let find_subprogram tbl n =
  (fun (x,y) -> List.map from_fparam x,y)
      (cast_s (find_symbols tbl n))

let find_variable tbl ?expected_type n =
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
  cast_v ~filter:ovl_predicate (find_symbols tbl n)

let find_unit t id =
  match (find_symbols t id) with
  | [Unit x] -> Some x
  | _ -> None

let builtin_type x = find_type builtin_table x

let add_use           t = t.pkgmgr#add_use
let remove_use        t = t.pkgmgr#remove_use
let get_use           t = t.pkgmgr#get_use
let is_extern         t = t.pkgmgr#is_extern
let as_extern_do      t = t.pkgmgr#as_extern_do
let add_renaming_decl t = t.pkgmgr#add_renaming_decl

let _ =
  List.iter (fun (n,t) -> add_type builtin_table n t)
  ["integer"  , T.integer
  ;"float"    , T.std_float
  ;"boolean"  , T.boolean
  ;"character", T.character
  ]

module SymStack = struct

  type t = {         s_stack  : table Stack.t
           ; mutable s_cpkg   : string option
           ; mutable s_with   : string list
           ; mutable s_strong : bool
           }

  let top s = Stack.top s.s_stack

  let print s =
    let res = Buffer.create 0 in
    Buffer.add_string res "Stack : (starting at top)\n";
    Stack.iter (fun t -> Buffer.add_string res (print_table t)) s.s_stack;
    Buffer.contents res

  let create _ =
    let s = Stack.create () in
    Stack.push builtin_table s;
    let library = create_table ~desc:"library" () in
    Stack.push library s;
    { s_stack  = s
    ; s_cpkg   = None
    ; s_with   = []
    ; s_strong = true
    }

  let set_current s x = s.s_cpkg <- Some x

  let reset_current s = s.s_cpkg <- None

  let current s = s.s_cpkg

  let add_with s x = s.s_with <- x::s.s_with

  let is_with  s x = List.mem x s.s_with

  let s_add_use s p =
    if (current s) = Some p then begin
      if (is_with s p) then begin
        Npkcontext.report_error "Symboltbl.s_add_use"
          (p^" is undefined")
      end;
      (top s).pkgmgr#add_use p
    end

  let s_get_use s =
    let (ctx:string list ref) = ref [] in
    Stack.iter (fun t -> ctx := t.pkgmgr#get_use@(!ctx)) s.s_stack;
    match s.s_cpkg with
    | None   ->    !ctx
    | Some p -> p::!ctx

  let normalize_name s (name:Syntax_ada.name) extern =
    try None,List.assoc (snd name) (top s).renaming with
    Not_found ->
    if (not extern) then name
    else let (parents,ident) = name in
         let pack = current s in
           match parents with
             | None                    -> (pack   , ident)
             | Some a when is_with s a -> (Some a , ident)
             | b      when b = pack    -> (b , ident)
             | _ -> Npkcontext.report_error "pkgmgr.normalize_name"
             ("unknown package "^(match parents with
                                    | Some x -> x
                                    | _      -> "<no name>"))

  let enter_context ?name ?desc ?(weakly=false) (s:t) =
    if (not debug_dont_push) then
      begin
          s.s_strong <- not weakly;
          Npkcontext.print_debug (">>>>>>> enter_context ("
                                 ^(match name with
                                     | Some n -> n
                                     | _      -> "")^")");
          let create_context _ =
            begin
              let new_context = create_table ?desc () in
              begin match name with
                | None   -> ()
                | Some n -> IHashtbl.add ((Stack.top s.s_stack).t_tbl)
                                          n (Unit new_context,true)
              end;
              new_context;
            end
          in
          let context =
            match name with
              | None   -> create_context ()
              | Some n -> begin
                            match (find_unit (Stack.top s.s_stack) n) with
                              | None   -> create_context ()
                              | Some u -> u
                          end
          in

          Stack.push context s.s_stack
        end

    let new_unit s name =
      while (Stack.length s.s_stack > 2) do
        ignore (Stack.pop s.s_stack);
      done;
      (* Here Stack.length s = 2 so we are at library level *)
      enter_context ~name s

    let exit_context s =
      if not (debug_dont_push) then
        begin
          s.s_strong <- true;
          Npkcontext.print_debug "<<<<<<< exit_context ()";
          Npkcontext.print_debug (print s);
        if(Stack.length s.s_stack > 2) then
          ignore (Stack.pop s.s_stack)
        else
          Npkcontext.report_error "exit_context" "Stack too small"
      end

  (**
   * Find some data in a stack.
   * Applies f to every table of s, from top to bottom.
   * The first x such as f t = Some x is returned.
   * If forall t in s, f t = None, Not_found is raised.
   *)
  let find_rec s f =
    let s = Stack.copy s in
    let r = ref None in
    try
      while (!r = None) do
        let t = Stack.pop s in
        r := f t
      done;
      match !r with
      | None   -> failwith "find" (* unreachable *)
      | Some x -> x
    with Stack.Empty -> raise Not_found

  (**
   * Find something in the current package.
   *)
  let find_current s n =
    let s = Stack.copy s in
    while Stack.length s>3 do
      ignore (Stack.pop s);
    done;
    if (Stack.length s <> 3) then
      begin
        error "No current package";
        []
      end
    else
      begin
        find_symbols (Stack.top s) n
      end

  let library s =
    let s = Stack.copy s in
    while (Stack.length s > 2) do
      ignore (Stack.pop s);
    done;
    Stack.top s

  let s_find_abs_var s p n =
    match find_unit (library s.s_stack) p with 
      | Some tbl ->find_variable tbl n
      | None     -> error "no such package";T.unknown

  (**
   * Find a variable in a symbol table stack.
   *)
  let s_find_variable (s:t) ?expected_type ?package n =
    match package with
      | Some p -> s_find_abs_var s p n
      | None   -> begin
                    let f t =
                      try Some (find_variable ?expected_type t n)
                      with Not_found -> None
                    in
                    try
                      find_rec s.s_stack f
                    with Not_found ->
                      error ("Cannot find variable '"^n^"'");
                      T.unknown
                  end

  let s_find_type s ?package n =
    ignore package;
    let f t =
      try Some (find_type t n)
      with Not_found -> None
    in
    try
      find_rec s.s_stack f
    with Not_found ->
      error ("Cannot find type "^n);
      T.unknown

  let s_add_variable s n v =
    add_variable   (top s) n v ~strongly:s.s_strong

  let s_add_type       s = add_type       (top s)
  let s_add_subprogram s = add_subprogram (top s)

  let type_ovl_intersection s n1 n2 =
    let inter l1 l2 =
      List.filter (fun x -> List.mem x l1) l2
    in
    let s1 = find_symbols (top s) n1 in
    let s2 = find_symbols (top s) n2 in
    let inte = inter s1 s2 in
    if inte = [] then T.unknown else cast_v inte

end
