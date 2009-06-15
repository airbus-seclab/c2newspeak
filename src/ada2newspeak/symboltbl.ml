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

(**
 * Wraps what the "current package" is and which packages are
 * marked using the "with" and "use" constructs.
 *)
class package_manager :
object
    method set_current :Syntax_ada.name -> unit
    method reset_current :unit
    method current    :Syntax_ada.package
    method add_with   :Syntax_ada.name -> unit
    method is_with    :Syntax_ada.package -> bool
    method add_use    :Syntax_ada.name -> unit
    method remove_use :Syntax_ada.name -> unit
    method get_use    :Syntax_ada.package list
    method is_extern :bool
    method as_extern_do :(unit->unit)->unit
    method normalize_name : Syntax_ada.name -> bool -> Syntax_ada.name
    method add_renaming_decl : Syntax_ada.name -> Syntax_ada.name -> unit
end = object (self)
      val mutable current_pkg:Syntax_ada.package                 = []
      val mutable    with_pkg:Syntax_ada.package list            = []
      val             context:(Syntax_ada.package,int) Hashtbl.t = Hashtbl.create 3
      val mutable     extflag:bool                    = false
      val mutable    renaming:(Syntax_ada.name*Syntax_ada.name) list = []

      method set_current (x,y) :unit =
        let p = x@[y] in
          current_pkg <- p

      method reset_current =
          current_pkg <- []

      method current =
          current_pkg

      method add_with (x,y) =
        let p = x@[y] in
          with_pkg <- p::with_pkg

      method is_with pkg =
          List.mem pkg with_pkg

      method add_use (x,y) =
        let p = x@[y] in
        if (self#current <> p) then begin
            if (not (self#is_with p)) then begin
              Npkcontext.report_error "Ada_normalize.add_context"
                ((Ada_utils.ident_list_to_string p)^" is undefined")
            end;
            let old_count = try Hashtbl.find context p with Not_found -> 0 in
            Hashtbl.replace context p (old_count + 1)
        end

      method remove_use (x,y) =
        let p = x@[y] in
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

      method normalize_name (name:Syntax_ada.name) extern =
        try List.assoc name renaming with
        Not_found ->
        if (not extern) then name
        else let (parents,ident) = name in
             let pack = self#current in
               match parents with
                 | []                              -> (pack, ident)
                 | a when a=pack || self#is_with a -> (  a , ident)
                 | _ -> Npkcontext.report_error "pkgmgr.normalize_name"
                 ("unknown package "^(Ada_utils.ident_list_to_string parents))

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

let error x =
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
 * Abstract specification for function/procedure parameters.
 *)
type f_param = { fp_name : string
               ; fp_in   : bool
               ; fp_out  : bool
               ; fp_type : Ada_types.t
               }

let to_fparam (a,b,c,d) =
  { fp_name = a
  ; fp_in   = b
  ; fp_out  = c
  ; fp_type = d
  }

let from_fparam f =
  ( f.fp_name
  , f.fp_in
  , f.fp_out
  , f.fp_type
  )

(**
 * A symbol table.
 * t_var and t_type hold respectively data related
 * to variables and returning their type and to types.
 *
 * TODO : However it is not type-safe, as different
 * information is typed the same way.
 *)

type table = { mutable renaming : (string*string) list
             ; t_var    : Ada_types.t IHashtbl.t
             ; t_type   : Ada_types.t IHashtbl.t
             ; t_func   : ((f_param list)*Ada_types.t option) IHashtbl.t
             ; t_units  : table IHashtbl.t
             ; pkgmgr   : package_manager
             }

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
    ;Ada_types.print s_t
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

(**
 * Private global symbol table.
 * It is made available to the rest of the world by
 * builtin_type and builtin_variable.
 *)
let builtin_table :table = { renaming = []
                           ; t_var    = IHashtbl.create 0
                           ; t_type   = IHashtbl.create 0
                           ; t_func   = IHashtbl.create 0
                           ; t_units  = IHashtbl.create 0
                           ; pkgmgr   = new package_manager
                           }

let create_table _ =  { renaming = []
                      ; t_var    = IHashtbl.copy builtin_table.t_var
                      ; t_type   = IHashtbl.copy builtin_table.t_type
                      ; t_func   = IHashtbl.copy builtin_table.t_func
                      ; t_units  = IHashtbl.create 0
                      ; pkgmgr   = new package_manager
                      }

let add_variable tbl n t =
  Npkcontext.print_debug ("Adding variable "^String.concat "." (fst n@[snd n]));
  IHashtbl.add tbl.t_var n t

let add_type tbl n typ =
  Npkcontext.print_debug ("Adding type  "   ^String.concat "." (fst n@[snd n]));
  IHashtbl.add tbl.t_type n typ

let add_subprogram tbl name params rettype =
  IHashtbl.add tbl.t_func name (List.map (fun (a,b,c,d) ->
    { fp_name = a
    ; fp_in   = b
    ; fp_out  = c
    ; fp_type = d
    }) params,rettype)

let rec find_information t hashtbl (package,id) =
  try find_information t hashtbl (package,List.assoc id t.renaming) with
    Not_found ->
  begin
    if package != [] then IHashtbl.find hashtbl (package,id)
    else (* no package : try it as a local variable,
          *              or within current context
          *              (if provided)
          *)
      try IHashtbl.find hashtbl ([],id)
      with Not_found ->
        begin
            let p = List.find (fun pkg -> IHashtbl.mem hashtbl (pkg,id))
                              (t.pkgmgr#current::t.pkgmgr#get_use)
              (* List.find throws Not_found if     *
               * no package matches the predicate. *
               * This is the expected behaviour.   *)
            in
            IHashtbl.find hashtbl (p,id)
        end
  end

let find_type tbl n =
  try find_information tbl tbl.t_type n
  with Not_found -> begin
                      error ("Cannot find type "
                            ^String.concat "." (fst n@[snd n])
                            );
                      raise Not_found
                    end

let find_subprogram tbl n =
  try (fun (x,y) -> List.map from_fparam x,y)
      (find_information tbl tbl.t_func n)
  with Not_found -> begin
                      error ("Cannot find subprogram "
                            ^String.concat "." (fst n@[snd n])
                            );
                      raise Not_found
                    end

let find_variable tbl n =
  try find_information tbl tbl.t_var n
  with Not_found ->
    begin
      try
        (* Maybe it is a parameterless function call *)
        match find_subprogram tbl n with
          | ([], Some rt) -> rt
          | _             -> raise Not_found
      with Not_found ->
        error ("Cannot find variable "^snd n);
 (*      Ada_types.unknown; *)
        failwith "aaarg"
    end

let add_renaming_declaration t newname oldname =
  t.renaming <- (newname,oldname)::t.renaming

let builtin_type x = find_type builtin_table ([],x)

let set_current       t = t.pkgmgr#set_current
let reset_current     t = t.pkgmgr#reset_current
let current           t = t.pkgmgr#current
let add_with          t = t.pkgmgr#add_with          
let is_with           t = t.pkgmgr#is_with           
let add_use           t = t.pkgmgr#add_use           
let remove_use        t = t.pkgmgr#remove_use        
let get_use           t = t.pkgmgr#get_use           
let is_extern         t = t.pkgmgr#is_extern         
let as_extern_do      t = t.pkgmgr#as_extern_do      
let normalize_name    t = t.pkgmgr#normalize_name    
let add_renaming_decl t = t.pkgmgr#add_renaming_decl 

let _ =
  List.iter (fun (n,t) -> add_type builtin_table ([],n) t)
  ["integer"  , Ada_types.integer
  ;"float"    , Ada_types.std_float
  ;"boolean"  , Ada_types.boolean
  ;"natural"  , Ada_types.natural
  ;"positive" , Ada_types.positive
  ;"character", Ada_types.character
  ]

module Stack = struct
  type t = table Stack.t

  let create _ =
    let s = Stack.create () in
    Stack.push builtin_table s;
    let library = create_table () in
    Stack.push library s;
    s

  let top x = Stack.top x

  let enter_context s name_opt =
    let new_context = create_table () in
    begin match name_opt with
      | None      -> ()
      | Some name -> IHashtbl.add ((Stack.top s).t_units) ([],name) new_context
    end;
    Stack.push new_context s

  let new_unit s name =
    while (Stack.length s > 2) do
      ignore (Stack.pop s);
    done;
    (* Here Stack.length s = 2 so we are at library level *)
    enter_context s (Some name)

  let exit_context x = ignore (Stack.pop x)
end
