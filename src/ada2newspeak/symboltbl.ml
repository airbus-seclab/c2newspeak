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
open Syntax_ada

module T = Ada_types

let error x =
  Npkcontext.report_error "Symbol table" x

type scope = Lexical | In_package of string

exception Parameterless_function of scope * T.t
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

    let add e sy = if (List.mem e sy) then sy else e::sy

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
    | Subprogram of (string * (Syntax_ada.param list) * T.t option)
    | Unit       of table
    | Variable   of string
                  * T.t
                  * (T.data_t option)
                  * bool (* if true, no storage will be allocated   *)
                  * bool (* R/O *)

  and table = { mutable t_tbl      : elt Symset.t
              ;         t_desc     : string option
              ;         t_loc      : Newspeak.location
              ; mutable t_use_list : string list
              ;         t_scope    : scope
              }
  and elt = scope * string * Newspeak.location * symbol

  let tbl_add_use t p =
    if (not (List.mem p t.t_use_list)) then
      t.t_use_list <- p::(t.t_use_list)

  let get_use t =
    t.t_use_list

  let print_symbol = function
    | Variable  (_,t,None,_,_)   -> "V",T.print t
    | Variable  (_,t,Some v,_,_) -> "V *","(" ^ T.print_data v ^ ")" ^ T.print t
    | Type         t   -> "T",T.print t
    | Subprogram (_,p,r) -> "S",(
                               let pdesc = " with "
                                         ^ string_of_int (List.length p)
                                         ^ " parameter(s)"
                               in match r with None   -> "procedure" ^ pdesc
                                             | Some t -> "function"
                                                       ^ pdesc
                                                       ^ " and return type "
                                                       ^ T.print t
                              )
    | Unit         _   -> "U","<unit>"

  let print_symbol_join s =
    let (s1,s2) = print_symbol s in
    s1 ^ " " ^ s2

  let print_table tbl =
    let out = Buffer.create 0 in
    let print_string x = Buffer.add_string out x in
    let pad width str =
      let x = String.length str in
      let b = (width - x) / 2   in
      let a =  width - x - b    in
      let b = max b 0 in
      let a = max a 0 in
      (String.make a ' ') ^ str ^ (String.make b ' ')
    in
    let line_printer (_,i,_,sym) =
      let (t,sym_d) = print_symbol sym in
      List.iter print_string ["|" ; pad 6 t ; "|" ; pad 15 i ; "| " ; sym_d];
      print_string "\n"
    in
    begin match tbl.t_desc with
      | None -> ()
      | Some desc -> print_string ("(" ^ desc ^ ")\n"
                                  ^ (if tbl.t_loc = Newspeak.unknown_loc then ""
                                     else "@ "
                                         ^ Newspeak.string_of_loc tbl.t_loc)
                                  ^ "\n")
    end;
    print_string "+------+---------------+---- . . .\n";
    Symset.iter line_printer tbl.t_tbl;
    print_string "+------+---------------+---- . . .\n";
    print_string "\n";
    Buffer.contents out

  let create_table scope ?desc _ = { t_tbl      = Symset.empty
                                   ; t_desc     = desc
                                   ; t_loc      = Npkcontext.get_loc ()
                                   ; t_use_list = ["standard"]
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
    Npkcontext.print_debug ("Adding " ^ desc ^ " '" ^ n ^ "', now Set = {"
      ^ String.concat ", " (List.map (fun (_,x,_,_) -> x)
                           (Symset.elements tbl.t_tbl))
      ^ "}")

  let add_variable tbl n =
    adder (fun (t,v,ns,r) -> Variable (n,t,v,ns,r))
          "variable" tbl n

  let add_type =
    adder (fun t -> Type t)
          "type"

	    
  let add_subprogram tbl n params ret =
    adder (fun (params,ret) -> Subprogram (n,params,ret))
          "subprogram"
          tbl
          n
          Newspeak.unknown_loc
          (params,ret)

(******************************************************************************
 *                                                                            *
 *                              cast_xxx functions                            *
 *                                                                            *
 ******************************************************************************)

  (* ('a -> 'b option) -> ?filter:('b->bool) -> 'a list -> 'b option *)
  let rec extract_unique p ?(filter:'b -> bool = fun _ -> true) l =
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
                              then 
				None
		              else Some r
              end

  let mkcast desc fn  ?(filter = fun _ -> true) lst  =
    if ((compare (List.length lst) 1 > 0) (*WG *) &&
	  (compare desc "subprogram" <> 0)  
	  (*WG *)) 
    then
      begin
	Npkcontext.print_debug ("Multiple interpretations for " ^ desc ^ " :");
	List.iter (fun (_,x) -> Npkcontext.print_debug
		     ("\t" ^ print_symbol_join x)) lst
      end;
  
    let fn' (wh,x) =
      match fn x with
      | None -> None
      | Some y -> Some (wh,y)
    in
    match extract_unique ~filter fn' lst with
      | None   -> error ("Ambiguous " ^ desc ^ " name") 
      | Some x -> x

  let cast_v ?filter = mkcast "variable"
                      (fun s -> match s with
                        | Variable (_,_,_     ,false,_) -> Some s
                        | Variable (_,_,Some _,true ,_) -> Some s
                        | Subprogram (_,[],Some  _)     -> Some s
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
    List.map (fun (wh,_,_,x) -> wh,x) (
      Symset.elements
        (Symset.filter (fun (_,m,_,_) -> m = id)
           t.t_tbl)
    )
      
  let tbl_find_type tbl n =
    cast_t (find_symbols tbl n)


 let tbl_find_subprogram_simple tbl n = 
   (fun (wh,(x,y,z)) -> wh,(x,y,z))
     (cast_s (find_symbols tbl n))

  let tbl_find_subprogram n_args xpect  my_find tbl n =
  
    let do_match xpect z = 
      match (xpect, z) with 
	  Some xpec, Some ret -> 
	    T.is_compatible xpec ret
	| None, None -> true
	| _ -> false
    in

    let filter_args norm_args (_, (_, params, _))  =
      let lgth1 = List.length norm_args in
      let lgth2 = List.length params in
	if ((compare  lgth1 lgth2 <> 0) ||
	      (List.exists (fun x -> match 
			      fst x with Some _ -> true
				|_ -> false) norm_args)
	   )
	then 
	  error ("Symbtbl: looking for a subprogram use of "^
		   "default param, or named not handled yet...")
	else (*pas de param 'nommés': on check la compatibilité dans l'ordre*)
	  (*comparing expected type and return type in compared lists*)
	   List.for_all2 (
	    fun x y -> 
	      let act_typ = snd x in
	      let for_typ = 
		let  n =  match y.param_type with 
		  | [] -> Npkcontext.report_error 
		      "symboltbl:find_subprogram" "unreachable"
		  | x::[]    -> None  , x
		  | x::y::[] -> Some x, y
		  | _ -> Npkcontext.report_error
		      "symboltbl:find_subprogram"
			"chain of selected names is too deep"
		in
		  try 
		    begin
		      try
			snd (my_find  n)
		      with Not_found -> Npkcontext.report_error
			("Cannot find type '") ""
		    end
		  with Not_found ->
		    Npkcontext.report_error "Symbtbl:not found" ""
	      in
		T.is_compatible act_typ for_typ
	  ) norm_args params
    in
      
    let subs = find_symbols tbl n in
      (*Attention au cas de multiple avec des parametres par defaut*)
    
      if (compare (List.length subs) 1 = 0) then 
      begin
	try 
	(fun (wh,(x,y,z)) -> wh,(x,y,z))
	(cast_s ~filter:(fun x -> 
	    let (_, (_, _, z)) = x in 
	     (filter_args n_args  x) && (do_match xpect z)
			) subs
	)
	with _ -> 
	  (fun (wh,(x,y,z)) -> wh,(x,y,z))  (cast_s subs)
(*WG TO DO*)

      end
      else
	(fun (wh,(x,y,z)) -> wh,(x,y,z))
	(cast_s ~filter:(fun x -> 
	    let (_, (_, _, z)) = x in 
	     (filter_args n_args  x) && (do_match xpect z)
			) subs
	)
      (* peut etre un potentiel renaming mais dont les 
	 args ne correspondent 
	if (compare (List.length subs) 1 = 0)
	then begin 
	print_endline "____________________only one ___________";
	(fun (wh,(x,y,z)) -> wh,(x,y,z))
        (cast_s  subs)
	end
	else
	(fun (wh,(x,y,z)) -> wh,(x,y,z))
	(cast_s ~filter:(fun x -> 
			     let (_, (_, _, z)) = x in 
	(filter_args n_args  x) && (do_match xpect z)
			  ) subs
	)
      *)


  let tbl_find_variable tbl ?expected_type n =
    let ovl_predicate = match expected_type with
      | Some t when not (T.is_unknown t)
          -> fun x ->    T.is_compatible x t
      | _ -> fun _ -> true
    in
    begin
    Npkcontext.print_debug ( "Find_variable "
                           ^ n
                           ^ " : expected type is "
                           ^ match expected_type with None   -> "None"
                                                    | Some t -> T.print t
                           )
    end;
    try
      let (s,sym) =
        cast_v ~filter:(function
                        | (_,(Variable (_,x,_,_,_)))      -> ovl_predicate x
                        | (_,(Subprogram(_, [], Some x))) -> ovl_predicate x
                        | _ -> true
                        ) (find_symbols tbl n)
      in
      let (n,t,v,r) = match sym with
        | Variable (n,x,      v, false, r) -> (n, x, v, r)
        | Variable (_,x, Some v, true , _) ->
              raise (Variable_no_storage (x, T.get_enum_litt_value x v))
        | Subprogram (_,[], Some rt)     ->
              raise (Parameterless_function (s, rt))
        | _ -> Npkcontext.report_error "find_variable" "unreachable"
      in
      s,(n,t,v,(match v with Some _ -> true | None -> r))
    with
    | Parameterless_function (Lexical, rt) ->
        raise (Parameterless_function (tbl.t_scope, rt))


  let tbl_find_unit t id =
    match (find_symbols t id) with
    | [_,Unit x] -> Some x
    | _ -> None

(******************************************************************************
 *                                                                            *
 *                            Builtin tables                                  *
 *                                                                            *
 ******************************************************************************)

  let standard_tbl = create_table Lexical ~desc:"standard" Newspeak.unknown_loc

  let system_tbl   = create_table Lexical ~desc:"system"   Newspeak.unknown_loc

  let _ =
    begin
      List.iter (fun (n,t) -> add_type standard_tbl n
                                       Newspeak.unknown_loc
                                       t (In_package "standard"))
      [ "integer"  , T.integer
      ; "float"    , T.std_float
      ; "boolean"  , T.boolean
      ; "character", T.character
      ];
      List.iter (fun (n,v) ->
        add_variable standard_tbl n Newspeak.unknown_loc
                     (T.boolean,Some (T.BoolVal v),true,true)
                     (In_package "standard");
      ) ["false",false
        ;"true" ,true];
      add_type system_tbl "address" Newspeak.unknown_loc
               T.system_address (In_package "system")
    end

end

(******************************************************************************
 *                                                                            *
 *                           Symbol table tree                                *
 *                                                                            *
 ******************************************************************************)

open Table

type t = {         s_stack    : table Tree.t
         ; mutable s_cpkg     : string option
         ; mutable s_with     : string list
         ; mutable s_renaming : (string * (string option * string)) list
         }

let top s = Tree.top s.s_stack

let print s =
  let res = Buffer.create 0 in
  Buffer.add_string res "Stack : (starting at top)\n";
  Tree.iter (fun t -> Buffer.add_string res (print_table t)) s.s_stack;
  Buffer.contents res

let create _ =
  let s = Tree.create () in
  let library = create_table  Lexical ~desc:"library" () in
  library.t_tbl <- Symset.add (Lexical
                              ,"standard"
                              ,Newspeak.unknown_loc
                              ,Unit standard_tbl)
                              library.t_tbl;
  library.t_tbl <- Symset.add (Lexical
                              ,"system"
                              ,Newspeak.unknown_loc
                              ,Unit system_tbl)
                              library.t_tbl;
  Tree.push library s;
  { s_stack    = s
  ; s_cpkg     = None
  ; s_with     = ["system";"machine_code";"unchecked_conversion"]
  ; s_renaming = []
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
      (p ^ " is undefined")

let s_get_use s =
  let ctx = Tree.fold (fun r t -> (get_use t) @ r) [] s.s_stack in
  Npkcontext.print_debug ( "s_get_use : context = {"
                         ^ String.concat "," ctx
                         ^ "}"
                         );
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
  match Tree.lookup f s with
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
 * 
*)
let add_renaming_decl s new_name old_name =
  if ((None,new_name) = old_name) then
    Npkcontext.report_error "add_renaming_decl"
                  ( "Circular declaration detected for '" ^ new_name ^ "'.")
  else
    (*Multiple renaming is possible: on ne le rajoute pas car s_renaming 
    n'a pas l'info sur les types params et retour
    on ne le rajoute pas pour ne pas boucler infiniment la recherche ulterieur*)
    (*WG *)
    if (List.mem new_name (List.map fst s.s_renaming)) then
      Npkcontext.report_warning "add_renaming_decl"
	( "Already renamed '"^ new_name ^ "'.")
    else
      (* dans le cas contraire (et seulement) on le rajoute*)
      begin
	Npkcontext.print_debug ( "renaming_declaration : "
				 ^ new_name
				 ^ " --> "
				 ^ (match (fst old_name) with
				      | None   -> ""
				      | Some p -> p ^ "."
				   )
				 ^ (snd old_name)
                               );
	s.s_renaming <- (new_name,old_name)::s.s_renaming
      end
	
let enter_context ?name ?desc (s:t) =
        Npkcontext.print_debug ( ">>>>>>> enter_context ("
                               ^ (match name with
                                    | Some n -> n
                                    | _      -> ""
                                 )
                               ^ ")"
			       );
        let create_context _ =
          begin
            let new_context = create_table ?desc (match name with None ->
              Lexical | Some p -> In_package p) () in
            begin match name with
              | None   -> ()
              | Some n -> begin
                            if (Tree.height s.s_stack <> 1) then
                              error "Adding some unit outside the library";
                              let top = Tree.top s.s_stack in
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
                          match (tbl_find_unit (Tree.top s.s_stack) n) with
                            | None   -> create_context ()
                            | Some u -> u
                        end
        in
        Tree.push context s.s_stack

let exit_context s =
  Npkcontext.print_debug "<<<<<<< exit_context ()";
  Npkcontext.print_debug (print s);
  if(Tree.height s.s_stack > 1) then
    ignore (Tree.pop s.s_stack)
  else
    Npkcontext.report_error "exit_context" "Stack too small"

let push_saved_context s ctx =
  Tree.push ctx s.s_stack

let library s =
  Tree.nth s 1


let s_find_abs _desc f s p n =
  match tbl_find_unit (library s.s_stack) p with
    | Some tbl -> f tbl n
    | None     -> raise Not_found
      

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

let find_variable_value s ?(silent = false) ?expected_type (package,n) =
  try
    s_find "variable" (fun tbl n -> tbl_find_variable tbl ?expected_type n)
      s ?package n
  with Not_found -> if silent
                    then raise Not_found
                    else begin
		      error ("Cannot find variable '" ^ n ^ "'"
                             ^ (match expected_type with
                                  | None   -> ""
                                  | Some _ -> " with this expected type")
			    )
		    end
		      (*WG	       ^(match expected_type with
                        | None   -> ""
                        | Some t -> Ada_types.print  t)
			^
			( print s;	(*WG*)	     
			)
			)
			end
			WG*)
		      
let rec find_variable s ?silent ?expected_type name =
  let var_name = snd name in 
  try
    let nam_assoc = List.assoc var_name s.s_renaming in
      (*Particular case: renaming with the same name: 
	only allowed one-level deep*)
      if (compare  var_name (snd nam_assoc) = 0) then
	(fun (x,(n,y,_,z)) -> (x,(n,y,z)))
	  (find_variable_value ?silent s ?expected_type nam_assoc) 
      else
	find_variable s ?silent ?expected_type nam_assoc
  with Not_found ->
    (fun (x,(n,y,_,z)) -> (x,(n,y,z)))
      (find_variable_value ?silent s ?expected_type name) 

(* WG --try
   find_variable s ?silent ?expected_type
   (List.assoc (snd name) s.s_renaming)
   with Not_found ->
   (fun (x,(n,y,_,z)) -> (x,(n,y,z)))
   (find_variable_value ?silent s ?expected_type name) 
*)

let find_type s (package,n) =
  try
    s_find "type" tbl_find_type s ?package n
  with Not_found -> error ("Cannot find type '" ^ n ^ "'")


let rec find_subprogram s ?(silent = false) (pack,n) norm_args xpect t_find =
  try 
    let ( p_opt, n_assoc) =  List.assoc n s.s_renaming in
      if (compare n n_assoc = 0) then
	begin 

	match p_opt with 
	    Some pck -> 
	      s_find "subprogram" 
		(tbl_find_subprogram norm_args xpect t_find) s
		~package:(pck) n_assoc
	  | _ ->  (*WG should not happen *)
	      s_find "subprogram" (tbl_find_subprogram norm_args xpect t_find) s n_assoc
	end
      else
	begin
	  find_subprogram s (p_opt, n_assoc) norm_args  xpect t_find 
	end

	  
  with Not_found ->
    (*No need for tbl_find_subprogram here*)
    try 
      match pack with 
	  Some pck ->  
	    s_find "subprogram"
	      (tbl_find_subprogram norm_args xpect t_find) s ~package:(pck) n 
	      
	|_-> s_find "subprogram" 
	   (tbl_find_subprogram norm_args xpect t_find) s n  
    
    with Not_found ->
	  if silent then
	    raise Not_found
	  else
	    error ("Cannot find subprogram '" ^ n ^ "'")
	  
  (*
    try  
    find_subprogram s (List.assoc n s.s_renaming) norm_args t_find 
    with Not_found ->
    try
    s_find "subprogram" (tbl_find_subprogram norm_args t_find) s ?package n
    with Not_found ->
    if silent then
    raise Not_found
    else
    error ("Cannot find subprogram '" ^ n ^ "'")
  *)
	
let is_operator_overloaded s n =
  try begin
    ignore (s_find "overloaded operator" tbl_find_subprogram_simple s n);
    true
  end
  with Not_found -> false

let scope t =
  t.t_scope

let add_variable s n loc ?value ?(no_storage = false) ?(ro = false) t =
  Npkcontext.print_debug ( "s_add_variable : adding '"
                         ^ n
                         ^ "' with "
                         ^ (match value with
                           | None   -> "no value"
                           | Some v -> "value " ^ T.print_data v
                           )
                         );
  add_variable (top s) n loc (t,value,no_storage,ro) (scope (top s))

let add_type s n loc v =
  add_type (top s) n loc v (scope (top s))


(*for Enumeration type redefining using the 'use ' clause*)
let replace_type s n new_t =
  let tabl = top s in
  let removed = 
    Symset.filter (fun (_,m,_,_) -> m <> n) tabl.t_tbl
  in
  let olds = 
    Symset.filter (fun (_,m,_,_) -> m = n) tabl.t_tbl
  in
    match (Symset.elements olds) with 
	[ (wh, m, lc, _)] ->
	  tabl.t_tbl <- Symset.add (wh, m, lc, Type new_t) removed; 
      | _ ->  error ("Replacing enumeration type failed "^n^
		       "too many candidate found ("^
		       (string_of_int(List.length(Symset.elements olds)))
		    ^")"
		    )
	  


(*for Enumeration type redefining using the 'use' clause*)
let replace_typ_enum s (name,data) oldt newt  = 
  let t = top s in
  let enum_searched = 
    Symset.filter ( 
      fun ( _, _, _, z)  ->
	match z with 
	    Variable (str , typ, _, no_st, _) -> 	 
	      (no_st && typ= oldt && str = name)
	  | _ -> false
    )  t.t_tbl 
  in
  let others = 
    Symset.filter ( 
      fun (_, _, _,  z)  ->
	match z with 
	    Variable (str , typ, _, no_st, _)  ->  
	      (not no_st || not (typ = oldt) || not (str = name))
	  | _ -> true
    )  t.t_tbl 
  in
      match (Symset.elements  enum_searched ) with 
	[(wh, m, lc, Variable (str,_,_, no_st, ro))] -> 
	    t.t_tbl <- Symset.add (wh, m, lc, 
		Variable (str, newt , Some data, no_st, ro)) others
	| _ ->  error ("Replacing enumeration Value  failed")
	  

  
let add_subprogram s n v rt =
  add_subprogram (top s) n v rt (scope (top s))

let type_ovl_intersection s n1 n2 =
  let inter l1 l2 =
    let sym_eq (_,x) (_,y) = match (x,y) with
      | Variable (_,t1,_,_,_), Variable (_,t2,_,_,_) -> t1 = t2
      | a, b -> a = b
    in
      List.filter (fun x -> List.exists (fun y -> sym_eq x y) l1) l2
  in   
    (*  Needs  more work than only 'find_symbols (top s) n1'...
	see t415
    *)
  let all_find_symbols stack name = 
    let symb1 = Tree.fold (fun rf it ->
	List.append rf (find_symbols it name)) [] stack.s_stack in
    let context = ref (s_get_use stack) in 
    let res = ref [] in
      while (!context <> []) do
	let p = (List.hd !context) in 
	  context := List.tl !context;
	  match (tbl_find_unit (library stack.s_stack) p) with
	    | Some tbl -> res:=List.append !res (find_symbols tbl name)
	    | None     -> ()
      done;
      List.append symb1 !res
  in
  let s1 = all_find_symbols s n1 in
  let s2 = all_find_symbols s n2 in
   
  let inte = inter s1 s2 in
  let print_set set = "{"
                    ^ String.concat ", " (List.map (fun (_,x) ->
                                  print_symbol_join x)
                      set)
                    ^ "}"
  in
  Npkcontext.print_debug ( "Type_ovl_intersection : result = "
                         ^ print_set inte
                         ^ (if inte <> [] then ""
                            else
                              " , L = " ^ print_set s1 ^
                              " , R = " ^ print_set s2
                           )
                         );
    if inte = [] then 
      Some (T.new_unknown "from empty context intersection") 
    else
      try
	match snd (cast_v inte) with
	  | Variable (_,r,_,_,_) -> Some r
	  | _ -> invalid_arg "type_ovl_inter"
      with Not_found -> None
	
