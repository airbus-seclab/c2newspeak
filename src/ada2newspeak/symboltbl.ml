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
open AdaSyntax

module T = AdaTypes

type scope = Lexical | In_package of string

exception Parameterless_function of scope * string * T.t
exception Variable_no_storage    of T.t * T.data_t

module Table = 
struct
  module Symset : 
  sig
    type 'a t
      
    val empty : 'a t
    val iter : ('a -> unit) -> 'a t -> unit
    val elements : 'a t -> 'a list
    val add : 'a -> 'a t -> 'a t
    val filter : ('a -> bool) -> 'a t -> 'a t
  end = 
  struct
    
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
    | Subprogram of (string * (AdaSyntax.param list) * T.t option)
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

  let print_symbol x = 
    match x with
      | Variable  (_,t,None,_,_)   -> "V",T.print t
      | Variable  (_,t,Some v,_,_) -> 
	  "V *","(" ^ T.print_data v ^ ")" ^ T.print t
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
              | Some r -> 
		  if List.exists (fun x -> p' x <> None) t
                  then None
		  else Some r
          end

  let mkcast desc fn ?(filter = fun _ -> true) lst  =
    let fn' (wh,x) =
      match fn x with
	| None -> None
	| Some y -> 
	    Some (wh,y)
    in
      match extract_unique ~filter fn' lst with
	| None ->     
	    Npkcontext.report_error "Symboltbl.mkcast" 
	      ("Ambiguous " ^ desc ^ " name") 
	| Some x -> x


  let mkcast_no_error fn lst  =
    let subps = List.filter ( fun (_,x) -> 
	match fn x with None -> false | _-> true 
			    ) lst
    in
      match subps with
	  [] -> false
	| _ -> true
     

  let cast_v ?filter = 
   mkcast "variable"
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
		       
  let cast_s_no_error  = 
    mkcast_no_error (function Subprogram x -> Some x | _ -> None) 
   



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

     
 let tbl_find_subprogram n_args xpect my_find tbl n =
    let match_ret_typ xpect z =
      match (xpect, z) with
    	  Some xpec, Some ret -> T.is_compatible xpec ret
    	| _ -> true
    in
    let filter_args norm_args (_, (_, params, _))  =
       let argtbl = Hashtbl.create 5 in
       let rec extract_positional_parameters ar =
	match ar with
	  | []   -> []
	  | (Some  _, _)::_  ->
	      let process_argument (x, typ) =
		match x with
		  | None -> 
		      Npkcontext.report_error "tbl_find_subprogram "
			"Named parameters shall follow positional ones"
		  | Some id when Hashtbl.mem argtbl id ->
                      Npkcontext.report_error "tbl_find_subprogram "
			("Parameter " ^ id ^ " appears twice")
		  | Some id -> Hashtbl.add argtbl id typ
	      in
		List.iter process_argument ar;
		[]
	  | (None, ty)::tl -> ty::(extract_positional_parameters tl)
      in
       let make_subt arg  =
	 let  n =  match arg.param_type with
    	   | [] -> Npkcontext.report_error
    	       "symboltbl:find_subprogram" "unreachable"
    	   | x::[]    -> None  , x 
	   | x::y::[] -> Some x, y
    	   | _ -> Npkcontext.report_error "symboltbl:find_subprogram"
    	       "chain of selected names is too deep"
    	 in try 
	     begin 
	       try snd (my_find n)
    	       with Not_found -> Npkcontext.report_error
    		 ("Cannot   find type '") "" end
    	   with Not_found ->
    	     Npkcontext.report_error "Symbtbl:not found" ""
       in
       let rec are_compatible pos_list spec =
         match pos_list, spec with
           |  [], _  -> (* end of positional parameters *)
		List.for_all ( function x -> 
		  try    
		    let typo  = Hashtbl.find argtbl x.formal_name in
		      T.is_compatible typo (make_subt x) 
		  with Not_found -> true
			     ) spec
		  
           | tp::pt, s::st ->
	       let subtyp = make_subt s in
	 	 ( T.is_compatible tp subtyp) &&
		   ( are_compatible pt st)
		   
          | _::_,[]     -> 
	      Npkcontext.report_error "normalize.function_call"
              "Too many actual arguments in function call"
      in
      let pos = extract_positional_parameters norm_args in
	are_compatible pos params 
    in
    let subs = find_symbols tbl n in
      (cast_s 
	 ~filter:(fun x -> let (_, (_sn, _, z)) = x in
		    ( filter_args n_args x)
		    && ( match_ret_typ xpect z) 
		 ) 
	 subs
	)


  let tbl_find_variable tbl ?expected_type n =
    let ovl_predicate = 
      match expected_type with
	| Some t when not (T.is_unknown t) ->
	             (fun x -> T.is_compatible x t)
	| _ -> (fun _ -> true)
    in
      try
	let (s,sym) =
          cast_v  ~filter:(
	     function
               | (_,(Variable (_,x,_,_,_)))      -> ovl_predicate x
               | (_,(Subprogram(_, [], Some x))) -> ovl_predicate x               
	       | _ -> true
                           ) (find_symbols tbl n)
	in
	let (n,t,v,r) = 
	  match sym with
            | Variable (n,x,      v, false, r) -> (n, x, v, r)
            | Variable (_,x, Some v, true , _) ->
		raise (Variable_no_storage (x, T.get_enum_litt_value x v))
            | Subprogram (n,[], Some rt)     ->
		raise (Parameterless_function (s, n, rt))
            | _ -> Npkcontext.report_error "find_variable" "unreachable"
	in
	  s, (n, t, v, (match v with Some _ -> true | None -> r))
      with
	| Parameterless_function (Lexical, n, rt) ->
            raise (Parameterless_function (tbl.t_scope, n, rt))
	  
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
	   (*; mutable s_with     : string list*)
         ; mutable s_renaming : ( (string option * string) *
		((string option * string) * 
		   (*arguments and  for subprogram*)
		   (AdaSyntax.param list)option
		) list
				) list
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
(*  ; s_with     = ["system";"machine_code";"unchecked_conversion"]*)
  ; s_renaming = []
  }

let set_current s x = s.s_cpkg <- Some x

let reset_current s = s.s_cpkg <- None

let current s = s.s_cpkg

(*
let add_with s x = s.s_with <- x::s.s_with
let is_with  s x = List.mem x s.s_with
*)
let is_ada_pck p = List.exists (fun x -> compare x p = 0) 
    ["system";"machine_code";"unchecked_conversion"]


let add_use is_with s p =
  (*if (compare p "trigonometric" = 0) then
    Npkcontext.report_error "Symboltbl.add_use"
    ( let (s,i,_j)  =  Npkcontext.get_loc() in
			 s^" "^(string_of_int i)
		     )
  ;*)

  if (is_with || is_ada_pck p ||  (current s = Some p)) then
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

let eq_renaming  (p1, n1) (p2, n2) = 
     ( compare n1 n2 = 0) &&
     (match p1, p2 with 
	    Some a, Some b -> compare a b = 0 
	  | _ -> false
     ) 
    
let add_renaming_decl s (current, new_name) new_args old_name  =
  (*Making sure old name can later be found *)
  let is_None x = match x with None -> true | _ -> false 
  in
    if ((is_None current) || (is_None (fst old_name) )) then 
      Npkcontext.report_error "add_renaming_decl"
	( "None package is specified" ^ new_name ^ "'.")
    ;
    if ((None,new_name) = old_name) then
      Npkcontext.report_error "add_renaming_decl"
	( "Circular declaration detected for '" ^ new_name ^ "'.")
    else
     
      (*Multiple renaming is possible*)     
      if (List.exists (fun (key, _) -> 
	eq_renaming key (current, new_name)) s.s_renaming)
      then
      begin
	Npkcontext.report_warning "add_renaming_decl"
	  ( "Already renamed '"^ new_name ^ "' but added");
	let founds, removeds =  List.partition ( fun (key,_) -> 
	  eq_renaming key (current, new_name)) s.s_renaming 
	in
	let bindings =  match founds with
	    hd::[] -> snd hd
	  | _ -> 
	      Npkcontext.report_error "Symbtbl.add_renaming_decl" 
		("Too many bining s for "^ new_name )
	in
	  s.s_renaming <- (  (current, new_name) 
			   , ((old_name, new_args)::bindings)
			  )::removeds
			  
      end
    else
      (* Else (and only in this case) add it*)
      begin
	Npkcontext.print_debug ( "renaming_declaration : "
				 ^(match current with
				       Some e -> e | _ -> "?")
				 ^ ("."^new_name)
				 ^ " --> "
				 ^ (match (fst old_name) with
				      | None   -> ""
				      | Some p -> p ^ "."
				   )
				 ^ (snd old_name)
                               );
	s.s_renaming <- 
	  ((current, new_name),[(old_name, new_args)])::s.s_renaming
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
                  if (Tree.height s.s_stack <> 1) then begin
		    Npkcontext.report_error "Symbtbl.enter_context" 
		      "Adding some unit outside the library"
		  end;
                  let top = Tree.top s.s_stack in
                    top.t_tbl <- 
		      Symset.add 
		      (Lexical, n, Newspeak.unknown_loc, Unit new_context)
                      top.t_tbl
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

(*Only used for renaming (for is_overloaded function)*)
let is_already_defined tbl n = 
   cast_s_no_error (find_symbols (top tbl) n)
     
let s_find_abs _desc f s p n =
  match tbl_find_unit (library s.s_stack) p with
    | Some tbl -> f tbl n
    | None     -> raise Not_found
      

let s_find desc finder s ?package n =
  match package with
    | Some p -> s_find_abs desc finder s p n
    | None  -> 
        begin
         try
           find_rec s.s_stack (fun t ->
             try Some (finder t n)
             with Not_found ->
	       None
           )
         with Not_found ->  
	   begin
             let context = ref (s_get_use s) in
             let res = ref None in
             while (!context <> [] && !res = None) do
               try
                 let p = (List.hd !context) in
	           res := Some (s_find_abs desc finder s p n);
               with Not_found ->
		 context := List.tl !context;
             done;
             match !res with
             | None ->raise Not_found 
             | Some (p,v) -> (p,v) 
           end
       end

let find_variable_value s ?expected_type (package,n) =
  s_find "variable" (fun tbl n -> tbl_find_variable tbl ?expected_type n)
    s ?package n

let find_variable s ?expected_type name =
  let rec find name =
    try
      let (_, x) = 
	List.find (fun (key, _) -> eq_renaming key name) s.s_renaming
      in
	match x with 
	    (nam_assoc, _)::[] -> find nam_assoc
	  |  _ ->
	       Npkcontext.report_warning "find_variable"
		 ( "Already renamed '"^(snd name)
		   ^"': not implemented for variable"); 
	       raise Not_found
    with Not_found ->
      let (x, (n, y, _, z)) = find_variable_value s ?expected_type name in
	(x, (n, y, z))
  in
    find name

let report_variable_not_found (_, n) = 
  Npkcontext.report_error "Symboltbl.report_variable_not_found" 
    ("Cannot find variable '"^n^"'")

let find_variable_with_error_report s ?expected_type name =
  try find_variable s ?expected_type name
  with Not_found -> report_variable_not_found name

let find_variable_value s ?expected_type name =
  try find_variable_value s ?expected_type name
  with Not_found -> report_variable_not_found name
	
let rec find_type s (package, n) = 
  try
    let x = snd ( List.find (fun (key, _) -> 
		  eq_renaming key (package,n) ) s.s_renaming)
    in  
      match x with 
	  (nam_assoc, _)::[]  ->  begin
	    find_type s nam_assoc
	  end
	|  _ -> begin  
	     Npkcontext.report_warning "find_type"
	       ( "Already renamed typr '"^n^ "' not implemented for type"); 
	     raise Not_found 
	   end
  with Not_found -> 
    s_find "type" tbl_find_type s ?package n


let rec find_subprogram_aux s ?(silent=false) (pack,n) n_args xpect t_find use  =
  let find_one_renaming  ((p_opt, n_a), params_opt) = 
    if (eq_renaming  (p_opt, n_a) (pack,n)) then 
      Npkcontext.report_error "Symboltbl.find_subprogram" 
	("program '" ^ n ^ "'" ^"can not be resolved")
    else 
      let (sc,(act_name,_, top)) =
	find_subprogram_aux s ~silent (p_opt, n_a) n_args  xpect t_find false
      in
	match params_opt with 
	    Some formals ->  (sc,(act_name, formals, top))
	  | None -> Npkcontext.report_error "Symboltbl.find_subprogram" 
	      ("program renamed'" ^ n ^ 
		 "'" ^"can not find previous formal parameters")
  in
  let res = ref[] in 
  let multiple_renaming x = 
    try
      let new_spec = find_one_renaming x  in
	res:=new_spec::(!res)
    with _ -> ()
  in
    

  (*Looking in use is used in order to look for function in use clause;
    it is only one level deep *)
  let looking_in_use pack use_enabled  tbl_find s find_auxiliary =  
    let find_pck n  = 
      match pack with 
	  Some p ->  s_find "subprogram" tbl_find s ~package:(p) n 
	| _ ->       s_find "subprogram" tbl_find s n
    in
     let context_orig = ref (s_get_use s) in
     let context = match pack with 
	 Some p -> ref (List.find_all ( fun x -> 
		(compare x p <> 0) && ( compare x "standard" <> 0) 
				      ) !context_orig 
		       ) 
       | _ -> context_orig
     in        
       try (find_pck n)
       with Not_found ->
	 try
           if use_enabled then
	     let res = ref None in
	       while (!context <> [] && !res = None) do	  
		 try
		   let p = (List.hd !context) in
		     let sub = find_auxiliary (Some p, n) in
		       res := Some (sub);
		 with Not_found -> 
		   context := List.tl !context;
	       done;
	       match !res with
		   Some prog ->  prog
		 | _ ->  
		     if silent then raise Not_found
		     else 
		       Npkcontext.report_error 
			 "Symboltbl" 
			 ("Cannot find subprogram '" ^ n ^ "'")
			 
	   else 
	     raise Not_found	     
	 with Not_found ->
	   if silent then
	     raise Not_found
	   else 
	     Npkcontext.report_error  "Symboltbl.multiple_renaming" 
	       ("Cannot find subprogram '" ^ n ^ "'")   
  in
    
    (*Beginning of the function *)
    try 
      let names = snd (
	List.find (fun (key, _) -> eq_renaming key (pack,n)) 
	  s.s_renaming) 
      in
	List.iter multiple_renaming names;
	let nb_solution = List.length !res in 
	  if (compare  nb_solution 0 = 0) then
	    raise Not_found
	  else if (compare nb_solution 1 > 0) then begin
	    Npkcontext.report_error "Symboltbl.multiple_renaming"
	      ("More than one program match spec:'" ^ n ^ "'")
	  end else
	    List.hd (!res)
	      
    with Not_found -> 
      let tbl_find = 	(tbl_find_subprogram n_args xpect t_find) in
      (*silent = true pour forcer la recursion dans les use*)
      let recursive_call = (fun x -> 
	find_subprogram_aux s ~silent:true x n_args  xpect t_find false)
      in
	looking_in_use pack use tbl_find s recursive_call
	
let find_subprogram s ?(silent=false) (pack,n) norm_args xpect t_find =
  find_subprogram_aux s ~silent:(silent) (pack,n) norm_args xpect t_find true

let is_operator_overloaded s n =
  try 
    begin  
      ignore (s_find "overloaded operator" tbl_find_subprogram_simple s n);
      true
    end
  with Not_found -> 
    List.exists (fun ((_, key), _) -> compare  key n = 0) s.s_renaming
      
let scope t = t.t_scope

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
      | _ -> 
	  Npkcontext.report_error "Symboltbl.replace_type"
	    ("Replacing enumeration type failed "^n^
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
	| _ -> 
	    Npkcontext.report_error "Symboltbl" 
	      ("Replacing enumeration Value  failed")
	 
let add_subprogram s n v rt =
  add_subprogram (top s) n v rt (scope (top s))

let rec make_name_of_lval lv =
  match lv with
    | Var x -> [x]
    | SName (pf, tl) -> make_name_of_lval pf @ [tl]
    | _ -> invalid_arg "make_name_of_lval"


let get_possible_common_type s lval1 lval2 =
  let inter l1 l2 =
    let sym_eq (_,x) (_,y) = 
      let extract_typ symbol = 
	match symbol with  
	    Variable (_,t,_,_,_) -> Some t
	  | Subprogram (_, _, t) -> t
	  | _ -> Npkcontext.report_error "get_possible_common_type"
	      "Unexpected case in get_possible_common_type"
      in
	match  extract_typ x,  extract_typ y with
	    Some a, Some b -> a = b 
	  | _ -> false
    in
      List.filter (fun x-> List.exists (fun y-> sym_eq x y) l1) l2
  in   
  let all_find_symbols stack pk_opt name = 
    let symb1 = 
      Tree.fold (fun rf it ->
	List.append rf (find_symbols it name)) [] stack.s_stack 
    in
    let uses = s_get_use stack in
    let context =
      match  pk_opt with 
	  None ->  ref (uses) 
	| Some pck -> 
	    if  (List.mem pck uses) then 
	      ref (uses) 
	    else
	      ref (pck::uses) 
    in 
    let res = ref [] in
      while (!context <> []) do
	let p = (List.hd !context) in
	  context := List.tl !context;
	  match (tbl_find_unit (library stack.s_stack) p) with
	    | Some tbl ->
		res:= List.append !res (find_symbols tbl name)
	    | None     -> ()
      done;
      List.append symb1 !res
  in
    try
      (* Hack: try to add a package name for find_all*)
      let try_add_pck strl =
	match strl with 
	    x::[]    -> None   , x
	  | y::x::[] -> Some(y), x
	  | _        -> None   , ListUtils.last strl
      in
      let p1, n1 = try_add_pck (make_name_of_lval lval1) in
      let p2, n2 = try_add_pck (make_name_of_lval lval2) in
      let s1 = all_find_symbols s p1 n1 in
      let s2 = all_find_symbols s p2 n2 in
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
	if inte = [] 
	then Some (T.new_unknown "from empty context intersection") 
	else begin
	  match snd (cast_v inte) with
	    | Variable (_,r,_,_,_) -> Some r
	    | _ -> None
	end
    with _ -> None
      
