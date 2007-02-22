open Cil
open Cilutils
open Npkcontext
open Npkutils

let check_main_signature t =
  let (ret, args) = 
    match unrollType t with
	TFun (ret, Some args, _, []) -> (ret, args)
      | _ -> error ("Cil2newspeak.translate.explore: "
		    ^"main, should have a function type")
  in
    if (!verb_warnings) && (unrollType ret <> TInt (IInt, [])) 
    then print_warning "return type of 'main' is not 'int'";
    match args with
	[] -> ()
      | (_, arg1, [])::(_, arg2, [])::[] 
	  when (unrollType arg1 = TInt (IInt, []))
	    && (unrollTypeDeep arg2 = 
		TPtr (TPtr (TInt (IChar, []), []), []))
	    -> ()
      | _ -> error ("Invalid argument types for main: "
		    ^"authorized forms are main() and"
		    ^" main(int, char**)")


let translate x = 
  let fun_decls = ref [] in
  let proto_decls = ref [] in
  let glb_decls = ref [] in

  (* Exploration of Cil's "globals" *)
  let explore g = 
    let loc = get_globalLoc g in
      update_loc loc;
      match g with
	| GType (t, _) ->
	    if !verb_warnings then print_warning ("skip typedef "^t.tname)
	| GEnumTag (info, _) -> 
	    if !verb_warnings then print_warning ("skip enum "^info.ename)
	| GCompTag (c, _) -> 
	    if !verb_warnings then print_warning ("skip composite typedef "^c.cname)
	| GCompTagDecl (c, _) -> 
	    if !verb_warnings then print_warning ("skip composite declaration "^c.cname)
	| GPragma (a, _) when !ignores_pragmas -> 
	    print_warning ("Directive ignored: "
			   ^"unknown #pragma "^(string_of_attribute a))
	    
	| GVarDecl ({vname = name; vtype = TFun (ret,args,_,_)}, _) ->
	    proto_decls := ((name, ret, args), loc)::(!proto_decls)
	    
	| GFun (f, _) -> 
	    if (f.svar.vname = "main") then check_main_signature f.svar.vtype;
	    fun_decls := f::(!fun_decls)
	      
	| GVarDecl (v, _) -> 
	    glb_decls := ((v, false, None), loc)::(!glb_decls)
	    
	| GVar (v, {init = i}, _) -> 
	    glb_decls := ((v, true, i), loc)::(!glb_decls)
	    
	| _ -> error ("Cil2newspeak.translate.explore: global "
		      ^(string_of_global g)^" not supported")
  in

    List.iter explore x;
    (* TODO: remove List.rev !!! *)
    (List.rev !glb_decls, (List.rev !fun_decls, List.rev !proto_decls))
	  
