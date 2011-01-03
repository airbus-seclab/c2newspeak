open Newspeak

module Set = Set.Make(String)

type t = (string, Set.t) Hashtbl.t

let compute prog =
  let callgraph = Hashtbl.create 100 in
  let add_call f g =
    try
      let callers = Hashtbl.find callgraph g in
	Hashtbl.replace callgraph g (Set.add f callers)
    with Not_found -> Hashtbl.add callgraph g (Set.singleton f)
  in
  let current_function = ref "" in

  let rec compute_blk x = List.iter compute_stmt x

  and compute_stmt (x, _) =
    match x with
	Decl (_, _, body) | InfLoop body | DoWith (body, _) -> 
	  compute_blk body
      | Select (branch1, branch2) ->
	  compute_blk branch1;
	  compute_blk branch2
      | Call (_, FunId g, _) -> add_call !current_function g
      | Call (_, FunDeref _, _) -> 
	  Utils.print_info "Function pointer dereference ignored"
      | _ -> ()
  in
  let compute_fundec f declaration =
    current_function := f;
    compute_blk declaration.body
  in

    Hashtbl.iter compute_fundec prog.fundecs;
    callgraph

let get_callers callgraph f =
  try Set.elements (Hashtbl.find callgraph f)
  with Not_found -> []
