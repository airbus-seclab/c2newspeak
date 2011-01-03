open Newspeak

module Set = Set.Make(String)

type info = {
  callers: Set.t;
  file: string
}

type t = (string, info) Hashtbl.t

let compute prog =
  let callgraph = Hashtbl.create 100 in
  let add_call f g =
    try
      let info = Hashtbl.find callgraph g in
      let info = { info with callers = Set.add f info.callers } in
	Hashtbl.replace callgraph g info
    with Not_found -> 
      let info = { file = "missing definition"; callers = Set.singleton f } in
	Hashtbl.add callgraph g info
  in
  let current_function = ref "" in

  let init f declaration = 
    let (filename, _, _) = declaration.position in
    let info = 
      {
	callers = Set.empty;
	file = filename
      }
    in
      Hashtbl.add callgraph f info
  in

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

    Hashtbl.iter init prog.fundecs;
    Hashtbl.iter compute_fundec prog.fundecs;
    callgraph

let get_callers callgraph f =
  let info = Hashtbl.find callgraph f in
    Set.elements info.callers
	
let get_position callgraph f =
  let info = Hashtbl.find callgraph f in
    info.file
  
