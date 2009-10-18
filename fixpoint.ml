
let f ln vertices x =
  Array.init (ln + 1) ( fun i ->
    (* join ( f(origin) / (origin, dest, f) in v / dest = i) *)
    let from_vals = Utils.filter_map (fun (origin, dest, (_,f)) ->
      if (dest = i) then Some (f (x.(origin)))
                    else None
    ) vertices in
    List.fold_left Range.join x.(i) from_vals
  )

let inline_print v =
  String.concat ", " (Array.to_list (Array.mapi
    (fun i x ->
      string_of_int i ^ "--> " ^ Range.to_string x
    ) v
  ))
    

let rec kleene n f x =
  if (Options.get_verbose ()) then
    prerr_endline ("Iteration #"^string_of_int n^" : "^inline_print x);
  let fx = f x in
  if fx = x then
    x
  else kleene (succ n) f fx

let widen _ =
  invalid_arg "Widening is not implemented yet"

let solve (ln, v) =
  let x0 = Array.make (ln + 1) Range.bottom in
  x0.(ln) <- Range.from_bounds 0 0;
  let g =
    if (Options.get_widening ()) then
      widen f
    else
      f
  in
  kleene 0 (g ln v) x0
