let rec filter_map f = function
  |  []  -> []
  | h::t -> begin
              match f h with
              | None   ->    filter_map f t
              | Some r -> r::filter_map f t
            end

let f ln vertices x =
  Array.init (ln + 1) ( fun i ->
    (* meet ( f(origin) / (origin, dest, f) in v / dest = i) *)
    let from_vals = filter_map (fun (origin, dest, (_,f)) ->
      if (dest = i) then Some (f (x.(origin)))
                    else None
    ) vertices in
    List.fold_left Range.meet Range.bottom from_vals
  )

let rec kleene f x =
  let fx = f x in
  if fx = x then
    x
  else kleene f fx

let solve (ln, v) =
  let x0 = Array.make (ln + 1) Range.top in
  kleene (f ln v) x0
