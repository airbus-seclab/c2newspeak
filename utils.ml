
let rec filter_map f = function
  |  []  -> []
  | h::t -> begin
              match f h with
              | None   ->    filter_map f t
              | Some r -> r::filter_map f t
            end

let may f = function
  | None   -> None
  | Some x -> Some (f x)
