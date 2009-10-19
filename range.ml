
type t = (int * int) option

let from_bounds a b =
  assert (a <= b);
  Some (a, b)

let top = Some (min_int, max_int)

let bottom = None

let (<=%) a b = match (a, b) with
  | None, _ -> true
  | Some _, None -> false
  | Some (a, b), Some (c, d) -> c <= a && b <= d

let join a b = match (a, b) with
  | None, _ -> b
  | _, None -> a
  | Some (l1, u1), Some (l2, u2) ->
      Some (min l1 l2, max u1 u2)

let meet a b = match (a, b) with
  | None, _ -> None
  | _, None -> None
  | Some (l1, u1), Some (l2, u2) ->
      begin
        if l2 <= l1 then
          begin
            if u2 < l1 then       None
            else if u2 <= u1 then from_bounds l1 u2
            else                  from_bounds l1 u1
          end
        else if l2 <= u1 then
          begin
            if u2 <= u1 then      from_bounds l2 u2
            else                  from_bounds l2 u1
          end
        else                      None
      end

let widen a b =
  match (a, b) with
  | None         , _             -> None
  | _            , None          -> None
  | Some (l1, u1), Some (l2, u2) -> let l = if l2 < l1
                                      then min_int
                                      else l1
                                    in
                                    let u = if u2 > u1
                                      then max_int
                                      else u1
                                    in
                                    Some (l, u)

let is_infinite x =
  x == max_int || x == min_int

let add_overflow n x =
  if (is_infinite x) then x
  else x + n

let shift n = function
  | None        -> None
  | Some (a, b) -> Some (add_overflow n a, add_overflow n b)

let add_bound ?(min=min_int) ?(max=max_int) =
  meet (from_bounds min max)

let to_string =
  let string_of_int_inf x =
    if x = max_int then "+oo"
    else if x = min_int then "-oo"
    else string_of_int x
  in function
  | None        -> "(bot)"
  | Some (a, b) when a = min_int && b = max_int -> "(top)"
  | Some (a, b) when a = b -> "{" ^ string_of_int     a ^ "}"
  | Some (a, b)            -> "[" ^ string_of_int_inf a ^ ";"
                                  ^ string_of_int_inf b ^ "]"

