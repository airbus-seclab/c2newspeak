
type t = (int * int) option

let from_bounds a b =
  assert (a <= b);
  Some (a, b)

let top = Some (min_int, max_int)

let bottom = None

let (<=%) a b = match (a, b) with
  | None, _ -> true
  | Some _, None -> false
  | Some (a, b), Some (c, d) -> a <= c && b <= d

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
        else if l2 < u1 then
          begin
            if u2 <= u1 then      from_bounds l2 u2
            else                  from_bounds l2 u1
          end
        else                      None
      end

let to_string =
  let int_inf_to_string x =
    if x = max_int then "+oo"
    else if x = min_int then "-oo"
    else string_of_int x
  in function
  | None        -> "(bot)"
  | Some (a, b) when a = b -> "{"^string_of_int a^"}"
  | Some (a, b)            -> "["^int_inf_to_string a^";"
                                 ^int_inf_to_string b^"]"

