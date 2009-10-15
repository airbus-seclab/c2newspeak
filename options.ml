
type t =
  | Cfg_only
  | Verbose

let options = ref []

let set x _ =
  options := x :: !options

let get x =
  List.mem x !options
