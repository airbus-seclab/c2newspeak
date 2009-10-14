
type t

val top : t

val bottom : t

(* a C b *)
val (<=%) : t -> t -> bool

(* a \/ b *)
val join : t -> t -> t

(* a /\ b *)
val meet : t -> t -> t

val to_string : t -> string
