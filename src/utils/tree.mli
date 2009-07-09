
module type TREE = sig
  type 'a t
  val create : unit -> 'a t
  val push   : 'a -> 'a t -> unit
  val pop    : 'a t -> unit
  val top    : 'a t -> 'a
  val lookup : ('a -> 'b option) -> 'a t -> 'b option
  val iter   : ('a -> unit) -> 'a t -> unit
  val fold   : ('res -> 'a -> 'res) -> 'res -> 'a t -> 'res
  val height : 'a t -> int
  val nth    : 'a t -> int -> 'a
end

module StackedTree : TREE
module FCNSTree    : TREE

