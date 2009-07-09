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

module StackedTree : TREE = struct
  type 'a t = 'a Stack.t

  let create = Stack.create

  let push = Stack.push

  let top = Stack.top

  let pop x = ignore (Stack.pop x)

  let lookup p s =
    let s = Stack.copy s in
    let r = ref None in
    while (!r = None && not (Stack.is_empty s)) do
      let t = Stack.pop s in
        r := p t
    done;
    !r

  let iter = Stack.iter

  let height = Stack.length

  let nth s n =
    let s = Stack.copy s in
    while (height s > n) do
      pop s;
    done;
    top s

  let fold f init s =
    let r = ref init in
    iter (fun x -> r := f !r x) s;
    !r

end


module FCNSTree : TREE = struct

  type 'a t = 'a tree ref

  and 'a tree = 'a node option

  and 'a node = {         fcns_contents    : 'a
                ; mutable fcns_parent      : 'a tree
                ; mutable fcns_firstchild  : 'a tree
                ; mutable fcns_nextsibling : 'a tree
                }

  let create _ =
    ref None
 
  let iter f t =
    let rec iter_tree f t =
      match t with
      | None   -> ()
      | Some n -> begin
                    f n.fcns_contents;
                    iter_tree f n.fcns_parent
                  end
    in
    iter_tree f !t
  
  let fold f init s =
    let r = ref init in
    iter (fun x -> r := f !r x) s;
    !r

  let top x =
    match !x with
    | None -> invalid_arg "FCNSTree.top"
    | Some n -> n.fcns_contents

  let height x = fold (fun r _ -> succ r) 0 x

  let nth x n =
    let rec nth_tree x n =
      match x with
      | None    -> invalid_arg "FCNSTree.nth"
      | Some nd -> begin
                     if (n = 1) then
                       nd
                     else
                       nth_tree nd.fcns_parent (pred n)
                   end
    in
    let r = nth_tree !x n in
    r.fcns_contents

  let lookup p x =
    let rec lookup_tree p x =
      match x with
       | None  -> None
       | Some n -> begin
                     match p n.fcns_contents with
                     | Some x -> Some x
                     | None   -> lookup_tree p n.fcns_parent
                   end
    in
    lookup_tree p !x

  let pop x =
    match !x with
    | None  -> invalid_arg "FCNSTree.pop"
    | Some n -> x := n.fcns_parent

  let push (e:'a) (x:'a t) :unit =
    let newnode = { fcns_contents    = e
                  ; fcns_parent      = !x
                  ; fcns_firstchild  = None
                  ; fcns_nextsibling = None
                  }
    in
    (* Compute NS(NS(NS(...(x)))) -- 0 times or more until the node has no NS *)
    let rec nsstar (x:'a node) :'a node =
      match x.fcns_nextsibling with
      | None   -> x
      | Some s -> nsstar s
    in
    let push_node (x:'a node) =
      begin
        match x.fcns_firstchild with
        | None    -> x.fcns_firstchild             <- Some newnode
        | Some fc -> (nsstar(fc)).fcns_nextsibling <- Some newnode
      end
    in
    begin
      match !x with
      | None      -> ()
      | Some node -> push_node node
    end;
    x := Some newnode
end



