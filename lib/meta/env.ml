module type Key = sig
  type t

  val compare : t -> t -> int
end

module type Env = sig
  type key

  type +'a tree

  val empty : unit -> 'a tree

  val insert : 'a tree -> key -> 'a -> 'a tree

  val get : 'a tree -> key -> 'a option
end

module MakeEnv (K : Key) : Env with type key = K.t = struct
  type key = K.t

  type 'a tree =
    | Empty
    | Node of { k : key; v : 'a; left : 'a tree; right : 'a tree }

  let make_node k v left right = Node { k; v; left; right }

  let empty () = Empty

  let rec insert tree key value =
    match tree with
    | Empty -> make_node key value Empty Empty
    | Node { k; v; left; right } -> (
        match K.compare key k with
        | -1 -> make_node k v (insert left key value) right
        | 1 -> make_node k v left (insert right key value)
        | _ -> make_node k value left right )

  exception Blah

  let rec get tree key =
    match tree with
    | Empty -> None
    | Node { k; v; left; right } -> (
        match K.compare key k with
        | 0 -> Some v
        | -1 -> get left key
        | 1 -> get right key
        | _ -> raise Blah )
end
