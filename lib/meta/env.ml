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

  val count : 'a tree -> int

  val depth : 'a tree -> int
end

(** This makes a really crude binary tree. No effort made whatsoever to keep it
    balanced etc. *)
module MakeEnv (K : Key) : Env with type key = K.t = struct
  type key = K.t

  type 'a tree =
    | Empty
    | Node of { k : key; v : 'a; left : 'a tree; right : 'a tree }

  let make_node k v left right = Node { k; v; left; right }

  let empty () = Empty

  type comp = LT | GT | EQ

  let compare x y =
    let n = K.compare x y in
    if n < 0 then LT else if n > 0 then GT else EQ

  (** If the key already exists, this will just overwrite it.*)
  let rec insert tree key value =
    match tree with
    | Empty -> make_node key value Empty Empty
    | Node { k; v; left; right } -> (
        match compare key k with
        | EQ -> make_node k value left right
        | LT -> make_node k v (insert left key value) right
        | GT -> make_node k v left (insert right key value) )

  let rec get tree key =
    match tree with
    | Empty -> None
    | Node { k; v; left; right } -> (
        match compare key k with
        | EQ -> Some v
        | LT -> get left key
        | GT -> get right key )

  let rec count tree =
    match tree with
    | Empty -> 0
    | Node { left; right; _ } -> 1 + count left + count right

  let rec depth tree =
    match tree with
    | Empty -> 0
    | Node { left; right; _ } -> 1 + max (depth left) (depth right)
end
