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

module MakeEnv (K : Key) : Env with type key = K.t
