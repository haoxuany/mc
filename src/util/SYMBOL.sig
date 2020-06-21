
signature SYMBOL = sig
  type t

  val fresh : string -> t
  val compare : t * t -> order
  val eq : t * t -> bool

  val name : t -> string
end
