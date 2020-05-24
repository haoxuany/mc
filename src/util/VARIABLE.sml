
signature VARIABLE = sig
  type t

  val new : unit -> t
  val eq : t * t -> bool
  val compare : t * t -> order
  val print : t -> string
end

