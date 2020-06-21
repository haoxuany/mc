
signature PRINT = sig
  type var
  type kind
  type con
  type term

  type t

  val serializeKind : kind -> t
  val serializeCon : con -> t
  val serializeTerm : term -> t

  val printKind : kind -> unit
  val printCon : con -> unit
  val printTerm : term -> unit
end
