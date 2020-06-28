
signature PRINT = sig
  type var
  type kind
  type con
  type term
  type sg
  type module

  type t

  val serializeKind : kind -> t
  val serializeCon : con -> t
  val serializeTerm : term -> t
  val serializeSg : sg -> t
  val serializeModule : module -> t

  val printKind : kind -> unit
  val printCon : con -> unit
  val printTerm : term -> unit
  val printSg : sg -> unit
  val printModule : module -> unit
end
