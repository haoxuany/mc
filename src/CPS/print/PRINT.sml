
signature PRINT = sig
  type var
  type kind
  type con
  type value
  type exp

  type t

  val serializeKind : kind -> t
  val serializeCon : con -> t
  val serializeValue : value -> t
  val serializeExp : exp -> t

  val printKind : kind -> unit
  val printCon : con -> unit
  val printValue : value -> unit
  val printExp : exp -> unit
end
