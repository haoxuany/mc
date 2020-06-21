
signature PRINT = sig
  type var
  type value
  type exp
  type block
  type program

  type t

  val serializeValue : value -> t
  val serializeExp : exp -> t
  val serializeBlock : block -> t
  val serializeProgram : program -> t

  val printValue : value -> unit
  val printExp : exp -> unit
  val printBlock : block -> unit
  val printProgram : program -> unit
end
