
signature CONTEXT = sig
  type var
  type kind
  type con
  type value
  type exp
  type block
  type program

  type t

  val new : unit -> t
  val extendKind : t -> kind -> t
  val extendType : t -> var -> con -> t

  val lookupKind : t -> int -> kind
  val lookupType : t -> var -> con
end
