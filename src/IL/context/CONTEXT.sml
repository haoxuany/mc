
signature CONTEXT = sig
  type var
  type kind
  type con
  type term
  type sg
  type module

  type t

  val new : unit -> t
  val extendKind : t -> kind -> t
  val extendType : t -> var -> con -> t
  val extendSg : t -> var -> sg -> t

  val lookupKind : t -> int -> kind
  val lookupType : t -> var -> con
  val lookupSg : t -> var -> (con * sg) (* alpha * signature *)
end
