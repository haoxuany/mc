
signature CONTEXT = sig
  type var
  type kind
  type con
  type term
  type sg
  type psg
  type module
  type lmodule

  type t

  val new : unit -> t
  val extendKind : t -> kind -> t
  val extendType : t -> var -> con -> t
  val extendSg : t -> var -> sg -> t

  val lookupKind : t -> int -> kind
  val lookupType : t -> var -> con
  val lookupSg : t -> var -> (con * sg) (* fst * signature *)

  val concat : t -> t -> t
  val kinds : t -> kind list (* reversed *)
end
