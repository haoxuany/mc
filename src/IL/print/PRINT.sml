
signature PRINT = sig
  type var
  type kind
  type con
  type term
  type sg
  type psg
  type module
  type lmodule

  type t

  val serializeKind : kind -> t
  val serializeCon : con -> t
  val serializeTerm : term -> t
  val serializeSg : sg -> t
  val serializePsg : psg -> t
  val serializeModule : module -> t
  val serializeLmodule : lmodule -> t

  val printKind : kind -> unit
  val printCon : con -> unit
  val printTerm : term -> unit
  val printSg : sg -> unit
  val printPsg : psg -> unit
  val printModule : module -> unit
  val printLmodule : lmodule -> unit
end
