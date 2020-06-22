
signature PRINT = sig
  type sym
  type macro
  type ctype
  type state
  type exp
  type decl
  type cfile

  type stream = TextIO.outstream

  val printMacro : stream -> macro -> unit
  val printCType : stream -> ctype -> unit
  val printState : stream -> int -> state -> unit
  val printExp : stream -> int -> exp -> unit
  val printDecl : stream -> decl -> unit
  val printCFile : stream -> cfile -> unit
end
