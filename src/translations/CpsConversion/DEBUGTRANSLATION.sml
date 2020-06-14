
signature DEBUGTRANSLATION = sig
  type ctx

  type var = FOmegaS.Abt.var
  type kind = FOmegaS.Abt.kind
  type con = FOmegaS.Abt.con
  type term = FOmegaS.Abt.term

  val emptyCtx : unit -> ctx

  val lookupType : ctx -> var -> con

  val extendType : ctx -> var -> con -> ctx

  val extendKind : ctx -> kind -> ctx

  val weakHeadNormalize : ctx -> con -> con

  val debug : ctx -> term -> con
    (* k,    kexn *)
    -> var -> var
    -> Cps.Abt.exp -> Cps.Abt.con
    -> string (* name to report *)
    -> unit
end
