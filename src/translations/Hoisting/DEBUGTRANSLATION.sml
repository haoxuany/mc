
signature DEBUGTRANSLATION = sig
  type ctx

  type var = Cps.Abt.var
  type kind = Cps.Abt.kind
  type con = Cps.Abt.con
  type exp = Cps.Abt.exp
  type value = Cps.Abt.value

  val emptyCtx : unit -> ctx

  val lookupType : ctx -> var -> con

  val extendType : ctx -> var -> con -> ctx

  val extendTypes : ctx -> (var * con) list -> ctx

  val extendKind : ctx -> kind -> ctx

  val typeValueSynth : ctx -> value -> con

  val weakHeadNormalize : ctx -> con -> con

  val kinds : ctx -> kind list
end
