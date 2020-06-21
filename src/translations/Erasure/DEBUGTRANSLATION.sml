
signature DEBUGTRANSLATION = sig
  type ctx

  type var = BlockedCps.Abt.var
  type kind = BlockedCps.Abt.kind
  type con = BlockedCps.Abt.con
  type exp = BlockedCps.Abt.exp
  type value = BlockedCps.Abt.value
  type block = BlockedCps.Abt.block

  val emptyCtx : unit -> ctx

  val lookupType : ctx -> var -> con

  val extendType : ctx -> var -> con -> ctx

  val extendTypes : ctx -> (var * con) list -> ctx

  val extendKind : ctx -> kind -> ctx

  val typeValueSynth : ctx -> value -> con

  val typeBlockSynth : ctx -> block -> con

  val weakHeadNormalize : ctx -> con -> con
end
