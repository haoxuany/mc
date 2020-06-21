
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

  val extendKind : ctx -> kind -> ctx

  val weakHeadNormalize : ctx -> con -> con

  val debugValue : ctx ->
    value -> con ->
    value -> con ->
    string ->
    unit

  val debugExp : ctx ->
    exp ->
    exp ->
    string ->
    unit

  val debugNoFreeVars : ctx ->
    value ->
    string ->
    unit
end
