
structure DebugTranslation : DEBUGTRANSLATION = struct
  structure SLang = Cps
  structure S = SLang.Abt
  structure SContext = SLang.Context

  type ctx = SContext.t

  open SContext

  val weakHeadNormalize = SLang.Equiv.weakHeadNormalize
end
