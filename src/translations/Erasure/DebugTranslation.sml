
structure DebugTranslation : DEBUGTRANSLATION = struct
  structure SLang = BlockedCps
  structure S = SLang.Abt
  structure SContext = SLang.Context

  type ctx = SContext.t

  open SContext

  val emptyCtx = SContext.new

  fun extendTypes ctx l =
    List.foldr (fn ((x, c), ctx) => extendType ctx x c) ctx l

  val typeValueSynth = SLang.TypeCheck.typeValueSynth

  val typeBlockSynth = SLang.TypeCheck.typeBlockSynth

  val weakHeadNormalize = SLang.Equiv.weakHeadNormalize
end
