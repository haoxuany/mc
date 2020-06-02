
structure DebugTranslation : DEBUGTRANSLATION = struct
  structure SLang = FOmegaS
  structure TLang = Cps
  structure S = SLang.Abt
  structure T = TLang.Abt
  structure SContext = SLang.Context
  structure TContext = TLang.Context

  open CpsConstructorTranslation

  open S

  type ctx = SContext.t * TContext.t

  fun emptyCtx () =
    (SContext.new (), TContext.new ())

  fun lookupType (ctx : ctx) =
    SContext.lookupType (#1 ctx)

  fun extendType (ctx : ctx) x t = let
    val (sctx, tctx) = ctx
  in
    (SContext.extendType sctx x t,
    TContext.extendType tctx x (translateCon t))
  end

  fun extendKind (ctx : ctx) k = let
    val (sctx, tctx) = ctx
  in
    (SContext.extendKind sctx k,
    TContext.extendKind tctx (translateKind k))
  end

  fun weakHeadNormalize (ctx : ctx) = SLang.Equiv.weakHeadNormalize (#1 ctx)

  fun debug (ctx : ctx)
    (sexp : S.term)
    (sty : S.con)
    (k : Variable.t) (kexn : Variable.t)
    (texp : T.exp)
    (tty : T.con)
    (name : string) = let
  in () end
end
