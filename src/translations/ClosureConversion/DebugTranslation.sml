
structure DebugTranslation : DEBUGTRANSLATION = struct
  structure SLang = Cps
  structure TLang = Cps
  structure S = SLang.Abt
  structure T = TLang.Abt
  structure SContext = SLang.Context
  structure TContext = TLang.Context

  open ClosureConversionConstructorTranslation

  open Cps

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

  fun check f name thm =
    ((f ()) handle e =>
      (TextIO.print (String.concat [
        "For Closure Conversion, step '", name,
        "' failed for thm (", thm,
        ") with failure :", General.exnName e,
        ": ", General.exnMessage e, "\n"
      ]); raise e))

  fun debugValue (ctx : ctx)
    (svalue : value)
    (sty : con)
    (tvalue : value)
    (tty : con)
    (name : string) = let
    val check = fn thm => fn f => check f name thm

    val () = check
      "source value typechecks"
      (fn () => SLang.TypeCheck.typeValueCheck (#1 ctx) svalue sty)
    val () = check
      "type translation correct"
      (fn () => TLang.Equiv.conEquiv (#2 ctx) (translateCon sty) tty T.Kind_type)
    val () = check
      "target value typechecks"
      (fn () => TLang.TypeCheck.typeValueCheck (#2 ctx) tvalue tty)
  in () end

  fun debugExp (ctx : ctx)
    (sexp : exp)
    (texp : exp)
    (name : string) = let
    val check = fn thm => fn f => check f name thm

    val () = check
      "source exp typechecks"
      (fn () => SLang.TypeCheck.typeExpCheck (#1 ctx) sexp)
    val () = check
      "target exp typechecks"
      (fn () => TLang.TypeCheck.typeExpCheck (#2 ctx) texp)
  in () end

  fun debugNoFreeVars (ctx : ctx)
    (tval : value)
    (name : string) = let
    val check = fn thm => fn f => check f name thm

    val () = check
      "translation has no free variables"
      (fn () => let
        val free = TLang.FreeVars.freeVarsValue tval
        val free = TLang.FreeVars.VarSet.toList free
        val len = List.length free
      in if len = 0 then
          (TLang.Equiv.kindCheck (#2 ctx)
            (TLang.TypeCheck.typeValueSynth
            (TLang.Context.withoutVars (#2 ctx)) tval)
            T.Kind_type)
         else raise Fail
           (String.concat (("free vars of: ") ::
             (ParList.map T.Variable.print free)))
      end)
  in () end
end
