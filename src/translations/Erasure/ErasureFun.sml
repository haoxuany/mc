
functor ErasureFun(
  structure DebugTranslation : DEBUGTRANSLATION
) = struct
  structure SLang = BlockedCps
  structure TLang = Low
  structure S = SLang.Abt
  structure T = TLang.Abt

  structure DebugTranslation = DebugTranslation
  open DebugTranslation

  val new = Variable.new
  exception TypeError

  open SLang.Subst

  fun translateValue (ctx : ctx) value = let
    val result = case value of
      S.Value_var x => T.Value_var x

    | S.Value_pick (v, i) => T.Value_pick (translateValue ctx v, i)

    | S.Value_pack (_, v, _) => translateValue ctx v

    | S.Value_polyapp (v, _) => translateValue ctx v

    | S.Value_tuple vs =>
        T.Value_tuple (ParList.map (translateValue ctx) vs)

    | S.Value_inj (c, i, v) =>
        (case weakHeadNormalize ctx c of
           S.Type_sum cs => T.Value_inj (List.length cs, i, translateValue ctx v)
         | _ => raise TypeError)

    | S.Value_fold (c, v) => translateValue ctx v

  in result end

  and translateExp (ctx : ctx) exp = let
    val result = case exp of
      S.Exp_app (v, vs) =>
        T.Exp_app (translateValue ctx v, ParList.map (translateValue ctx) vs)

    | S.Exp_unpack (v, x, e) => let
        val ty = typeValueSynth ctx v
        val (k, c) = case weakHeadNormalize ctx ty of
          S.Type_exists t => t
        | _ => raise TypeError
      in T.Exp_let (translateValue ctx v, x,
        translateExp (extendType (extendKind ctx k) x c) e) end

    | S.Exp_proj (v, i, x, e) => let
        val ty = typeValueSynth ctx v
        val ty = case weakHeadNormalize ctx ty of
          S.Type_product tys => List.nth (tys, i)
        | _ => raise TypeError
      in T.Exp_proj (translateValue ctx v, i, x,
        translateExp (extendType ctx x ty) e) end

    | S.Exp_case (v, cases) => let
        val ty = typeValueSynth ctx v
        val tys = case weakHeadNormalize ctx ty of
          S.Type_sum tys => tys
        | _ => raise TypeError
        val cases = ParList.map
          (fn ((x, e), ty) => (x, translateExp (extendType ctx x ty) e))
          (ListPair.zip (cases, tys))
      in T.Exp_case (translateValue ctx v, cases) end

    | S.Exp_unfold (v, x, e) => let
        val ty = typeValueSynth ctx v
        val ty = case weakHeadNormalize ctx ty of
          (rho as (S.Type_rec t)) => substInCon 0 [rho] 0 t
        | _ => raise TypeError
      in T.Exp_let (translateValue ctx v, x,
        translateExp (extendType ctx x ty) e) end

    | S.Exp_let (v, x, e) =>
        T.Exp_let (translateValue ctx v, x,
          translateExp (extendType ctx x (typeValueSynth ctx v)) e)

    | S.Exp_exit i => T.Exp_exit i

  in result end

  and translateBlock (ctx : ctx) block = let
    val result = case block of
      S.Block_fixlam lams => let
        val ctx = List.foldr
          (fn ((_, f, bnds, e), ctx) =>
            extendType ctx f (S.Type_not (ParList.map #2 bnds)))
          ctx
          lams
        val lams = ParList.map
          (fn (s, f, bnds, e) =>
            (s, f, ParList.map #1 bnds, translateExp (extendTypes ctx bnds) e))
          lams
      in T.Block_fixlam lams end

   | S.Block_lam (bnds, e) =>
       T.Block_lam (ParList.map #1 bnds, translateExp (extendTypes ctx bnds) e)

   | S.Block_polylam (k, b) =>
       translateBlock (extendKind ctx k) b

  in result end

  and translateProgram program =
    case program of
      S.Program (bnds, e) => let
        fun trans ctx bnds newbnds =
          case bnds of
            nil => T.Program (List.rev newbnds, translateExp ctx e)
          | (x, b) :: rest =>
              trans (extendType ctx x (typeBlockSynth ctx b)) rest
              ((x, translateBlock ctx b) :: newbnds)
      in trans (emptyCtx ()) bnds nil end

end
