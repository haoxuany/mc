
functor HoistingFun(
  structure DebugTranslation : DEBUGTRANSLATION
) = struct
  open HoistingConstructorTranslation
  structure DebugTranslation = DebugTranslation
  open DebugTranslation

  (* Note: this only works assuming that variable *)
  structure BindingSet = SplaySet(
    structure Elem = struct
      type t = int (* order *) * T.var * T.block
      fun eq ((_, a, _), (_, b, _)) = Variable.eq (a, b)
      fun compare ((ia, a, _), (ib, b, _)) =
        case (Int.compare (ia, ib), Variable.compare (a, b)) of
          (GREATER, _) => LESS
        | (LESS, _) => GREATER
        | (EQUAL, ord) => ord
    end
  )
  open BindingSet

  val new = Variable.new
  exception TypeError

  open SLang.Subst

  (* we need an order element to make sure that subtree
  * get an earlier block than the current tree. This is
  * because hoisting can introduce variables that refers to previous
  * blocks, which is not closure converted. say
  *
  * fn x => (fn y => fn y) is a closure converted program, however,
  * let z = fn y => y in fn x => z end is not. Hence,
  * the subtree (fn y => fn y) needs to be hoisted at a *higher* order
  * than the current expression. *)
  fun translateValue (ctx : ctx) value order = let
    val translateValue = fn ctx => fn value =>
      translateValue ctx value (order + 1)
    val translateExp = fn ctx => fn exp =>
      translateExp ctx exp (order + 1)

    val result = case value of
      S.Value_var v => (empty, T.Value_var v)

    | S.Value_fixlam lams => let
        val ctx = List.foldr
          (fn ((_, f, bnds, _), ctx) =>
            extendType ctx f (S.Type_not (ParList.map #2 bnds)))
          ctx lams

        val (bs, lams) = ListPair.unzip ( ParList.map
          (fn (s, f, bnds, e) => let
            val (b, e) = translateExp (extendTypes ctx bnds) e
          in (b,
            (s, f, ParList.map (fn (x, c) => (x, translateCon c)) bnds, e))
          end)
          lams
        )

        (* for ctx k1, k2, ..., kn |- v : tau, construct
        * |- Lam k1. Lam k2. ... .v' *)
        fun constructKindB kinds =
          case kinds of
            nil => T.Block_fixlam lams
          | k :: rest => T.Block_polylam (
              translateKind k,
              constructKindB rest
            )

        val kinds = kinds ctx
        val fixblock = constructKindB kinds

        val f = new ()

        val b = ParList.foldr
          (fn (a, b) => union a b)
          (singleton (order, f, fixblock))
          bs

        (* for ctx k0, k1, ... , kn |- v : tau, construct
        * |- f <c :: k0> ... <c :: kn>
        * hence, k0 <=> debrujin indicies position n
        * k1 <=> n - 1
        * ...
        * kn <=> 0
        * the constructor variable here is thus var (n - i)
        *)
        val n = List.length kinds
        fun constructApp i =
          if i = 0 then T.Value_var f
          else T.Value_polyapp (constructApp (i - 1), T.Con_var (n - i))

      in (b, constructApp n) end

    | S.Value_pick (v, i) => let
        val (b, v) = translateValue ctx v
      in (b, T.Value_pick (v, i)) end

    | S.Value_lam (bnds, e) => let
        val (b, e) = translateExp (extendTypes ctx bnds) e

        (* for ctx k1, k2, ..., kn |- v : tau, construct
        * |- Lam k1. Lam k2. ... .v' *)
        fun constructKindB kinds =
          case kinds of
            nil => T.Block_lam
              (ParList.map (fn (x, c) => (x, translateCon c)) bnds, e)
          | k :: rest => T.Block_polylam (
              translateKind k,
              constructKindB rest
            )

        val kinds = kinds ctx
        val lamblock = constructKindB kinds

        val f = new ()

        val b = insert b (order, f, lamblock)

        (* for ctx k0, k1, ... , kn |- v : tau, construct
        * |- f <c :: k0> ... <c :: kn>
        * hence, k0 <=> debrujin indicies position n
        * k1 <=> n - 1
        * ...
        * kn <=> 0
        * the constructor variable here is thus var (n - i)
        *)
        val n = List.length kinds
        fun constructApp i =
          if i = 0 then T.Value_var f
          else T.Value_polyapp (constructApp (i - 1), T.Con_var (n - i))

      in (b, constructApp n) end

    | S.Value_pack (c, v, c') => let
        val (b, v) = translateValue ctx v
      in (b, T.Value_pack (translateCon c, v, translateCon c')) end

    | S.Value_tuple vs => let
        val (b, vs) = ListPair.unzip (ParList.map (translateValue ctx) vs)
        val b = List.foldr (fn (a, b) => union a b) empty b
      in (b, T.Value_tuple vs) end

    | S.Value_inj (c, i, v) => let
        val (b, v) = translateValue ctx v
      in (b, T.Value_inj (translateCon c, i, v)) end

    | S.Value_fold (c, v) => let
        val (b, v) = translateValue ctx v
      in (b, T.Value_fold (translateCon c, v)) end

  in result end

  and translateExp (ctx : ctx) exp order = let
    val translateValue = fn ctx => fn value =>
      translateValue ctx value (order + 1)
    val translateExp = fn ctx => fn exp =>
      translateExp ctx exp (order + 1)

    val result = case exp of
      S.Exp_app (v, v') => let
        val (b, v) = translateValue ctx v
        val (b', v') = ListPair.unzip (List.map (translateValue ctx) v')
      in (List.foldr (fn (a, b) => union a b) b b', T.Exp_app (v, v')) end

    | S.Exp_unpack (v, x, e) => let
        val (k, c) = case weakHeadNormalize ctx (typeValueSynth ctx v) of
          S.Type_exists cexists => cexists
        | _ => raise TypeError

        val (b, v) = translateValue ctx v
        val (b', e) = translateExp
          (extendType (extendKind ctx k) x c) e
      in (union b b', T.Exp_unpack (v, x, e)) end

    | S.Exp_proj (v, i, x, e) => let
        val t = case weakHeadNormalize ctx (typeValueSynth ctx v) of
          S.Type_product tys => List.nth (tys, i)
        | _ => raise TypeError

        val (b, v) = translateValue ctx v
        val (b', e) = translateExp (extendType ctx x t) e
      in (union b b', T.Exp_proj (v, i, x, e)) end

    | S.Exp_case (v, cases) => let
        val tys = case weakHeadNormalize ctx (typeValueSynth ctx v) of
          S.Type_sum tys => tys
        | _ => raise TypeError

        val (b, v) = translateValue ctx v
        val cases = ParList.map
          (fn ((x, e), t) => let
            val (b, e) = translateExp (extendType ctx x t) e
          in (b, (x, e)) end)
          (ListPair.zip (cases, tys))

        val b = List.foldr
          (fn ((a, _), b) => union a b)
          b cases
      in (b, T.Exp_case (v, ParList.map #2 cases)) end

    | S.Exp_unfold (v, x, e) => let
        val t = case weakHeadNormalize ctx (typeValueSynth ctx v) of
          rho as (S.Type_rec t) => substInCon 0 [rho] 0 t
        | _ => raise TypeError

        val (b, v) = translateValue ctx v
        val (b', e) = translateExp (extendType ctx x t) e
      in (union b b', T.Exp_unfold (v, x, e)) end

    | S.Exp_let (v, x, e) => let
        val (b', e) = translateExp (extendType ctx x (typeValueSynth ctx v)) e
        val (b, v) = translateValue ctx v
      in (union b b', T.Exp_let (v, x, e)) end

    | S.Exp_exit i => (empty, T.Exp_exit i)

  in result end

  fun translateProgram e = let
    val (b, e) = translateExp (emptyCtx ()) e 0
  in T.Program (
    ParList.map (fn (_, x, b) => (x, b)) (BindingSet.toList b),
    e) end
end
