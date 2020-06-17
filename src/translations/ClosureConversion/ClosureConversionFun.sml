
functor ClosureConversionFun(
  structure DebugTranslation : DEBUGTRANSLATION
) = struct
  open ClosureConversionConstructorTranslation
  open FreeVars
  structure DebugTranslation = DebugTranslation
  open DebugTranslation

  val new = Variable.new
  exception TypeError

  fun translateValue (ctx : ctx) (fullvalue as v) = let
    val debug = fn (ans as (result, tau, tau'), name) =>
      (debugValue ctx fullvalue tau result tau' name; ans)
    infix 9 debug

    val result =
      case v of
        Value_var x => let
          val result = fullvalue
          val tau = lookupType ctx x
          val tau' = translateCon tau
        in (result, tau, tau') end
        debug "var"

      | Value_fixlam lams => let
        (* <not t1, not t2, ..., not tn>
        * => exists env :: T. env * <not (env * t1), ..., not (env * tn)> *)
          val (ctx, tys) = List.foldr
            (fn ((f, _, c, _), (ctx, tys)) => let
              val notc = Type_not c
            in (extendType ctx f notc, notc :: tys) end)
            (ctx, nil)
            lams

          val free = VarSet.toList (freeVarsValue fullvalue)
          val freeTys = Type_product (ParList.map
            (fn x => translateCon (lookupType ctx x))
            free)
          val freeI = ListPair.zip (
            List.tabulate (List.length free, fn i => i),
            free
          )

          val (lams, t') = ListPair.unzip (ParList.map
            (fn (f, x, c, e) => let
              val e' = translateExp (extendType ctx x c) e
              val c' = translateCon c

              val y = new () (* : freeTys * xty. new lambda binder *)
              val env = new () (* : freeTys. environment bindings *)
              fun constructBind vars =
                case vars of
                  nil => e'
                | (i, x) :: rest => Exp_proj (
                    Value_var env,
                    i,
                    x,
                    constructBind rest
                  )

              val result = (f, y,
                Type_product [freeTys, c'],
                Exp_proj (
                  Value_var y,
                  1,
                  x,
                  Exp_proj (
                    Value_var y,
                    0,
                    env,
                    constructBind freeI
                  )
                )
              )
            in (
              result,
              Type_not (
                Type_product [
                  Con_var 0,
                  substInCon 0 nil 1 c'
                ]
              )
            ) end)
            lams)

          val tau' = Type_exists (
            Kind_type,
            Type_product [
              Con_var 0,
              Type_productfix t'
            ]
          )
          val result = Value_pack (
            freeTys,
            Value_tuple [
              Value_tuple (ParList.map Value_var free),
              Value_fixlam lams
            ],
            tau'
          )
          val tau = Type_productfix tys
        in (result, tau, tau') end
        debug "fixlam"

      | Value_pick (v, i) => let
        (* assume v : <not t1, ..., not tn> =>
        * => exists env :: T. env * <not (env * t1), ..., not (env * tn)> *)
        (* result is : not ti => exists env0 :: T. env0 * (not (env0 * ti)) *)
        (* Note, by canonical forms of v's productfix type, we know already
        * that v is a closure converted fixpoint package. As such, env0 can
        * be trivially unit and contain no interesting value. It is however
        * not obvious that this form isn't eta-reducible. *)
          val (v', t, u) = translateValue ctx v

          val tinot = case weakHeadNormalize ctx t of
            Type_productfix tys => List.nth (tys, i)
          | _ => raise TypeError
          val ti = case tinot of Type_not ti => ti | _ => raise TypeError

          val tau = tinot
          val tau' = translateCon tinot
          val unitty = Type_product nil

          val y = new () (* : env0 * ti *)
          val v = new () (* : ti *)
          val z = new () (* : env * <not (env * t1), ...> *)
          val lams = new () (* : <not (env * t1), ...> *)
          val env = new () (* : env *)
          val result = Value_pack (
            unitty,
            Value_tuple [
              Value_tuple nil,
              Value_lam (
                y, (* : env0 * ti *)
                Type_product [unitty, ti],
                Exp_proj (
                  Value_var y,
                  1,
                  v, (* : ti *)
                  Exp_unpack (
                    v',
                    z, (* : env * <not (env * t1), ...> *)
                    substInExp 0 nil 1 (varSubst nil) (Exp_proj (
                      Value_var z,
                      0,
                      env,
                      Exp_proj (
                        Value_var z,
                        1,
                        lams, (* : <not (env * t1), ...> *)
                        Exp_app (
                          Value_pick (Value_var lams, i),
                          Value_tuple [Value_var env, Value_var v]
                        )
                      )
                    ))
                  )
                )
              )
            ],
            tau'
          )

        in (result, tau, tau') end
        debug "pick"

      | Value_lam (x, c, e) => let
          (* not t => exists env :: T. env * (not (env * u)) *)
          val e' = translateExp (extendType ctx x c) e
          val c' = translateCon c

          val free = VarSet.toList (freeVarsValue fullvalue)
          val freeTys = Type_product (ParList.map
            (fn x => translateCon (lookupType ctx x))
            free)
          val freeI = ListPair.zip (
            List.tabulate (List.length free, fn i => i),
            free
          )

          val env = new () (* : env *)
          val y = new () (* : env * u *)
          val yty = Type_product [freeTys, c']

          fun constructBind vars =
            case vars of
              nil => e'
            | (i, x) :: rest => Exp_proj (
                Value_var env,
                i,
                x,
                constructBind rest
              )

          val tau = Type_not c
          val tau' = Type_exists (
            Kind_type,
            Type_product [
              Con_var 0,
              Type_not (
                Type_product [
                  Con_var 0,
                  substInCon 0 nil 1 c'
                ]
              )
            ]
          )
          val result = Value_pack (
            freeTys,
            Value_tuple [
              Value_tuple (ParList.map Value_var free),
              Value_lam (
                y, yty, (* : env * u *)
                Exp_proj (
                  Value_var y,
                  1,
                  x,
                  Exp_proj (
                    Value_var y,
                    0,
                    env,
                    constructBind freeI
                  )
                )
              )
            ],
            tau'
          )
        in (result, tau, tau') end
        debug "lam"

      | Value_pack (t, v, t') => let
          val (v', _, _) = translateValue ctx v
          val u = translateCon t
          val u' = translateCon t'

          val result = Value_pack (u, v', u')
          val tau = t'
          val tau' = u'
        in (result, tau, tau') end
        debug "pack"

      | Value_tuple vs => let
          val vs' = ParList.map (translateValue ctx) vs
          val (result, tau, tau') = List.foldr
            (fn ((v, t, u), (result, tau, tau')) =>
              (v :: result, t :: tau, u :: tau'))
            (nil, nil, nil)
            vs'

          val result = Value_tuple result
          val tau = Type_product tau
          val tau' = Type_product tau'
        in (result, tau, tau') end
        debug "tuple"

      | Value_inj (c, i, v) => let
          val (v', _, _) = translateValue ctx v

          val tau = c
          val tau' = translateCon c
          val result = Value_inj (tau', i, v')
        in (result, tau, tau') end
        debug "inj"

      | Value_fold (c, v) => let
          val (v', _, _) = translateValue ctx v

          val c' = translateCon c
          val result = Value_fold (c', v')
          val tau = Type_rec c
          val tau' = Type_rec c'
        in (result, tau, tau') end
        debug "fold"

  in result end

  and translateExp (ctx : ctx) (fullexp as e) = let
    val debug = fn (result, name) =>
      (debugExp ctx fullexp result name; result)
    infix 9 debug

    val result = case e of
      Exp_app (v, v') => let
        val (v, _, _) = translateValue ctx v
        (* where not t2 => exists env :: T. env * (not (env * u2)) *)
        val (v', t2, u2) = translateValue ctx v

        val x = new () (* : env * (not (env * u2)) *)
        val env = new () (* : env *)
        val f = new () (* : not (env * u2) *)
        val result = Exp_unpack (
          v,
          x, (* : env * (not (env * u2)) *)
          substInExp 0 nil 1 (varSubst nil) (
            Exp_proj (
              Value_var x,
              1,
              f, (* : not (env * u2) *)
              Exp_proj (
                Value_var x,
                0,
                env, (* : env *)
                Exp_app (
                  Value_var f,
                  Value_tuple [Value_var env, v']
                )
              )
            )
          )
        )
      in result end
      debug "app"

    | Exp_unpack (v, x, e) => let
        val (v', t, _) = translateValue ctx v

        val (k, c) = case weakHeadNormalize ctx t of
          Type_exists (k, c) => (k, c)
        | _ => raise TypeError

        val result = Exp_unpack (v', x,
          translateExp (extendType (extendKind ctx k) x c) e)
      in result end
      debug "unpack"

   | Exp_proj (v, i, x, e) => let
       val (v', t, _) = translateValue ctx v

       val t = case weakHeadNormalize ctx t of
         Type_product cons => List.nth (cons, i)
       | _ => raise TypeError

       val result = Exp_proj (v', i, x,
         translateExp (extendType ctx x t) e)
     in result end
     debug "proj"

   | Exp_case (v, cases) => let
       val (v', t, _) = translateValue ctx v

       val t = case weakHeadNormalize ctx t of
         Type_sum t => t
       | _ => raise TypeError

       val cases' = ParList.map
         (fn ((x, e), t) => (x, translateExp (extendType ctx x t) e))
         (ListPair.zip (cases, t))
       val result = Exp_case (v', cases')
     in result end
     debug "case"

   | Exp_unfold (v, x, e) => let
       val (v', t, _) = translateValue ctx v

       val t = case weakHeadNormalize ctx t of
         Type_rec c => substInCon 0 [t] 0 c
       | _ => raise TypeError

       val result = Exp_unfold (v', x,
         translateExp (extendType ctx x t) e)
     in result end
     debug "unfold"

   | Exp_let (v, x, e) => let
       val (v', t, _) = translateValue ctx v

       val result = Exp_let (v', x,
         translateExp (extendType ctx x t) e)
     in result end
     debug "let"

   | Exp_exit _ => fullexp debug "exit"

  in result end
end
