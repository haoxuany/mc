
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
        * => exists env :: T. env * <not (t1 @ env), ..., not (tn @ env)> *)
          val free = VarSet.toList (freeVarsValue fullvalue)
          val freeTys = Type_product (ParList.map
            (fn x => translateCon (lookupType ctx x))
            free)
          val freeI = ListPair.zip (
            List.tabulate (List.length free, fn i => i),
            free
          )
          val free = Value_tuple (ParList.map Value_var free)

          val lams = ParList.map (fn (f, bnds, e) => let
            val f' = new () (* not (c @ env) *)
            val cnot = Type_not (ParList.map #2 bnds)
          in (f, bnds, e, f', cnot) end) lams

          val tau = Type_productfix (ParList.map #5 lams)

          val ctx = List.foldr
            (fn ((f, _, _, _, cnot), ctx) => extendType ctx f cnot)
            ctx lams

          val lams = ParList.map
            (fn (f, bnds, e, f', _) => let
              val ctx = List.foldr
                (fn ((x, c), ctx) => extendType ctx x c)
                ctx bnds
              val e' = translateExp ctx e

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

              val argbnds = (ParList.map
                  (fn (x, c) => (x, translateCon c)) bnds)
              val bnds = argbnds @ [(env, freeTys)]

              val result = (f, bnds, constructBind freeI)
            in (
              result,
              f',
              Type_not (
                (ParList.map (fn (_, c) => substInCon 0 nil 1 c) argbnds)
                @ [Con_var 0]
              )
            ) end)
            lams

          val rebindFixpoints = fn e => ParList.foldr
            (fn (((f, _, _), f', ty), e) =>
              (* assume f' : (not (ui @ env)) *)
              Exp_let (
                Value_pack (
                  freeTys,
                  Value_tuple [
                    free,
                    Value_var f'
                  ],
                  Type_exists (
                    Kind_type,
                    Type_product
                      [Con_var 0,
                      (* this is already lifted in the previous function *)
                      ty]
                  )
                ),
                f, (* : exists env :: T. env * (not (ui @ env)) *)
                e
              ))
            e
            lams

          val (lams, tys) = ListPair.unzip (ParList.map
            (fn ((_, bnds, e), f', ty) =>
              ((f', bnds, rebindFixpoints e), ty))
            lams)

          val tau' = Type_exists (
            Kind_type,
            Type_product [
              Con_var 0,
              Type_productfix tys
            ]
          )
          val result = Value_pack (
            freeTys,
            Value_tuple [
              free,
              Value_fixlam lams
            ],
            tau'
          )
        in (result, tau, tau') end
        debug "fixlam"

      | Value_pick (v, i) => let
        (* assume v : <not t1, ..., not tn> =>
        * => exists env :: T. env * <not (t1 @ env), ..., not (tn @ env)> *)
        (* result is : not ti => exists env0 :: T. env0 * (not (ti @ env0)) *)
        (* Note, by canonical forms of v's productfix type, we know already
        * that v is a closure converted fixpoint package. As such, env0 can
        * contains no interesting value other than v itself,
        * which is out of this lambda scope. *)
          val (v', t, u) = translateValue ctx v

          val tinot = case weakHeadNormalize ctx t of
            Type_productfix tys => List.nth (tys, i)
          | _ => raise TypeError
          val ti = case tinot of Type_not ti => ti | _ => raise TypeError

          val tau = tinot
          val tau' = translateCon tinot
          val unitty = Type_product nil

          val bnds = ParList.map
            (fn ty => (new (), translateCon ty))
            ti

          val vx = new () (* : u *)
          val z = new () (* : env * <not (ti @ env), ...> *)
          val lams = new () (* : <not (ti @ env), ...> *)
          val env = new () (* : env *)
          val result = Value_pack (
            u,
            Value_tuple [
              v',
              (Value_lam (
                bnds @ [(vx, u)], (* ti1, ..., tin, env0 *)
                Exp_unpack (
                  Value_var vx,
                  z, (* : env * <not (ti @ env), ...> *)
                  substInExp 0 nil 1 (varSubst nil) (Exp_proj (
                    Value_var z,
                    0,
                    env,
                    Exp_proj (
                      Value_var z,
                      1,
                      lams, (* : <not (ti @ env), ...> *)
                      Exp_app (
                        Value_pick (Value_var lams, i),
                        (ParList.map (fn (x, _) => Value_var x) bnds)
                        @ [Value_var env]
                      )
                    )
                  ))
                )
              ))
              debugNoFreeVars "pick"
            ],
            tau'
          )

        in (result, tau, tau') end
        debug "pick"

      | Value_lam (bnds, e) => let
          (* not t => exists env :: T. env * (not (u @ env)) *)
          val free = VarSet.toList (freeVarsValue fullvalue)
          val freeTys = Type_product (ParList.map
            (fn x => translateCon (lookupType ctx x))
            free)
          val freeI = ListPair.zip (
            List.tabulate (List.length free, fn i => i),
            free
          )

          val ctx = List.foldr
            (fn ((x, c), ctx) => extendType ctx x c)
            ctx bnds
          val e' = translateExp ctx e

          val env = new () (* : env *)
          fun constructBind vars =
            case vars of
              nil => e'
            | (i, x) :: rest => Exp_proj (
                Value_var env,
                i,
                x,
                constructBind rest
              )

          val tau = Type_not (ParList.map #2 bnds)
          val bnds = ParList.map (fn (x, c) => (x, translateCon c)) bnds
          val tau' = Type_exists (
            Kind_type,
            Type_product [
              Con_var 0,
              Type_not (
                (ParList.map (fn (_, c) => substInCon 0 nil 1 c) bnds)
                @ [Con_var 0]
              )
            ]
          )

          val result = Value_pack (
            freeTys,
            Value_tuple [
              Value_tuple (ParList.map Value_var free),
              (Value_lam (
                bnds @ [(env, freeTys)],
                constructBind freeI
              ))
              debugNoFreeVars "lam"
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
        (* where not t2 => exists env :: T. env * (not (u2 @ env)) *)
        val v' = ParList.map (fn v => #1 (translateValue ctx v)) v'

        val x = new () (* : env * (not (u2 @ env)) *)
        val env = new () (* : env *)
        val f = new () (* : not (u2 @ env) *)
        val result = Exp_unpack (
          v,
          x, (* : env * (not (u2 @ env)) *)
          substInExp 0 nil 1 (varSubst nil) (
            Exp_proj (
              Value_var x,
              1,
              f, (* : not (u2 @ env) *)
              Exp_proj (
                Value_var x,
                0,
                env, (* : env *)
                Exp_app (
                  Value_var f,
                  v' @ [Value_var env]
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
