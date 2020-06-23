
functor OptimizeFun (
  structure Abt : ABT
  structure FreeVars : FREEVARS
  where type var = Abt.var
  where type kind = Abt.kind
  where type con = Abt.con
  where type value = Abt.value
  where type exp = Abt.exp

  structure Subst : SUBST
  where type var = Abt.var
  where type kind = Abt.kind
  where type con = Abt.con
  where type value = Abt.value
  where type exp = Abt.exp
) : OPTIMIZE = struct
  open Abt
  open FreeVars
  open Subst

  exception TypeError

  fun varRedexRemovalExp exp = let
    val opt = varRedexRemovalExp
    fun bump (e, i) = (e, i + 1)

    val exp = case exp of
      Exp_app (
        Value_lam (bnds, e),
        vs
      ) => bump (opt (substInExp 0 nil 0
        (varSubst
          (ParList.map (fn ((x, _), v) => (v, x))
          (ListPair.zip (bnds, vs)))) e))

    | Exp_app (
        Value_pick (
          Value_fixlam lams,
          s
        ),
        vs
      ) => let
        val (lams, is) = ListPair.unzip (ParList.map (fn (s, f, bnds, e) => let
          val (e, i) = varRedexRemovalExp e
        in ((s, f, bnds, e), i) end) lams)
        val i = ParList.foldr (op +) 0 is

        val (s, f, bnds, e) =
          case List.find (fn (s', _, _, _) => Symbols.eq (s, s')) lams of
            SOME lam => lam
          | NONE => raise TypeError
        (* note: we don't want to over optimize this, since this could
        * potentially never terminate. Instead we only do it if there are no
        * mutually recursive calls tracing back to the current function,
        * which means this optimization is guaranteed to eventually terminate. *)

        (* TODO: actually implement this on my mind. For time being check
        * for 0 occurances instead. *)
        val freeE = freeVarsExp e
        val intersect =
          List.exists (fn (_, f, _, _) => VarSet.member freeE f) lams
      in if intersect
        then (Exp_app (Value_pick (Value_fixlam lams, s), vs), i)
        else let
          val e = substInExp 0 nil 0
            (varSubst (ParList.map (fn ((x, _), v) => (v, x))
              (ListPair.zip (bnds, vs))))
            e
        in (e, i + 1) end
      end


    | Exp_app (v, vs) => (exp, 0)

    | Exp_unpack (
        Value_pack (c, v, c'),
        x,
        e
      ) =>
        bump (opt (Exp_let (v, x, substInExp 0 [c] 0 (varSubst nil) e)))

    | Exp_unpack (z, x, e) =>
        let val (e, i) = opt e in (Exp_unpack (z, x, e), i) end

    | Exp_proj (
        Value_tuple vs,
        i, x, e) =>
        bump (opt (Exp_let (List.nth (vs, i), x, e)))

    | Exp_proj (z, i, x, e) =>
        let val (e, i') = opt e in (Exp_proj (z, i, x, e), i') end

    | Exp_case (
        Value_inj (_, i, v),
        cases) => let
          val (x, e) = List.nth (cases, i)
        in bump (opt (Exp_let (v, x, e))) end

    | Exp_case (z, cases) =>
        let val (cases, is) = ListPair.unzip (ParList.map (fn (y, e) => let
          val (e, i) = opt e
        in ((y, e), i) end) cases)
      in (Exp_case (z, cases), List.foldr (op +) 0 is) end

    | Exp_unfold (
        Value_fold (c, v),
        x, e) =>
        bump (opt (Exp_let (v, x, e)))

    | Exp_unfold (z, x, e) =>
        let val (e, i) = opt e in (Exp_unfold (z, x, e), i) end

    | Exp_let (v, x, e) =>
        (case freeVarsCountExp e x of
           0 => bump (opt e)
         | 1 => bump (opt (substInExp 0 nil 0 (varSubst [(v, x)]) e))
         | _ =>
            case v of
              (* note: we always inline projections, because odds are we always want to
              * project it, and it will always get projected multiple times *)
              Value_tuple _ =>
                bump (opt (substInExp 0 nil 0 (varSubst [(v, x)]) e))
            | _ =>
                (* nope, might increase size of program *)
                let val (e, i) = opt e in (Exp_let (v, x, e), i) end)

    | Exp_exit _ => (exp, 0)

  in exp end
end
