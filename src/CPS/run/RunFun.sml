
functor RunFun(
  structure Abt : ABT
  structure Subst : SUBST
  where type var = Abt.var
  where type kind = Abt.kind
  where type con = Abt.con
  where type value = Abt.value
  where type exp = Abt.exp
) : RUN = struct
  open Abt
  open Subst

  (* Metatheory violation: doesn't typecheck, hence got stuck *)
  exception Stuck of exp

  fun step (fullexp as exp) = let
    val exp = case exp of
      Exp_app (v, v') =>
        (case v of
           (* by inversion, v should be a lambda or a pick of some fixpoint lambda*)
           Value_lam (bnds, exp) =>
             substInExp 0 nil 0 (varSubst
             (ParList.map
               (fn ((x, _), v) => (v, x)) (ListPair.zip (bnds, v'))))
             exp
         | Value_pick (fix as (Value_fixlam lams), i) => let
             val substs = ParList.map
               (fn (s, f, _, _) => (Value_pick (fix, s), f))
               lams
             val (bnds, e) =
               case List.find (fn lam => Symbols.eq (#1 lam, i)) lams of
                 NONE => raise Stuck exp
               | SOME (_, _, bnds, e) => (bnds, e)
           in substInExp 0 nil 0 (varSubst
             ((ParList.map
               (fn ((x, _), v) => (v, x)) (ListPair.zip (bnds, v')))
             @ substs)
            ) e end
         | _ => raise Stuck fullexp)
    | Exp_unpack (v, x, e) =>
        (case v of
           (* by inversion, v should be a pack *)
           Value_pack (c, v, _) =>
             substInExp 0 [c] 0 (varSubst [(v, x)]) e
         | _ => raise Stuck fullexp)
    | Exp_proj (v, i, x, e) =>
        (case v of
           (* by inversion, v should be a tuple *)
           Value_tuple vals =>
             substInExp 0 nil 0 (varSubst [(List.nth (vals, i), x)]) e
         | _ => raise Stuck fullexp)
    | Exp_case (v, cases) =>
        (case v of
           (* by inversion, v should be a injection *)
           Value_inj (_, i, v) => let
             val (x, e) = List.nth (cases, i)
           in substInExp 0 nil 0 (varSubst [(v, x)]) e end
         | _ => raise Stuck fullexp)
    | Exp_unfold (v, x, e) =>
        (case v of
           (* by inversion, v should be a fold *)
           Value_fold (_, v) =>
             substInExp 0 nil 0 (varSubst [(v, x)]) e
         | _ => raise Stuck fullexp)
    | Exp_let (v, x, e) =>
        substInExp 0 nil 0 (varSubst [(v, x)]) e
    | Exp_exit _ => raise Stuck fullexp
  in exp end

  fun run exp = (run (step exp))
end
