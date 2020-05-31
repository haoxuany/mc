
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
           (* by inversion, v should be a lambda *)
           Value_lam (var, con, exp) =>
             substInExp 0 nil 0 (varSubst [(v', var)]) exp
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
  in exp end

  fun run exp = (run (step exp))
end
