
functor FreeVarsFun(
  structure Abt : ABT
) : FREEVARS = struct
  open Abt

  structure VarSet = SplaySet(structure Elem = Variable)
  type varset = VarSet.set

  local open VarSet in

  fun ++ (a, b) = union a b
  infix 4 ++
  fun // (a, b) = remove a b
  infix 3 //

  fun freeVarsValue v =
    case v of
      Value_var v => singleton v
    | Value_fixlam lams => let
        val (free, fs) = ParList.foldr
          (fn ((_, f, bnds, e), (free, fs)) =>
            (free ++ (difference
              (freeVarsExp e)
              (ParList.foldr (fn ((x, _), bnds) => insert bnds x) empty bnds)),
            insert fs f))
          (empty, empty)
          lams
      in difference free fs end
    | Value_pick (v, _) => freeVarsValue v
    | Value_lam (bnds, e) => difference (freeVarsExp e)
        (ParList.foldr (fn ((x, _), bnds) => insert bnds x) empty bnds)
    | Value_pack (_, v, _) => freeVarsValue v
    | Value_tuple vs => ParList.foldr
        (fn (v, free) => free ++ (freeVarsValue v))
        empty
        vs
    | Value_inj (_, _, v) => freeVarsValue v
    | Value_fold (_, v) => freeVarsValue v
  and freeVarsExp e =
    case e of
      Exp_app (v, vs') => ParList.foldr
        (fn (v, free) => (freeVarsValue v) ++ free)
        (freeVarsValue v)
        vs'
    | Exp_unpack (v, x, e) =>
        (freeVarsValue v) ++ ((freeVarsExp e) // x)
    | Exp_proj (v, _, x, e) =>
        (freeVarsValue v) ++ ((freeVarsExp e) // x)
    | Exp_case (v, cases) =>
        (ParList.foldr
          (op ++)
          (freeVarsValue v)
          (ParList.map (fn (x, e) => (freeVarsExp e) // x) cases)
        )
    | Exp_unfold (v, x, e) =>
        (freeVarsValue v) ++ ((freeVarsExp e) // x)
    | Exp_let (v, x, e) =>
        (freeVarsValue v) ++ ((freeVarsExp e) // x)
    | Exp_exit _ => empty
  end
end
