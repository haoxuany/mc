
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

  fun freeVarsCountValue v x =
    case v of
      Value_var v => if Variable.eq (v, x) then 1 else 0
    | Value_fixlam lams =>
        if List.exists (fn (_, f, _, _) => Variable.eq (f, x)) lams then 0
        else List.foldr (op +) 0
          (ParList.map
            (fn (_, _, bnds, e) =>
              if List.exists (fn (y, _) => Variable.eq (x, y)) bnds then 0
              else freeVarsCountExp e x) lams)
    | Value_pick (v, _) => freeVarsCountValue v x
    | Value_lam (bnds, e) =>
        if List.exists (fn (y, _) => Variable.eq (x, y)) bnds then 0
        else freeVarsCountExp e x
    | Value_pack (_, v, _) => freeVarsCountValue v x
    | Value_tuple vs => List.foldr (op +) 0
        (ParList.map (fn v => freeVarsCountValue v x) vs)
    | Value_inj (_, _, v) => freeVarsCountValue v x
    | Value_fold (_, v) => freeVarsCountValue v x

  and freeVarsCountExp e x =
    case e of
      Exp_app (v, vs) =>
        List.foldr (op +)
        (freeVarsCountValue v x)
        (ParList.map (fn v => freeVarsCountValue v x) vs)

    | Exp_unpack (v, y, e) => let
        val v = freeVarsCountValue v x
      in if Variable.eq (y, x) then v
         else v + (freeVarsCountExp e x)
      end
    | Exp_proj (v, _, y, e) => let
        val v = freeVarsCountValue v x
      in if Variable.eq (y, x) then v
         else v + (freeVarsCountExp e x)
      end
    | Exp_case (v, cases) =>
        List.foldr (op +)
        (freeVarsCountValue v x)
        (ParList.map (fn (y, e) =>
          if Variable.eq (x, y) then 0
          else freeVarsCountExp e x) cases)
    | Exp_unfold (v, y, e) => let
        val v = freeVarsCountValue v x
      in if Variable.eq (y, x) then v
         else v + (freeVarsCountExp e x)
      end
    | Exp_let (v, y, e) => let
        val v = freeVarsCountValue v x
      in if Variable.eq (y, x) then v
         else v + (freeVarsCountExp e x)
      end
    | Exp_exit _ => 0
end
