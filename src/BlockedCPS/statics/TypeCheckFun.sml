
functor TypeCheckFun(
  structure Abt : ABT

  structure Context : CONTEXT
  (* nooo SML/NJ doesn't allow a type sharing spec here for whatever magical reason *)
  where type var = Abt.var
  where type kind = Abt.kind
  where type con = Abt.con
  where type value = Abt.value
  where type exp = Abt.exp
  where type block = Abt.block
  where type program = Abt.program

  structure Subst : SUBST
  where type var = Abt.var
  where type kind = Abt.kind
  where type con = Abt.con
  where type value = Abt.value
  where type exp = Abt.exp
  where type block = Abt.block
  where type program = Abt.program

  structure Equiv : EQUIV
  where type context = Context.t
  where type var = Abt.var
  where type kind = Abt.kind
  where type con = Abt.con
  where type value = Abt.value
  where type exp = Abt.exp
  where type block = Abt.block
  where type program = Abt.program
) : TYPECHECK = struct
  open Abt
  open Context
  open Subst
  open Equiv

  (* Bidirectional type checking *)
  (* I can't find a really good paper to link here because most of
   * them are working with different type systems, but
   * Frank Pfenning's notes on verifications is probably good enough.
   * The only difference here is that when I need to verify some
   * subexpression is of a specific type, I need to weak head normalize it,
   * otherwise we might get redexes.
   *)
  (* ctx |> v --> c *)
  fun typeValueSynth ctx value =
    case value of
      Value_var v => lookupType ctx v
    | Value_pick (v, i) =>
        (case weakHeadNormalize ctx (typeValueSynth ctx v) of
           Type_productfix cons => List.nth (cons, i)
        | _ => raise TypeError)
    | Value_pack (c, v, c') =>
        (case weakHeadNormalize ctx c' of
           Type_exists (k, crest) =>
             (kindCheck ctx c k;
             typeValueCheck ctx v (substInCon 0 [c] 0 crest);
             c')
         | _ => raise TypeError)
    | Value_polyapp (v, c) =>
        (case weakHeadNormalize ctx (typeValueSynth ctx v) of
           Type_forall (k, crest) =>
             (kindCheck ctx c k;
             substInCon 0 [c] 0 crest)
         | _ => raise TypeError)
    | Value_tuple vals =>
        Type_product (ParList.map (typeValueSynth ctx) vals)
    | Value_inj (c, i, v) =>
        (case weakHeadNormalize ctx c of
           Type_sum tys => (
             conEquiv ctx
             (List.nth (tys, i)) (typeValueSynth ctx v)
             Kind_type;
             c
           )
        | _ => raise TypeError)
    | Value_fold (c, v) => let
        val ty = Type_rec c
      in (typeValueCheck ctx v (substInCon 0 [ty] 0 c); ty) end

  (* ctx |> v <-- c *)
  and typeValueCheck ctx value con =
    (conEquiv ctx con (typeValueSynth ctx value) Kind_type) : unit

  (* ctx |> e : 0 *)
  and typeExpCheck ctx exp =
    case exp of
      Exp_app (v, v') =>
        (case weakHeadNormalize ctx (typeValueSynth ctx v) of
           Type_not cs =>
             (ParList.map (fn (v, c) => typeValueCheck ctx v c)
             (ListPair.zip (v', cs)); ())
         | _ => raise TypeError)
    | Exp_unpack (v, x, e) =>
        (case weakHeadNormalize ctx (typeValueSynth ctx v) of
           Type_exists (k, c) =>
             typeExpCheck (extendType (extendKind ctx k) x c) e
         | _ => raise TypeError)
    | Exp_proj (v, i, x, exp) =>
        (case weakHeadNormalize ctx (typeValueSynth ctx v) of
           Type_product tys =>
             typeExpCheck (extendType ctx x (List.nth (tys, i))) exp
         | _ => raise TypeError)
    | Exp_case (v, cases) =>
        (case weakHeadNormalize ctx (typeValueSynth ctx v) of
           Type_sum tys =>
             ListPair.appEq
             (fn (ty, (x, e)) => typeExpCheck (extendType ctx x ty) e)
             (tys, cases)
        | _ => raise TypeError)
    | Exp_unfold (v, x, e) =>
        (case weakHeadNormalize ctx (typeValueSynth ctx v) of
           (ty as (Type_rec c)) =>
             typeExpCheck (extendType ctx x (substInCon 0 [ty] 0 c)) e
         | _ => raise TypeError)
    | Exp_let (v, x, e) =>
        typeExpCheck (extendType ctx x (typeValueSynth ctx v)) e
    | Exp_exit _ => ()

  (* ctx |> b --> c *)
  and typeBlockSynth ctx block =
    case block of
      Block_fixlam lams => let
        val (ctx, tys) = List.foldr
          (fn ((f, bnds, _), (ctx, tys)) => let
            val ty = Type_not (ParList.map (#2) bnds)
            val () = kindCheck ctx ty Kind_type
          in (extendType ctx f ty, ty :: tys) end)
          (ctx, nil)
          lams
        val _ = ParList.map
          (fn (_, bnds, e) => typeExpCheck
            (ParList.foldr (fn ((x, c), ctx) => extendType ctx x c) ctx bnds)
            e)
          lams
      in Type_productfix tys end
    | Block_lam (bnds, e) => let
        val (ctx, tys) = List.foldr
          (fn ((x, c), (ctx, tys)) =>
            (kindCheck ctx c Kind_type;
              (extendType ctx x c, c :: tys)))
          (ctx, nil)
          bnds
        val () = typeExpCheck ctx e
      in Type_not tys end
    | Block_polylam (k, b) =>
        typeBlockSynth (extendKind ctx k) b

  (* ctx |> b <-- c *)
  and typeBlockCheck ctx block con =
    (conEquiv ctx con (typeBlockSynth ctx block) Kind_type) : unit

  (* ctx |> p : 0 *)
  and typeProgramCheck ctx program =
    case program of
      Program_dyn e => typeExpCheck ctx e
    | Program_bnd (b, x, p) =>
        typeProgramCheck (extendType ctx x (typeBlockSynth ctx b)) p
end
