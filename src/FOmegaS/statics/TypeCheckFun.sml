
functor TypeCheckFun(
  structure Abt : ABT

  structure Context : CONTEXT
  (* nooo SML/NJ doesn't allow a type sharing spec here for whatever magical reason *)
  where type var = Abt.var
  where type kind = Abt.kind
  where type con = Abt.con
  where type term = Abt.term

  structure Subst : SUBST
  where type var = Abt.var
  where type kind = Abt.kind
  where type con = Abt.con
  where type term = Abt.term

  structure Equiv : EQUIV
  where type context = Context.t
  where type var = Abt.var
  where type kind = Abt.kind
  where type con = Abt.con
  where type term = Abt.term
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
  (* ctx |> t --> c *)
  fun typeSynth ctx term =
    case term of
      Term_var v => lookupType ctx v
    | Term_let (e, v, e') =>
        typeSynth (extendType ctx v (typeSynth ctx e)) e'
    | Term_fix (v, t, e) =>
        (typeCheck (extendType ctx v t) e t; t)
    | Term_lam (v, t, e) =>
        (kindCheck ctx t Kind_type;
        Type_arrow (t, typeSynth (extendType ctx v t) e))
    | Term_app (e, e') =>
        (case weakHeadNormalize ctx (typeSynth ctx e) of
          Type_arrow (t, t') => (typeCheck ctx e' t; t')
        | _ => raise TypeError)
    | Term_polylam (k, e) =>
        (kindValid ctx k;
        Type_forall (k, typeSynth (extendKind ctx k) e))
    | Term_polyapp (e, c) =>
        (case weakHeadNormalize ctx (typeSynth ctx e) of
           Type_forall (k, c') =>
             (kindCheck ctx c k;
             substConInCon 0 [c] 0 c')
        | _ => raise TypeError)
    | Term_pack (c, e, cexists) =>
        (case weakHeadNormalize ctx cexists of
           Type_exists (k, c') =>
             (kindCheck ctx c k; typeCheck ctx e (substConInCon 0 [c] 0 c');
             cexists)
        | _ => raise TypeError)
    | Term_unpack (e, v, e') =>
        (case weakHeadNormalize ctx (typeSynth ctx e) of
           Type_exists (k, c) => let
             val tresult = typeSynth (extendType (extendKind ctx k) v c) e'
             (* Note, we need to make sure that tresult does not depend on
             * binding of con to k. However at this point tresult came from
             * context of ctx, cv : k, so we need to unshift tresult to fix
             * indices *)
             val tresult = substConInCon 0 [] ~1 tresult
             val () = kindCheck ctx tresult Kind_type
           in tresult end
         | _ => raise TypeError)
    | Term_tuple es => Type_product (ParList.map (typeSynth ctx) es)
    | Term_proj (e, i) =>
        (case weakHeadNormalize ctx (typeSynth ctx e) of
           Type_product tys => List.nth (tys, i)
         | _ => raise TypeError)
    | Term_inj (c, i, e) =>
        (case weakHeadNormalize ctx c of
           Type_sum tys => (typeCheck ctx e (List.nth (tys, i)); c)
         | _ => raise TypeError)
    | Term_case (e, cases) =>
        (case weakHeadNormalize ctx (typeSynth ctx e) of
           Type_sum tys => let
             (* Performance can be improved by parallelizing this. *)
             val tys = ListPair.mapEq
               (fn (ty, (x, e)) => typeSynth (extendType ctx x ty) e)
               (tys, cases)
             val ty = ParList.foldl
               (fn (ty, NONE) => SOME ty
                 | (ty, SOME ty') => (conEquiv ctx ty ty' Kind_type; SOME ty'))
                NONE tys
           in
             case ty of
               (* This is the nullary case. This is interesting because
               * this should technically return any type (abort for nullary
               * sums). However, it's not really possible to construct
               * a value of type void. If we do want the void type,
               * this return value would need to be annotated
               * (through a smart constructor maybe?)
               * We don't do this here to cut down some space.
               *)
               NONE => raise TypeError
             | SOME ty => ty
           end
        | _ => raise TypeError)
  | Term_fold (c, e) => let
      val ty = Type_rec c
    in (typeCheck ctx e (substConInCon 0 [ty] 0 c); ty) end
  | Term_unfold e =>
      (case weakHeadNormalize ctx (typeSynth ctx e) of
         (ty as (Type_rec c)) => substConInCon 0 [ty] 0 c
       | _ => raise TypeError)


  (* ctx |> t <-- c *)
  and typeCheck ctx term con =
    (conEquiv ctx con (typeSynth ctx term) Kind_type) : unit
end
