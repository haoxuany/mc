
functor TypeCheckFun(
  structure Abt : ABT

  structure Context : CONTEXT
  (* nooo SML/NJ doesn't allow a type sharing spec here for whatever magical reason *)
  where type var = Abt.var
  where type kind = Abt.kind
  where type con = Abt.con
  where type term = Abt.term
  where type sg = Abt.sg
  where type psg = Abt.psg
  where type module = Abt.module
  where type lmodule = Abt.lmodule

  structure Subst : SUBST
  where type var = Abt.var
  where type kind = Abt.kind
  where type con = Abt.con
  where type term = Abt.term
  where type sg = Abt.sg
  where type psg = Abt.psg
  where type module = Abt.module
  where type lmodule = Abt.lmodule

  structure Equiv : EQUIV
  where type context = Context.t
  where type var = Abt.var
  where type kind = Abt.kind
  where type con = Abt.con
  where type term = Abt.term
  where type sg = Abt.sg
  where type psg = Abt.psg
  where type module = Abt.module
  where type lmodule = Abt.lmodule
) : TYPECHECK = struct
  open Abt
  open Context
  open Subst
  open Equiv

  exception TODO

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
    | Term_fixlam lams => let
        val (tys, ctx) = List.foldr
          (fn ((s, f, _, c, _, c'), (tys, ctx)) => let
            val ty = Type_arrow (c, c')
          in ((s, ty) :: tys, extendType ctx f ty) end)
          (nil, ctx)
          lams
        val _ = ParList.map
          (fn (_, _, x, c, e, c') =>
            (kindCheck ctx c Kind_type;
            typeCheck (extendType ctx x c) e c'))
          lams
      in Type_productfix tys end
    | Term_pick (e, i) =>
        (case weakHeadNormalize ctx (typeSynth ctx e) of
           Type_productfix tys =>
             (case List.find (fn (s, _) => Symbols.eq (s, i)) tys of
                NONE => raise TypeError
              | SOME (_, ty) => ty)
        | _ => raise TypeError)
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
             substInCon 0 [c] 0 c')
        | _ => raise TypeError)
    | Term_pack (c, e, cexists) =>
        (case weakHeadNormalize ctx cexists of
           Type_exists (k, c') =>
             (kindCheck ctx c k; typeCheck ctx e (substInCon 0 [c] 0 c');
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
             val tresult = substInCon 0 [] ~1 tresult
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
    in (typeCheck ctx e (substInCon 0 [ty] 0 c); ty) end
  | Term_unfold e =>
      (case weakHeadNormalize ctx (typeSynth ctx e) of
         (ty as (Type_rec c)) => substInCon 0 [ty] 0 c
       | _ => raise TypeError)
  | Term_ext m =>
      (case sgSynth ctx m of
         (_, Sg_type t) => t
       | _ => raise TypeError)


  (* ctx |> t <-- c *)
  and typeCheck ctx term con =
    (conEquiv ctx con (typeSynth ctx term) Kind_type) : unit

  (* This follows [Crary 2020] Appendix pretty closely, but refactored
  * such that it returns (fst m, sg). *)
  and sgSynth ctx module =
    case module of
      Module_var x => let
        val (c, sg) = lookupSg ctx x
        (* This is following [Crary 2020] where we selfify/singleton
        * the signature *)
      in (c, singletonSg c sg) end
    | Module_unit => (Con_unit, Sg_unit)
    | Module_con c => (c, Sg_kind (kindSynth ctx c))
    | Module_term t => (Con_unit, Sg_type (typeSynth ctx t))
    | Module_lam (x, s, m) =>
        (sgValid ctx s;
        let
          val (fstc, sg) = sgSynth (extendSg ctx x s) m
        in
          (Con_lam (fstSg s, fstc) , Sg_lam (s, sg))
        end)
    | Module_app (m, m') => let
        val (c1, (s, s')) = case sgSynth ctx m of
          (c1, Sg_lam lam) => (c1, lam)
        | _ => raise TypeError
        val c2 = sgCheck ctx m' s
      in (Con_app (c1, c2), substInSg 0 [c2] 0 s') end
    | Module_pair (m, x, m') => let
        val (a, sg) = sgSynth ctx m
        val (a', sg') = sgSynth (extendSg ctx x sg) m'
      in
        (* because Con_pair is not dependent, we need to subst a into a' *)
        (Con_pair (a, substInCon 0 [a] 0 a'),
        Sg_pair (sg, sg'))
      end
    | Module_tuple (m, m') => let
        val (a, sg) = sgSynth ctx m
        val (a', sg') = sgSynth ctx m'
      in
        (Con_pair (a, a'),
        (* Sg_pair is dependent, so we need to lift sg' out *)
        Sg_pair (sg, substInSg 0 nil 1 sg'))
      end
    | Module_proj1 m => let
        val (a, sg) = case sgSynth ctx m of
          (a, Sg_pair (sg, _)) => (a, sg)
        | _ => raise TypeError
      in (Con_proj1 a, sg) end
    | Module_proj2 m => let
        val (a, sg) = case sgSynth ctx m of
          (a, Sg_pair (_, sg')) => (a, substInSg 0 [Con_proj1 a] 0 sg')
        | _ => raise TypeError
      in (Con_proj2 a, sg) end
    | Module_let (e, x, m) => let
        val ty = typeSynth ctx e
      in sgSynth (extendType ctx x ty) m end
    | Module_circ l => (Con_unit, Sg_circ (psgSynth ctx l))

  and sgCheck ctx module sg = let
    val (a, sg') = sgSynth ctx module
  in subsg ctx sg' sg; a end

  and psgSynth ctx lmodule =
    case lmodule of
      Lmodule_ret m => Psg_shift (#2 (sgSynth ctx m))
    | Lmodule_seal (m, s) => (
        sgValid ctx s;
        sgCheck ctx m s;
        Psg_shift s
      )
    | Lmodule_bind (m, x, l) => let
        val p1 = case #2 (sgSynth ctx m) of
          Sg_circ p => p
        | _ => raise TypeError
        val (ctx', s) = psgExtract p1
        val p2 = psgSynth
          (extendSg (concat ctx ctx') x s) l

        fun construct ctxkinds =
          case ctxkinds of
            nil => Psg_exists (fstSg s, p2)
          | k :: rest => Psg_exists (k, construct rest)

      in construct (List.rev (kinds ctx')) end

  and psgExtract psg = let
    fun extract ctx psg =
      case psg of
        Psg_shift sg => (ctx, sg)
      | Psg_exists (k, psg) => extract (extendKind ctx k) psg
  in extract (Context.new ()) psg end
end
