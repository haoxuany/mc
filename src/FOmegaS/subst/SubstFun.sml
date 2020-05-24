
functor SubstFun(
  structure Abt : ABT
) : SUBST  = struct

  open Abt

  val $ = fn (a, b) => a b
  infixr 4 $

  val (==) = Variable.eq
  infixr 1 ==

  (* easy case: direct variable map substitution *)
  (* we assume globally unique variables through Variable,
  * to cut down cases of capture (which is computationally heavy) *)
  fun substTerm (terma : term) x termb = let
    val subst = substTerm terma x
    val termb =
      case termb of
        Term_var v => if v == x then terma else termb
      | Term_let (x, a, b) =>
          Term_let (x, subst a, subst b)
      | Term_lam (x, c, rest) =>
          Term_lam (x, c, subst rest)
      | Term_app (a, b) =>
          Term_app (subst a, subst b)
      | Term_polylam (k, a) =>
          Term_polylam (k, subst a)
      | Term_polyapp (a, c) =>
          Term_polyapp (subst a, c)
      | Term_pack (c, a, c') =>
          Term_pack (c, subst a, c')
      | Term_unpack (x, a, b) =>
          Term_unpack (x, subst a, subst b)
      | Term_tuple terms =>
          Term_tuple (ParList.map subst terms)
      | Term_proj (a, i) =>
          Term_proj (subst a, i)
      | Term_inj (c, i, a) =>
          Term_inj (c, i, subst a)
      | Term_case (a, cases) =>
          Term_case (subst a, ParList.map (fn (i, a) => (i, subst a)) cases)
  in termb end

  (* hard case: DeBruijn Explicit substitutions of second order (con + kind) *)

  (* substConInCon [
  * 0 . 1 . 2 . ... . shifts - 1 .
  * cons0 [^ shifts] . cons1 [^ shifts] . ... . cons(n-1) [^ shifts] .
  * ^ (shifts + l)
  * ] *)
  fun substConInCon shifts cons n lifts c = let
    val substCon = substConInCon shifts cons n lifts
    val substConB = substConInCon (shifts + 1) cons n lifts
    val substKind = substConInKind shifts cons n lifts

    val con =
      case c of
        Type_arrow (c1, c2) => Type_arrow (substCon c1, substCon c2)
      | Type_forall (k, c') =>
          (* c' is under a binder *)
          Type_forall (substKind k, substConB c')
      | Type_exists (k, c') =>
          (* c' is under a binder *)
          Type_exists (substKind k, substConB c')
      | Type_product tys =>
          Type_product $ ParList.map substCon tys
      | Type_sum tys =>
          Type_sum $ ParList.map substCon tys
      | Type_exn => Type_exn
      | Con_var (v as i) =>
          if i < shifts then c (* in the first range [id substs]*)
          else
            let
              val i = i - shifts
            in
              (* in second range [tycon substs] *)
              if i < n then substConInCon 0 [] 0 shifts $ List.nth (cons, i)
              (* in third range [lifts] *)
              else Con_var $ v - n (* - shifts + shifts *) + lifts
            end
      | Con_lam (k, c) =>
          (* c is under a binder *)
          Con_lam (substKind k, substConB c)
      | Con_app (c, c') =>
          Con_app (substCon c, substCon c')
      | Con_pair (c, c') =>
          Con_pair (substCon c, substCon c')
      | Con_proj1 c => Con_proj1 $ substCon c
      | Con_proj2 c => Con_proj2 $ substCon c
  in con end

  and substConInKind shifts cons n lifts k = let
    val substCon = substConInCon shifts cons n lifts
    val substKind = substConInKind shifts cons n lifts
    val substKindB = substConInKind (shifts + 1) cons n lifts

    val kind = case k of
        Kind_type => Kind_type
      | Kind_pi (k, k') =>
          (* k' contains a binder (referring to k) *)
          Kind_pi (substKind k, substKindB k')
      | Kind_sigma (k, k') =>
          (* k' contains a binder (referring to k) *)
          Kind_sigma (substKind k, substKindB k')
      | Kind_singleton c =>
          Kind_singleton $ substCon c
  in kind end

  fun substConInTerm shifts cons n lifts t = let
    val substKind = substConInKind shifts cons n lifts
    val substCon = substConInCon shifts cons n lifts
    val substTerm = substConInTerm shifts cons n lifts
    val substTermB = substConInTerm (shifts + 1) cons n lifts

    val term = case t of
        Term_var v => t
      | Term_let (v, t, t') =>
          Term_let (v, substTerm t, substTerm t')
      | Term_lam (v, c, t) =>
          (* only variable binder here, no debruijn binder *)
          Term_lam (v, substCon c, substTerm t)
      | Term_app (t, t') =>
          Term_app (substTerm t, substTerm t')
      | Term_polylam (k, t) =>
          (* t is under a binder *)
          Term_polylam (substKind k, substTermB t)
      | Term_polyapp (t, c) =>
          Term_polyapp (substTerm t, substCon c)
      | Term_pack (c, t, c') =>
          Term_pack (substCon c, substTerm t, substCon c')
      | Term_unpack (v, t, t') =>
          (* t' is under a debruijn binder from tycon binding *)
          Term_unpack (v, substTerm t, substTermB t')
      | Term_tuple terms =>
          Term_tuple $ ParList.map substTerm terms
      | Term_proj (t, i) =>
          Term_proj (substTerm t, i)
      | Term_inj (c, i, t) =>
          Term_inj (substCon c, i, substTerm t)
      | Term_case (t, cases) =>
          Term_case (substTerm t,
          ParList.map (fn (v, t) => (v, substTerm t)) cases)
  in term end

  val substConInCon = fn shifts => fn cons => fn lifts =>
    substConInCon shifts cons (List.length cons) lifts
  val substConInKind = fn shifts => fn cons => fn lifts =>
    substConInKind shifts cons (List.length cons) lifts
  val substConInTerm = fn shifts => fn cons => fn lifts =>
    substConInTerm shifts cons (List.length cons) lifts
end
