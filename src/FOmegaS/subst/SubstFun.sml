
functor SubstFun(
  structure Abt : ABT
) : SUBST  = struct

  open Abt

  val $ = fn (a, b) => a b
  infixr 4 $

  structure Dict = SplayRDict(
    structure Key = Abt.Variable
  )

  type vardict = term Dict.dict
  fun varSubst list =
    ParList.foldl
    (fn ((term, var), dict) => Dict.insert dict var term)
    Dict.empty
    list

  (* DeBruijn Explicit substitutions of second order (con + kind) *)

  (* substInCon [
  * 0 . 1 . 2 . ... . shifts - 1 .
  * cons0 [^ shifts] . cons1 [^ shifts] . ... . cons(n-1) [^ shifts] .
  * ^ (shifts + l)
  * ] *)
  fun substInCon shifts cons n lifts c = let
    val substCon = substInCon shifts cons n lifts
    val substConB = substInCon (shifts + 1) cons n lifts
    val substKind = substInKind shifts cons n lifts

    val con =
      case c of
        Type_arrow (c, c') => Type_arrow (substCon c, substCon c')
      | Type_productfix tys => Type_productfix $ ParList.map substCon tys
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
      | Type_rec ty =>
          (* ty is under a binder from rec *)
          Type_rec $ substConB ty
      | Type_exn => Type_exn
      | Con_var (v as i) =>
          if i < shifts then c (* in the first range [id substs]*)
          else
            let
              val i = i - shifts
            in
              (* in second range [tycon substs] *)
              if i < n then substInCon 0 [] 0 shifts $ List.nth (cons, i)
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
      | Con_unit => Con_unit
  in con end

  and substInKind shifts cons n lifts k = let
    val substCon = substInCon shifts cons n lifts
    val substKind = substInKind shifts cons n lifts
    val substKindB = substInKind (shifts + 1) cons n lifts

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
      | Kind_unit => Kind_unit
  in kind end

  fun substConInTerm shifts cons n lifts t = let
    val substKind = substInKind shifts cons n lifts
    val substCon = substInCon shifts cons n lifts
    val substConB = substInCon (shifts + 1) cons n lifts
    val substTerm = substConInTerm shifts cons n lifts
    val substTermB = substConInTerm (shifts + 1) cons n lifts

    val term = case t of
        Term_var v => t
      | Term_let (t, v, t') =>
          Term_let (substTerm t, v, substTerm t')
      | Term_fixlam lams =>
          Term_fixlam (ParList.map
            (fn (f, x, c, t, c') => (f, x, substCon c, substTerm t, substCon c'))
            lams
          )
      | Term_pick (t, i) =>
          Term_pick (substTerm t, i)
      | Term_app (t, t') =>
          Term_app (substTerm t, substTerm t')
      | Term_polylam (k, t) =>
          (* t is under a binder *)
          Term_polylam (substKind k, substTermB t)
      | Term_polyapp (t, c) =>
          Term_polyapp (substTerm t, substCon c)
      | Term_pack (c, t, c') =>
          Term_pack (substCon c, substTerm t, substCon c')
      | Term_unpack (t, v, t') =>
          (* t' is under a debruijn binder from tycon binding *)
          Term_unpack (substTerm t, v, substTermB t')
      | Term_tuple terms =>
          Term_tuple $ ParList.map substTerm terms
      | Term_proj (t, i) =>
          Term_proj (substTerm t, i)
      | Term_inj (c, i, t) =>
          Term_inj (substCon c, i, substTerm t)
      | Term_case (t, cases) =>
          Term_case (substTerm t,
          ParList.map (fn (v, t) => (v, substTerm t)) cases)
      | Term_fold (c, t) =>
          (* c is under a binder *)
          Term_fold (substConB c, substTerm t)
      | Term_unfold t => Term_unfold (substTerm t)
  in term end

  (* this function alpha varies variables, which is slower, in order
  * to handle variable substitution *)
  and substConTermInTerm shifts cons n lifts dict t = let
    val substKind = substInKind shifts cons n lifts
    val substCon = substInCon shifts cons n lifts
    val substConB = substInCon (shifts + 1) cons n lifts
    val substTerm = substConTermInTerm shifts cons n lifts
    val substTermB = substConTermInTerm (shifts + 1) cons n lifts

    fun alphaNew x = let
      val y = Variable.new ()
      val dict = Dict.insert dict x (Term_var y)
    in (y, dict) end

    val term = case t of
        Term_var v =>
          (case Dict.find dict v of
             NONE => t
           | SOME e =>
               if shifts = 0 then e
               (* we could be under binders here, in which case we
               * need to lift e's constructors to the right aligment *)
               else substConInTerm 0 nil 0 shifts e)
      | Term_let (t, v, t') => let
          val (v, dictA) = alphaNew v
        in Term_let (substTerm dict t, v, substTerm dictA t') end
      | Term_fixlam lams => let
          val (fs, dict) = List.foldr
          (fn ((f, _, c, _, c'), (fs, dict)) => let
            val f' = Variable.new ()
            val dict = Dict.insert dict f (Term_var f')
          in (f' :: fs, dict) end)
          (nil, dict)
          lams
        in
          Term_fixlam (ParList.map
            (fn ((_, x, c, t, c'), f) => let
              val y = Variable.new ()
              val dict = Dict.insert dict x (Term_var y)
            in
              (f, y, substCon c, substTerm dict t, substCon c')
            end)
            (ListPair.zip (lams, fs))
          )
        end
      | Term_pick (t, i) =>
          Term_pick (substTerm dict t, i)
      | Term_app (t, t') =>
          Term_app (substTerm dict t, substTerm dict t')
      | Term_polylam (k, t) =>
          (* t is under a binder *)
          Term_polylam (substKind k, substTermB dict t)
      | Term_polyapp (t, c) =>
          Term_polyapp (substTerm dict t, substCon c)
      | Term_pack (c, t, c') =>
          Term_pack (substCon c, substTerm dict t, substCon c')
      | Term_unpack (t, v, t') => let
          val (v, dictA) = alphaNew v
          (* t' is under a tycon and var binder *)
        in Term_unpack (substTerm dict t, v, substTermB dictA t') end
      | Term_tuple terms =>
          Term_tuple $ ParList.map (substTerm dict) terms
      | Term_proj (t, i) =>
          Term_proj (substTerm dict t, i)
      | Term_inj (c, i, t) =>
          Term_inj (substCon c, i, substTerm dict t)
      | Term_case (t, cases) =>
          Term_case (substTerm dict t,
          ParList.map
          (fn (v, t) => let
            val (v, dictA) = alphaNew v
          in (v, substTerm dictA t) end)
          cases)
      | Term_fold (c, t) =>
          (* c is under a binder *)
          Term_fold (substConB c, substTerm dict t)
      | Term_unfold t => Term_unfold (substTerm dict t)
  in term end

  val substInCon = fn shifts => fn cons => fn lifts =>
    substInCon shifts cons (List.length cons) lifts
  val substInKind = fn shifts => fn cons => fn lifts =>
    substInKind shifts cons (List.length cons) lifts
  fun substInTerm shifts cons lifts dict =
    if Dict.isEmpty dict then
      substConInTerm shifts cons (List.length cons) lifts
    else substConTermInTerm shifts cons (List.length cons) lifts dict
end
