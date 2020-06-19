
functor SubstFun(
  structure Abt : ABT
) : SUBST  = struct

  open Abt

  val $ = fn (a, b) => a b
  infixr 4 $

  structure Dict = SplayRDict(
    structure Key = Abt.Variable
  )

  type vardict = value Dict.dict
  fun varSubst list =
    ParList.foldl
    (fn ((value, var), dict) => Dict.insert dict var value)
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
        Type_not cs => Type_not (ParList.map substCon cs)
      | Type_productfix tys =>
          Type_productfix $ ParList.map substCon tys
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

  fun substConInValue shifts cons n lifts value = let
    val substKind = substInKind shifts cons n lifts
    val substCon = substInCon shifts cons n lifts
    val substConB = substInCon (shifts + 1) cons n lifts
    val substValue = substConInValue shifts cons n lifts
    val substValueB = substConInValue (shifts + 1) cons n lifts
    val substExp = substConInExp shifts cons n lifts
    val substExpB = substConInExp (shifts + 1) cons n lifts

    val value = case value of
        Value_var v => value
      | Value_pick (v, i) =>
          Value_pick (substValue v, i)
      | Value_pack (c, v, c') =>
          Value_pack (substCon c, substValue v, substCon c')
      | Value_polyapp (v, c) =>
          Value_polyapp (substValue v, substCon c)
      | Value_tuple vals =>
          Value_tuple (ParList.map substValue vals)
      | Value_inj (c, i, v) =>
          Value_inj (substCon c, i, substValue v)
      | Value_fold (c, v) =>
          (* c is under a binder *)
          Value_fold (substConB c, substValue v)
  in value end

  (* this function alpha varies variables, which is slower, in order
  * to handle variable substitution *)
  and substConValueInValue shifts cons n lifts dict value = let
    val substKind = substInKind shifts cons n lifts
    val substCon = substInCon shifts cons n lifts
    val substConB = substInCon (shifts + 1) cons n lifts
    val substValue = substConValueInValue shifts cons n lifts
    val substValueB = substConValueInValue (shifts + 1) cons n lifts
    val substExp = substConValueInExp shifts cons n lifts
    val substExpB = substConValueInExp (shifts + 1) cons n lifts

    fun alphaNew x = let
      val y = Variable.new ()
      val dict = Dict.insert dict x (Value_var y)
    in (y, dict) end

    val value = case value of
        Value_var v =>
          (case Dict.find dict v of
             NONE => value
           | SOME v =>
               if shifts = 0 then v
               (* we could be under binders here, in which case we
               * need to lift e's constructors to the right aligment *)
               else substConInValue 0 nil 0 shifts v)
      | Value_pick (v, i) =>
          Value_pick (substValue dict v, i)
      | Value_pack (c, v, c') =>
          Value_pack (substCon c, substValue dict v, substCon c')
      | Value_polyapp (v, c) =>
          Value_polyapp (substValue dict v, substCon c)
      | Value_tuple vals =>
          Value_tuple (ParList.map (substValue dict) vals)
      | Value_inj (c, i, v) =>
          Value_inj (substCon c, i, substValue dict v)
      | Value_fold (c, v) =>
          (* c is under a binder *)
          Value_fold (substConB c, substValue dict v)
  in value end

  and substConInExp shifts cons n lifts exp = let
    val substKind = substInKind shifts cons n lifts
    val substCon = substInCon shifts cons n lifts
    val substConB = substInCon (shifts + 1) cons n lifts
    val substValue = substConInValue shifts cons n lifts
    val substValueB = substConInValue (shifts + 1) cons n lifts
    val substExp = substConInExp shifts cons n lifts
    val substExpB = substConInExp (shifts + 1) cons n lifts

    val exp = case exp of
        Exp_app (v, v') =>
          Exp_app (substValue v, ParList.map substValue v')
      | Exp_unpack (v, x, e) =>
          Exp_unpack (substValue v, x, substExpB e)
      | Exp_proj (v, i, x, e) =>
          Exp_proj (substValue v, i, x, substExp e)
      | Exp_case (v, cases) =>
          Exp_case (substValue v,
            ParList.map (fn (x, e) => (x, substExp e)) cases)
      | Exp_unfold (v, x, e) =>
          Exp_unfold (substValue v, x, substExp e)
      | Exp_let (v, x, e) =>
          Exp_let (substValue v, x, substExp e)
      | Exp_exit _ => exp
  in exp end

  (* this function alpha varies variables, which is slower, in order
  * to handle variable substitution *)
  and substConValueInExp shifts cons n lifts dict exp = let
    val substKind = substInKind shifts cons n lifts
    val substCon = substInCon shifts cons n lifts
    val substConB = substInCon (shifts + 1) cons n lifts
    val substValue = substConValueInValue shifts cons n lifts
    val substValueB = substConValueInValue (shifts + 1) cons n lifts
    val substExp = substConValueInExp shifts cons n lifts
    val substExpB = substConValueInExp (shifts + 1) cons n lifts

    fun alphaNew x = let
      val y = Variable.new ()
      val dict = Dict.insert dict x (Value_var y)
    in (y, dict) end

    val exp = case exp of
        Exp_app (v, v') =>
          Exp_app (substValue dict v, ParList.map (substValue dict) v')
      | Exp_unpack (v, x, e) => let
          val (x, dictA) = alphaNew x
        in Exp_unpack (substValue dict v, x, substExpB dictA e) end
      | Exp_proj (v, i, x, e) => let
          val (x, dictA) = alphaNew x
        in Exp_proj (substValue dict v, i, x, substExp dictA e) end
      | Exp_case (v, cases) =>
          Exp_case (substValue dict v,
            ParList.map
            (fn (x, e) => let
              val (x, dict) = alphaNew x
            in (x, substExp dict e) end)
            cases)
      | Exp_unfold (v, x, e) => let
          val (x, dictA) = alphaNew x
        in Exp_unfold (substValue dict v, x, substExp dictA e) end
      | Exp_let (v, x, e) => let
          val (x, dictA) = alphaNew x
        in Exp_let (substValue dict v, x, substExp dictA e) end
      | Exp_exit _ => exp
  in exp end

  and substConInBlock shifts cons n lifts block = let
    val substKind = substInKind shifts cons n lifts
    val substCon = substInCon shifts cons n lifts
    val substExp = substConInExp shifts cons n lifts
    val substBlockB = substConInBlock (shifts + 1) cons n lifts

    val block = case block of
        Block_fixlam lams =>
          Block_fixlam (ParList.map (fn (f, bnds, e) =>
            (f,
            ParList.map (fn (x, c) => (x, substCon c)) bnds,
            substExp e)
          ) lams)
      | Block_lam (bnds, e) =>
          Block_lam (
            ParList.map (fn (x, c) => (x, substCon c)) bnds,
            substExp e
          )
      | Block_polylam (k, b) =>
          Block_polylam (substKind k, substBlockB b)
  in block end

  and substConValueInBlock shifts cons n lifts dict block = let
    val substKind = substInKind shifts cons n lifts
    val substCon = substInCon shifts cons n lifts
    val substExp = substConValueInExp shifts cons n lifts
    val substBlockB = substConValueInBlock (shifts + 1) cons n lifts

    val block = case block of
        Block_fixlam lams => let
          val (fs, dict) = List.foldr
          (fn ((f, _, _), (fs, dict)) => let
            val f' = Variable.new ()
            val dict = Dict.insert dict f (Value_var f')
          in (f' :: fs, dict) end)
          (nil, dict)
          lams
        in Block_fixlam (ParList.map
          (fn ((_, bnds, e), f) => let
            val (bnds, dict) = List.foldr
              (fn ((x, c), (bnds, dict)) => let
                val y = Variable.new ()
                val dict = Dict.insert dict x (Value_var y)
              in ((y, substCon c) :: bnds, dict) end)
              (nil, dict)
              bnds
          in (f, bnds, substExp dict e) end)
          (ListPair.zip (lams, fs))) end
      | Block_lam (bnds, t) => let
          val (bnds, dict) = List.foldr
            (fn ((x, c), (bnds, dict)) => let
              val y = Variable.new ()
                val dict = Dict.insert dict x (Value_var y)
              in ((y, substCon c) :: bnds, dict) end)
            (nil, dict)
            bnds
        in Block_lam (bnds, substExp dict t) end
      | Block_polylam (k, b) =>
          Block_polylam (substKind k, substBlockB dict b)
  in block end

  and substConInProgram shifts cons n lifts program = let
    val substExp = substConInExp shifts cons n lifts
    val substBlock = substConInBlock shifts cons n lifts
    val substProgram = substConInProgram shifts cons n lifts

    val program = case program of
        Program_dyn e => Program_dyn (substExp e)
      | Program_bnd (b, x, p) =>
          Program_bnd (substBlock b, x, substProgram p)
  in program end

  and substConValueInProgram shifts cons n lifts dict program = let
    val substExp = substConValueInExp shifts cons n lifts
    val substBlock = substConValueInBlock shifts cons n lifts
    val substProgram = substConValueInProgram shifts cons n lifts

    fun alphaNew x = let
      val y = Variable.new ()
      val dict = Dict.insert dict x (Value_var y)
    in (y, dict) end

    val program = case program of
        Program_dyn e => Program_dyn (substExp dict e)
      | Program_bnd (b, x, p) => let
          val (y, dictA) = alphaNew x
        in Program_bnd (substBlock dict b, y, substProgram dictA p) end
  in program end


  val substInCon = fn shifts => fn cons => fn lifts =>
    substInCon shifts cons (List.length cons) lifts
  val substInKind = fn shifts => fn cons => fn lifts =>
    substInKind shifts cons (List.length cons) lifts
  fun substInValue shifts cons lifts dict =
    if Dict.isEmpty dict then
      substConInValue shifts cons (List.length cons) lifts
    else substConValueInValue shifts cons (List.length cons) lifts dict
  fun substInExp shifts cons lifts dict =
    if Dict.isEmpty dict then
      substConInExp shifts cons (List.length cons) lifts
    else substConValueInExp shifts cons (List.length cons) lifts dict
  fun substInBlock shifts cons lifts dict =
    if Dict.isEmpty dict then
      substConInBlock shifts cons (List.length cons) lifts
    else substConValueInBlock shifts cons (List.length cons) lifts dict
  fun substInProgram shifts cons lifts dict =
    if Dict.isEmpty dict then
      substConInProgram shifts cons (List.length cons) lifts
    else substConValueInProgram shifts cons (List.length cons) lifts dict
end
