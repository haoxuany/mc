
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

  fun substValueInValue dict value = let
    val substValue = substValueInValue
    val substExp = substValueInExp

    fun alphaNew x = let
      val y = Variable.new ()
      val dict = Dict.insert dict x (Value_var y)
    in (y, dict) end

    val value = case value of
        Value_var v =>
          (case Dict.find dict v of
             NONE => value
           | SOME v => v)
      | Value_pick (v, i) =>
          Value_pick (substValue dict v, i)
      | Value_tuple vals =>
          Value_tuple (ParList.map (substValue dict) vals)
      | Value_inj (c, i, v) =>
          Value_inj (c, i, substValue dict v)
  in value end

  and substValueInExp dict exp = let
    val substValue = substValueInValue
    val substExp = substValueInExp

    fun alphaNew x = let
      val y = Variable.new ()
      val dict = Dict.insert dict x (Value_var y)
    in (y, dict) end

    val exp = case exp of
        Exp_app (v, v') =>
          Exp_app (substValue dict v, ParList.map (substValue dict) v')
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
      | Exp_let (v, x, e) => let
          val (x, dictA) = alphaNew x
        in Exp_let (substValue dict v, x, substExp dictA e) end
      | Exp_exit _ => exp
  in exp end

  and substValueInBlock dict block = let
    val substExp = substValueInExp
    val substBlock = substValueInBlock

    val block = case block of
        Block_fixlam lams => let
          val (fs, dict) = List.foldr
          (fn ((_, f, _, _), (fs, dict)) => let
            val f' = Variable.new ()
            val dict = Dict.insert dict f (Value_var f')
          in (f' :: fs, dict) end)
          (nil, dict)
          lams
        in Block_fixlam (ParList.map
          (fn ((s, _, xs, e), f) => let
            val (xs, dict) = List.foldr
              (fn (x, (xs, dict)) => let
                val y = Variable.new ()
                val dict = Dict.insert dict x (Value_var y)
              in (y :: xs, dict) end)
              (nil, dict)
              xs
          in (s, f, xs, substExp dict e) end)
          (ListPair.zip (lams, fs))) end
      | Block_lam (xs, t) => let
          val (xs, dict) = List.foldr
            (fn (x, (xs, dict)) => let
              val y = Variable.new ()
                val dict = Dict.insert dict x (Value_var y)
              in (y :: xs, dict) end)
            (nil, dict)
            xs
        in Block_lam (xs, substExp dict t) end
  in block end

  and substValueInProgram dict program = let
    val substExp = substValueInExp
    val substBlock = substValueInBlock

    val program = case program of
        Program (bnds, e) => let
          fun subst bnds dict substbnds =
            case bnds of
              nil => Program (List.rev substbnds, substExp dict e)
            | (x, b) :: rest => let
                val y = Variable.new ()
                val dict' = Dict.insert dict x (Value_var y)
              in subst rest dict' ((y, substBlock dict b) :: substbnds) end
        in subst bnds dict nil end
  in program end

  fun substInValue dict value =
    if Dict.isEmpty dict then value
    else substValueInValue dict value
  fun substInExp dict exp =
    if Dict.isEmpty dict then exp
    else substValueInExp dict exp
  fun substInBlock dict block =
    if Dict.isEmpty dict then block
    else substValueInBlock dict block
  fun substInProgram dict program =
    if Dict.isEmpty dict then program
    else substValueInProgram dict program
end
