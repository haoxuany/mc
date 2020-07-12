
functor ContextFun(
  structure Abt : ABT
  structure Subst : SUBST
  where type var = Abt.var
  where type kind = Abt.kind
  where type con = Abt.con
  where type term = Abt.term
  where type sg = Abt.sg
  where type psg = Abt.psg
  where type module = Abt.module
  where type lmodule = Abt.lmodule
) :> CONTEXT
where type var = Abt.var
where type kind = Abt.kind
where type con = Abt.con
where type term = Abt.term
where type sg = Abt.sg
where type psg = Abt.psg
where type module = Abt.module
where type lmodule = Abt.lmodule
= struct
  open Abt
  open Subst

  structure Dict = SplayDict(
    structure Key = Abt.Variable
  )

  (* Note: we need an index for this, which records the
  * size of the con : kind context at time of insertion.
  * This is because conOfKind can change, and we need to know
  * how much to lift the resulting type to avoid capture
  *)
  type varTypeBind = {
    ty : con,
    index : int
  }
  type varSgBind = {
    sg : sg,
    index : int
  }

  type t = {
    conOfKindN : int, (* for memoization *)
    conOfKind : kind list,
    termOfType : varTypeBind Dict.dict,
    moduleOfSg : varSgBind Dict.dict
  }

  fun new () =
    { conOfKindN = 0
    , conOfKind = nil
    , termOfType = Dict.empty
    , moduleOfSg = Dict.empty
    }

  fun extendKind ({conOfKind, termOfType, conOfKindN, moduleOfSg } : t) kind =
    { conOfKind = kind :: conOfKind
    , conOfKindN = conOfKindN + 1
    , termOfType = termOfType
    , moduleOfSg = moduleOfSg
    }

  fun extendType ({conOfKind, termOfType, conOfKindN, moduleOfSg } : t) var ty =
    { conOfKind = conOfKind
    , conOfKindN = conOfKindN
    , termOfType = Dict.insert termOfType var
        {ty = ty, index = conOfKindN}
    , moduleOfSg = moduleOfSg
    }

  fun extendSg ({conOfKind, termOfType, conOfKindN, moduleOfSg } : t) var sg =
    { conOfKind = (fstSg sg) :: conOfKind
    , conOfKindN = conOfKindN + 1
    , termOfType = termOfType
    , moduleOfSg = Dict.insert moduleOfSg var
        {sg = sg, index = conOfKindN}
    }

  fun lookupKind ({conOfKind, conOfKindN, ...} : t) i =
    (* when pulling out the ith judgement, this requires lifting
    * i + 1 times to avoid capture. +1 because lists are 0-indexed,
    * and we need to avoid 0 referring to itself (so lift one more time) *)
    substInKind 0 nil (i + 1)
    (List.nth (conOfKind, i))

  fun lookupType ({termOfType, conOfKindN, ...} : t) var = let
    val {ty, index} = Dict.lookup termOfType var
  in substInCon 0 nil (conOfKindN - index) ty end

  fun lookupSg ({moduleOfSg, conOfKindN, ...} : t) var = let
    val {sg, index} = Dict.lookup moduleOfSg var
  in (
    Con_var (conOfKindN - index - 1),
    substInSg 0 nil (conOfKindN - index) sg
  ) end

  fun concat (ctx : t) (ctx' : t) : t = let
    val
    { conOfKindN = n1
    , conOfKind = ks1
    , termOfType = ts1
    , moduleOfSg = ms1
    } = ctx
    val
    { conOfKindN = n2
    , conOfKind = ks2
    , termOfType = ts2
    , moduleOfSg = ms2
    } = ctx'

    val latter = fn (_, _, a) => a
  in
    { conOfKindN = n1 + n2
    , conOfKind = ks2 @ ks1 (* because everything is in reverse here *)
    , termOfType = Dict.union ts1
        (Dict.map (fn {ty, index} => {ty = ty, index = index + n1}) ts2)
        latter
    , moduleOfSg = Dict.union ms1
        (Dict.map (fn {sg, index} => {sg = sg, index = index + n1}) ms2)
        latter
    }
  end

  fun kinds (ctx : t) = #conOfKind ctx
end
