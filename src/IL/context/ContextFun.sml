
functor ContextFun(
  structure Abt : ABT
  structure Subst : SUBST
  where type var = Abt.var
  where type kind = Abt.kind
  where type con = Abt.con
  where type term = Abt.term
  where type sg = Abt.sg
  where type module = Abt.module
) :> CONTEXT
where type var = Abt.var
where type kind = Abt.kind
where type con = Abt.con
where type term = Abt.term
where type sg = Abt.sg
where type module = Abt.module
= struct
  open Abt
  open Subst

  structure Dict = SplayRDict(
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


end
