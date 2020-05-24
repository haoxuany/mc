
functor ContextFun(
  structure Abt : ABT
  structure Subst : SUBST
  where type var = Abt.var
  where type kind = Abt.kind
  where type con = Abt.con
  where type term = Abt.term
) :> CONTEXT
where type var = Abt.var
where type kind = Abt.kind
where type con = Abt.con
where type term = Abt.term
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

  type t = {
    conOfKindN : int, (* for memoization *)
    conOfKind : kind list,
    termOfType : varTypeBind Dict.dict
  }

  fun new () = { conOfKindN = 0, conOfKind = nil, termOfType = Dict.empty }

  fun extendKind ({conOfKind, termOfType, conOfKindN } : t) kind =
    { conOfKind = kind :: conOfKind
    , termOfType = termOfType
    , conOfKindN = conOfKindN + 1 }

  fun extendType ({conOfKind, termOfType, conOfKindN} : t) var ty =
    { conOfKind = conOfKind
    , termOfType = Dict.insert termOfType var
        {ty = ty, index = conOfKindN}
    , conOfKindN = conOfKindN
    }

  fun lookupKind ({conOfKind, conOfKindN, ...} : t) i =
    (* when pulling out the ith judgement, this requires lifting
    * i + 1 times to avoid capture. +1 because lists are 0-indexed,
    * and we need to avoid 0 referring to itself (so lift one more time) *)
    substConInKind 0 nil (i + 1)
    (List.nth (conOfKind, i))

  fun lookupType ({termOfType, conOfKindN, ...} : t) var = let
    val {ty, index} = Dict.lookup termOfType var
  in substConInCon 0 nil (conOfKindN - index) ty end


end
