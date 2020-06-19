
signature FREEVARS = sig
  type var
  type kind
  type con
  type value
  type exp
  type block
  type program

  type varset

  structure VarSet : SET
  where type elem = var
  where type set = varset

  val freeVarsValue : value -> VarSet.set
  val freeVarsExp : exp -> VarSet.set
  val freeVarsBlock : block -> VarSet.set
end
