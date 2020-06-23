
signature FREEVARS = sig
  type var
  type kind
  type con
  type value
  type exp

  type varset

  structure VarSet : SET
  where type elem = var
  where type set = varset

  val freeVarsValue : value -> VarSet.set
  val freeVarsExp : exp -> VarSet.set

  val freeVarsCountValue : value -> var -> int
  val freeVarsCountExp : exp -> var -> int
end
