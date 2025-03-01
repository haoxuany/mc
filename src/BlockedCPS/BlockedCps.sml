
structure BlockedCps = struct
  structure Abt = AbtFun(
    structure Variable = Variable
  )
  structure FreeVars = FreeVarsFun(
    structure Abt = Abt
  )
  structure Subst = SubstFun(
    structure Abt = Abt
  )
  structure Context = ContextFun(
    structure Abt = Abt
    structure Subst = Subst
  )
  structure Equiv = EquivFun(
    structure Abt = Abt
    structure Context = Context
    structure Subst = Subst
  )
  structure TypeCheck = TypeCheckFun(
    structure Abt = Abt
    structure Context = Context
    structure Subst = Subst
    structure Equiv = Equiv
  )
  structure Print = PrintFun(
    structure Abt = Abt
  )
end
