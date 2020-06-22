
structure FOmegaS = struct
  structure Symbols = Symbols
  structure Abt = AbtFun(
    structure Variable = Variable
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
  structure Run = RunFun(
    structure Abt = Abt
    structure Subst = Subst
  )
  structure Print = PrintFun(
    structure Abt = Abt
  )
end
