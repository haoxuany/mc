
structure Low = struct
  structure Abt = AbtFun(
    structure Variable = Variable
  )
  structure FreeVars = FreeVarsFun(
    structure Abt = Abt
  )
  structure Subst = SubstFun(
    structure Abt = Abt
  )
  structure Print = PrintFun(
    structure Abt = Abt
  )
end
