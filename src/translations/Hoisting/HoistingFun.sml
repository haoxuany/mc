
functor HoistingFun(
  structure DebugTranslation : DEBUGTRANSLATION
) = struct
  open HoistingConstructorTranslation
  structure DebugTranslation = DebugTranslation
  open DebugTranslation

  val new = Variable.new
  exception TypeError

end
