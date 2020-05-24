
signature TYPECHECK = sig
  type context
  type var
  type kind
  type con
  type term

  exception TypeError

  (* Bidirectional type checking *)
  (* ctx |> t --> c *)
  val typeSynth : context -> term -> con
  (* ctx |> t <-- c *)
  val typeCheck : context -> term -> con -> unit
end
