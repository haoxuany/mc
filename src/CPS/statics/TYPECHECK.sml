
signature TYPECHECK = sig
  type context
  type var
  type kind
  type con
  type value
  type exp

  exception TypeError

  (* Bidirectional type checking *)
  (* ctx |> v --> c *)
  val typeValueSynth : context -> value -> con
  (* ctx |> v <-- c *)
  val typeValueCheck : context -> value -> con -> unit
  (* ctx |> e : void *)
  val typeExpCheck : context -> exp -> unit
end
