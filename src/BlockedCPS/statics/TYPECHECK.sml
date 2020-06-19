
signature TYPECHECK = sig
  type context
  type var
  type kind
  type con
  type value
  type exp
  type block
  type program

  exception TypeError

  (* Bidirectional type checking *)
  (* ctx |> v --> c *)
  val typeValueSynth : context -> value -> con
  (* ctx |> v <-- c *)
  val typeValueCheck : context -> value -> con -> unit
  (* ctx |> e : void *)
  val typeExpCheck : context -> exp -> unit
  (* ctx |> b --> c *)
  val typeBlockSynth : context -> block -> con
  (* ctx |> b <-- c *)
  val typeBlockCheck : context -> block -> con -> unit
  (* ctx |> p : void *)
  val typeProgramCheck : context -> program -> unit
end
