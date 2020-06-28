
signature TYPECHECK = sig
  type context
  type var
  type kind
  type con
  type term
  type sg
  type module

  exception TypeError

  (* Bidirectional type checking *)
  (* ctx |> t --> c *)
  val typeSynth : context -> term -> con
  (* ctx |> t <-- c *)
  val typeCheck : context -> term -> con -> unit
  (* See [Crary 2017] and [Crary 2019] for the formulation of these *)
  (* ctx |>p m --> sg if returns (SOME fst(m), sg) *)
  (* ctx |>I m --> sg if returns (NONE, sg) *)
  val sgSynth : context -> module -> (con option * sg)
  (* ctx |>p m <-- sg if returns SOME (fst(m)) *)
  (* ctx |>I m <-- sg if returns NONE *)
  val sgCheck : context -> module -> sg -> con option
end
