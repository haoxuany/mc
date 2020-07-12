
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
  (* See [Crary 2020, Figure 8] *)
  (* ctx |> m --> sg which returns (fst(m), sg) *)
  val sgSynth : context -> module -> (con * sg)
  (* ctx |> m <-- sg which returns fst(m), raises TypeError otherwise *)
  val sgCheck : context -> module -> sg -> con
end
