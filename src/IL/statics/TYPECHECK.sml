
signature TYPECHECK = sig
  type context
  type var
  type kind
  type con
  type term
  type sg
  type psg
  type module
  type lmodule

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
  (* ctx |> lm --> psg *)
  val psgSynth : context -> lmodule -> psg
  (* psg => ctx.sg *)
  val psgExtract : psg -> (context * sg)
end
