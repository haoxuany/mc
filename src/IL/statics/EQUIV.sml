
signature EQUIV = sig
  type context
  type var
  type kind
  type con
  type term
  type sg
  type module

  (* see Stone's PhD Thesis on what any of this is *)

  (* Singleton kind encodings for higher kinds: Figure 2.4 *)
  (* S(c :: k) := k' *)
  val singleton : con -> kind -> kind
  (* Derived forms for higher singletons for signatures: [Crary 2019, Figure 3] *)
  val singletonSg : con -> sg -> sg

  exception TypeError

  (* Kind validity: Figure 4.2 *)
  (* ctx |> k *)
  val kindValid : context -> kind -> unit
  (* Subkinding: Figure 4.2 *)
  (* ctx |> k <= k' *)
  val subkind : context -> kind -> kind -> unit
  (* Kind equivalence: Figure 4.2 *)
  (* ctx |> k <=> k' *)
  val kindEquiv : context -> kind -> kind -> unit
  (* Kind synthesis: Figure 4.3 *)
  (* ctx |> c --> k *)
  val kindSynth : context -> con -> kind
  (* Kind checking: Figure 4.3 *)
  (* ctx |> c <-- k *)
  val kindCheck : context -> con -> kind -> unit
  (* Kind extraction (of a path): Figure 4.4 *)
  (* ctx |> c ^ k *)
  val kindExtract : context -> con -> kind
  (* Weak head reduction + normalization: Figure 4.4 *)
  (* ctx |> c -> c' *)
  val weakHeadNormalize : context -> con -> con
  (* Algorithmic constructor equivalence: Figure 4.4 *)
  (* ctx |> c <=> c' :: k *)
  val conEquiv : context -> con -> con -> kind -> unit
  (* Algorithmic path equivalence: Figure 4.4 *)
  (* ctx |> p <-> p ^ k *)
  val pathEquiv : context -> con -> con -> kind
  (* The rest of these judgements are from [Crary 2017] Appendix *)
  (* ctx |> sg : sig *)
  val sgValid : context -> sg -> unit
  (* ctx |> sg === sg' : sig *)
  val sgEquiv : context -> sg -> sg -> unit
  (* ctx |> sg <= sg' : sig *)
  val subsg : context -> sg -> sg -> unit
end
