
signature FOMEGAS = sig
  (* Abstract Binding Tree of System F omega with Singleton Kinds *)
  structure Abt : ABT
  (* Representation of contexts (using debruijn indicies at the kind and con binding level) *)
  structure Context : CONTEXT
  (* Utilities for explicit substitutions [Abadi, Cardelli, Curien, Levy 1990] *)
  structure Subst : SUBST
  (* Constructor/Kind Equivalence, Validity, Weak Head Normalization and Subkinding [Stone 2000] *)
  structure Equiv : EQUIV
  (* Type checking and synthesis for terms *)
  structure TypeCheck : TYPECHECK
  (* Single step dynamics *)
  structure Run : RUN
end
