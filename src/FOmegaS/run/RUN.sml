
signature RUN = sig
  type var
  type kind
  type con
  type term

  (* Metatheory violation: doesn't typecheck, hence got stuck *)
  exception Stuck of term

  (* continuation to pass back to the caller if the term is a value *)
  exception TermVal

  (* throws TermVal if input is already a term *)
  val step : term -> term

  (* runs step until termination *)
  val run : term -> term
end
