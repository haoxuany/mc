
signature RUN = sig
  type var
  type kind
  type con
  type value
  type exp

  (* Metatheory violation: doesn't typecheck, hence got stuck *)
  exception Stuck of exp

  (* throws TermVal if input is already a term *)
  val step : exp -> exp

  (* runs step until termination *)
  val run : exp -> exp
end
