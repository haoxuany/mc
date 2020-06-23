
signature OPTIMIZE = sig
  type var
  type kind
  type con
  type value
  type exp

  (* substitutes variables that only occur a few times, removes
  * dead variables, and simplify redexes. This is no longer safe
  * for closure converted programs due to possibility of
  * substituting expressions with free variables. *)
  (* number represents how many transformations done *)
  val varRedexRemovalExp : exp -> (exp * int)
end
