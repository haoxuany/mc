
signature SUBST = sig
  type var
  type kind
  type con
  type value
  type exp

  type vardict
  (* Simultaneous substitutions *)
  val varSubst : (value * var) list -> vardict

  (* substInCon [
  * 0 . 1 . 2 . ... . shifts - 1 .
  * cons0 [^ shifts] . cons1 [^ shifts] . ... . cons(n-1) [^ shifts] .
  * ^ (shifts + l)
  * ] *)
  val substInCon : int -> con list -> int -> con -> con

  val substInKind : int -> con list -> int -> kind -> kind

  val substInValue : int -> con list -> int -> vardict -> value -> value

  val substInExp : int -> con list -> int -> vardict -> exp -> exp
end
