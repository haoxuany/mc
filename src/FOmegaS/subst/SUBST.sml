
signature SUBST = sig
  type var
  type kind
  type con
  type term

  type vardict
  (* Simultaneous substitutions *)
  val varSubst : (term * var) list -> vardict

  (* substInCon [
  * 0 . 1 . 2 . ... . shifts - 1 .
  * cons0 [^ shifts] . cons1 [^ shifts] . ... . cons(n-1) [^ shifts] .
  * ^ (shifts + l)
  * ] *)
  val substInCon : int -> con list -> int -> con -> con

  val substInKind : int -> con list -> int -> kind -> kind

  val substInTerm : int -> con list -> int -> vardict -> term -> term
end
