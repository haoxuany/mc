
signature SUBST = sig
  type var
  type kind
  type con
  type term

  val substTerm : term -> var -> term -> term

  (* substConInCon [
  * 0 . 1 . 2 . ... . shifts - 1 .
  * cons0 [^ shifts] . cons1 [^ shifts] . ... . cons(n-1) [^ shifts] .
  * ^ (shifts + l)
  * ] *)
  val substConInCon : int -> con list -> int -> con -> con

  val substConInKind : int -> con list -> int -> kind -> kind

  val substConInTerm : int -> con list -> int -> term -> term
end
