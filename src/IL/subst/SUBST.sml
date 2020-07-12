
signature SUBST = sig
  type var
  type kind
  type con
  type term
  type sg
  type psg
  type module
  type lmodule

  type 'a vardict
  (* Simultaneous substitutions *)
  val varSubst : ('a * var) list -> 'a vardict

  (* substInCon [
  * 0 . 1 . 2 . ... . shifts - 1 .
  * cons0 [^ shifts] . cons1 [^ shifts] . ... . cons(n-1) [^ shifts] .
  * ^ (shifts + l)
  * ] *)
  val substInCon : int -> con list -> int -> con -> con

  val substInKind : int -> con list -> int -> kind -> kind

  val substInTerm : int -> con list -> int -> term vardict -> term -> term

  val substInSg : int -> con list -> int -> sg -> sg

  val substInPsg : int -> con list -> int -> psg -> psg

  (* TODO: this could be generalized to substitute for module vars as
  * well, but is a bit tricky to implement *)
  val substInModule : int -> con list -> int -> module -> module

  val substInLmodule : int -> con list -> int -> lmodule -> lmodule
end
