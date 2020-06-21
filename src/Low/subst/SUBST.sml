
signature SUBST = sig
  type var
  type value
  type exp
  type block
  type program

  type vardict
  (* Simultaneous substitutions *)
  val varSubst : (value * var) list -> vardict

  val substInValue : vardict -> value -> value

  val substInExp : vardict -> exp -> exp

  val substInBlock : vardict -> block -> block

  val substInProgram : vardict -> program -> program
end
