
structure Examples = struct
  structure Helpers = struct
    open FOmegaS
    open Abt
    structure Print = PrintFun(
      structure Abt = Abt
      structure ExternalPrinter = struct
        type t = unit
        fun print _ _ = ()
      end
    )
    open Print

    val newvar = Variable.new
    val $ = fn (a, b) => a b
    val print = TextIO.print

    fun runTrace p = let
      val () = print "Term: "
      val () = printTerm p
      val () = print "\n"
      val () = print "Type: "
      val () = printCon (TypeCheck.typeSynth (Context.new ()) p)
      val () = print "\n"
      val next = (SOME (Run.step p)) handle Run.TermVal => NONE
      val () = case next of
        NONE => print "value\n"
      | SOME next => (print "step -------\n"; runTrace next)
    in () end
    fun printExample s p = let
      val () = print "Example: "
      val () = print s
      val () = print "\n============\n"
      val () = runTrace p
      val () = print "\n\n"
    in () end
  end

  open Helpers
  infixr 4 $

  val example_id = let
    val x = newvar ()
  in
    Term_lam (x,
      Type_arrow (Type_exn, Type_exn),
      Term_var x)
  end
  val () = printExample
    "Identity Function"
    example_id

  val example_poly = let
    val x = newvar ()
    val y = newvar ()
  in
    Term_let (
      Term_polylam (
        Kind_type,
        Term_lam (x,
          Type_arrow (Con_var 0, Con_var 0),
          Term_var x)
        ),
    y,
    Term_polyapp (Term_var y, Type_exn))
  end
  val () = printExample
    "Polymorphic identity function application"
    example_poly

  val un = Term_tuple []
  val bool = Type_sum [Type_product [], Type_product []];
  val tt = Term_inj (bool, 0, un)
  val ff = Term_inj (bool, 1, un)

  val example_flip = let
    val y = newvar ()
    val a = newvar ()
    val b = newvar ()
  in
    Term_lam (y, bool, Term_case (Term_var y, [(a, ff), (b, tt)]))
  end
  val () = printExample
    "Boolean flip function"
    example_flip

  val example_istrue = let
    val y = newvar ()
    val a = newvar ()
    val b = newvar ()
  in
    Term_lam (y, bool, Term_case (Term_var y, [(a, tt), (b, ff)]))
  end
  val () = printExample
    "Boolean istrue function"
    example_istrue

  val example_istruepack =
    Term_pack (
      bool,
      Term_tuple [tt, example_istrue],
      Type_exists (Kind_type,
        Type_product [
          Con_var 0,
          Type_arrow (Con_var 0, bool)
        ])
    )
  val () = printExample
    "Existential boolean istrue function with introduction"
    example_istruepack

  val example_istrueunpack = let
    val x = newvar ()
  in
    Term_unpack (example_istruepack,
      x,
      Term_app
        (Term_proj (Term_var x, 1),
        Term_proj (Term_var x, 0)))
  end
  val () = printExample
    "using boolean istrue function package"
    example_istrueunpack

  val example_fixpoint = let
    val x = newvar ()
    val y = newvar ()
    val a = newvar ()
    val b = newvar ()
    val temp = newvar ()
  in
    Term_let (
    Term_fix (x, Type_arrow (bool, bool),
      Term_lam (y, bool,
        Term_case (Term_var y, [(a, tt), (b, Term_app (Term_var x, tt))]))),
    temp,
    Term_app (Term_var temp, ff)
    )
  end
  val () = printExample
    "Always true fixpoint"
    example_fixpoint
end
