
structure Examples = struct
  structure Helpers = struct
    open Il
    open Abt
    open Print

    val newvar = Variable.new
    val fresh = Symbols.fresh
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
    fun printModuleCheck s m = let
      val () = print "Example: "
      val () = print s
      val () = print "\n============\n"
      val (a, s) = TypeCheck.sgSynth (Context.new ()) m
      val () = case a of
        NONE => print "Impure Module\n"
      | SOME c => (
          print "Pure Module with fst: ";
          printCon c;
          print "\n"
        )
      val () = print "with signature: "
      val () = printSg s
      val () = print "\n"
      val () = printModule m
      val () = print "\n\n"
    in () end
  end

  open Helpers
  infixr 4 $

  val example_id = let
    val fs = fresh "f"
    val f = newvar ()
    val x = newvar ()
  in
    Term_fixlam [
      (fs, f, x, Type_exn,
      Term_var x, Type_exn)
    ]
  end
  val () = printExample
    "Identity Function"
    example_id

  val example_poly = let
    val fs = fresh "f"
    val x = newvar ()
    val y = newvar ()
    val f = newvar ()
  in
    Term_let (
      Term_polylam (
        Kind_type,
        Term_fixlam
          [(fs, f, x, Con_var 0,
          Term_var x, Con_var 0)]),
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
    val fs = fresh "f"
    val f = newvar ()
    val y = newvar ()
    val a = newvar ()
    val b = newvar ()
  in
    Term_fixlam
      [(fs, f, y, bool,
      Term_case (Term_var y, [(a, ff), (b, tt)]), bool)]
  end
  val () = printExample
    "Boolean flip function"
    example_flip

  val fstrue = fresh "f"
  val example_istrue = let
    val f = newvar ()
    val y = newvar ()
    val a = newvar ()
    val b = newvar ()
  in
    Term_fixlam
      [(fstrue, f, y, bool,
      Term_case (Term_var y, [(a, tt), (b, ff)]), bool)]
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
          Type_productfix [(fstrue, Type_arrow (Con_var 0, bool))]
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
        (Term_pick (Term_proj (Term_var x, 1), fstrue),
        Term_proj (Term_var x, 0)))
  end
  val () = printExample
    "using boolean istrue function package"
    example_istrueunpack

  val example_fixpoint = let
    val fs = fresh "f"
    val f = newvar ()
    val y = newvar ()
    val a = newvar ()
    val b = newvar ()
    val temp = newvar ()
  in
    Term_let (
    Term_fixlam
      [(fs, f, y, bool,
        Term_case (Term_var y,
          [(a, tt), (b, Term_app (Term_var f, tt))]), bool)],
    temp,
    Term_app (Term_pick (Term_var temp, fs), ff)
    )
  end
  val () = printExample
    "Always true fixpoint"
    example_fixpoint

  val unitlistb = Type_sum [Type_product [], Con_var 0]
  val unitlist = Type_rec unitlistb

  val example_list = let
  in
    Term_fold (unitlistb,
      Term_inj (Type_sum [Type_product [], unitlist], 1,
        Term_fold (unitlistb,
          Term_inj (Type_sum [Type_product [], unitlist], 0, un))))
  end
  val () = printExample
    "length 2 unit list"
    example_list

  val mf = Module_term example_list
  val () = printModuleCheck
    "Module with list"
    mf

  val m = let
    val x = newvar ()
    val f = newvar ()
    val y = newvar ()
    val a = newvar ()
    val b = newvar ()
  in
    Module_pair (
      Module_con bool,
      x,
      Module_tuple (
        Module_term tt,
        Module_term (
          Term_fixlam
            [(fstrue, f, y, Con_var 0,
            Term_case (Term_var y, [(a, tt), (b, ff)]), Con_var 0)]
        )
      )
    )
  end
  val () = printModuleCheck
    "Boolean Module with istrue"
    m
end
