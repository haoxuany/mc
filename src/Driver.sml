
structure Driver = struct
  structure FLang = FOmegaS
  structure F = FLang.Abt
  structure CLang = Cps
  structure C = CLang.Abt
  structure BLang = BlockedCps
  structure B = BLang.Abt
  structure LLang = Low
  structure L = LLang.Abt
  structure CCLang = CSub
  structure CC = CSub.Abt

  local
   open F
  in
    val newvar = Variable.new
    val fresh = Symbols.fresh

    val example_unit = Term_tuple []

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

    val example_unitapp = let
      val fs = fresh "f"
      val f = newvar ()
      val x = newvar ()
    in
      Term_app (
        Term_pick (
          Term_fixlam [
            (fs, f, x, Type_product nil,
            Term_var x, Type_product nil)
          ],
          fs
        ),
        Term_tuple nil
      )
    end

    val example_divergefix = let
      val fs = fresh "f"
      val f = newvar ()
      val x = newvar ()
    in
      Term_app (
        Term_pick (
          Term_fixlam [
            (fs, f, x, Type_product nil,
            Term_app (Term_var f, Term_var x), Type_product nil)
          ],
          fs
        ),
        Term_tuple nil
      )
    end

    val bool = Type_sum [Type_product [], Type_product []];
    val un = Term_tuple []
    val tt = Term_inj (bool, 0, un)
    val ff = Term_inj (bool, 1, un)

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

    val example_istrueunpack = let
      val x = newvar ()
    in
      Term_unpack (example_istruepack,
        x,
        Term_app
          (Term_pick (Term_proj (Term_var x, 1), fstrue),
          Term_proj (Term_var x, 0)))
    end

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
  end

  val source = example_divergefix

  local
    open CpsConversion
    open C
  in

  val () = TextIO.print "fomega:\n"
  val () = FLang.Print.printTerm source
  val () = TextIO.print "\n\n\n"

  val k = newvar ()
  val kexn = newvar ()
  val (result, tau, tau') = translateTerm
    (DebugTranslation.emptyCtx ())
    source
    k
    kexn

  val () = FLang.TypeCheck.typeCheck (FLang.Context.new ()) source tau

  val x = newvar ()
  val y = newvar ()
  val program = Exp_let (
    Value_lam ([(x, tau')], Exp_exit 0),
    k,
    Exp_let (
      Value_lam ([(y, Type_exn)], Exp_exit 1),
      kexn,
      result
    )
  )
  val () = CLang.TypeCheck.typeExpCheck (CLang.Context.new ()) program

  val () = TextIO.print "cps:\n"
  val () = CLang.Print.printExp program
  val () = TextIO.print "\n\n\n"

  val (program, i) = CLang.Optimize.varRedexRemovalExp program
  val () = CLang.TypeCheck.typeExpCheck (CLang.Context.new ()) program

  val () = TextIO.print "redex removal:\n"
  val () = CLang.Print.printExp program
  val () = TextIO.print "\n"
  val () = TextIO.print
    (String.concat ["optimizations done: ", Int.toString i])
  val () = TextIO.print "\n\n\n"

  val (program, i) = CLang.Optimize.varRedexRemovalExp program
  val () = CLang.TypeCheck.typeExpCheck (CLang.Context.new ()) program

  val () = TextIO.print "redex removal:\n"
  val () = CLang.Print.printExp program
  val () = TextIO.print "\n"
  val () = TextIO.print
    (String.concat ["optimizations done: ", Int.toString i])
  val () = TextIO.print "\n\n\n"
  end

  local
    open ClosureConversion
    open C
  in
  val program = translateExp
    (DebugTranslation.emptyCtx ())
    program
  val () = CLang.TypeCheck.typeExpCheck (CLang.Context.new ()) program

  val () = TextIO.print "closure:\n"
  val () = CLang.Print.printExp program
  val () = TextIO.print "\n\n\n"
  end

  local
    open Hoisting
    open B
  in
  val program = translateProgram program

  val () = TextIO.print "hoist:\n"
  val () = BLang.Print.printProgram program
  val () = TextIO.print "\n\n\n"

  val () = BLang.TypeCheck.typeProgramCheck (BLang.Context.new ()) program
  end

  local
    open Erasure
    open L
  in
  val program = translateProgram program

  val () = TextIO.print "erasure:\n"
  val () = LLang.Print.printProgram program
  val () = TextIO.print "\n\n\n"
  end

  local
    open Codegen
    open CC
  in
  val program = translateProgram program

  val file = TextIO.openOut "test.c"
  val () = CCLang.Print.printCFile file program
  val () = TextIO.closeOut file
  end
end
