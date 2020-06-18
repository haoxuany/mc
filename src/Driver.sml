
structure Driver = struct
  structure SLang = FOmegaS
  structure S = SLang.Abt
  structure TLang = Cps
  structure T = TLang.Abt

  local
   open S
  in
    val newvar = Variable.new

    val example_unit = Term_tuple []

    val example_id = let
      val f = newvar ()
      val x = newvar ()
    in
      Term_fixlam [
        (f, x, Type_exn,
        Term_var x, Type_exn)
      ]
    end

    val example_unitapp = let
      val f = newvar ()
      val x = newvar ()
    in
      Term_app (
        Term_pick (
          Term_fixlam [
            (f, x, Type_product nil,
            Term_var x, Type_product nil)
          ],
          0
        ),
        Term_tuple nil
      )
    end

    val example_divergefix = let
      val f = newvar ()
      val x = newvar ()
    in
      Term_app (
        Term_pick (
          Term_fixlam [
            (f, x, Type_product nil,
            Term_app (Term_var f, Term_var x), Type_product nil)
          ],
          0
        ),
        Term_tuple nil
      )
    end

    val bool = Type_sum [Type_product [], Type_product []];
    val un = Term_tuple []
    val tt = Term_inj (bool, 0, un)
    val ff = Term_inj (bool, 1, un)

    val example_istrue = let
      val f = newvar ()
      val y = newvar ()
      val a = newvar ()
      val b = newvar ()
    in
      Term_fixlam
        [(f, y, bool,
        Term_case (Term_var y, [(a, tt), (b, ff)]), bool)]
    end

    val example_istruepack =
      Term_pack (
        bool,
        Term_tuple [tt, example_istrue],
        Type_exists (Kind_type,
          Type_product [
            Con_var 0,
            Type_productfix [Type_arrow (Con_var 0, bool)]
          ])
      )

    val example_istrueunpack = let
      val x = newvar ()
    in
      Term_unpack (example_istruepack,
        x,
        Term_app
          (Term_pick (Term_proj (Term_var x, 1), 0),
          Term_proj (Term_var x, 0)))
    end

    val example_fixpoint = let
      val f = newvar ()
      val y = newvar ()
      val a = newvar ()
      val b = newvar ()
      val temp = newvar ()
    in
      Term_let (
      Term_fixlam
        [(f, y, bool,
          Term_case (Term_var y,
            [(a, tt), (b, Term_app (Term_var f, tt))]), bool)],
      temp,
      Term_app (Term_pick (Term_var temp, 0), ff)
      )
    end
  end

  val source = example_fixpoint

  local
    open CpsConversion
    open T
  in

  val () = TextIO.print "fomega:\n"
  val () = SLang.Print.printTerm source
  val () = TextIO.print "\n\n\n"

  val k = newvar ()
  val kexn = newvar ()
  val (result, tau, tau') = translateTerm
    (DebugTranslation.emptyCtx ())
    source
    k
    kexn

  val () = SLang.TypeCheck.typeCheck (SLang.Context.new ()) source tau

  val x = newvar ()
  val y = newvar ()
  val program = Exp_let (
    Value_lam (x, tau', Exp_exit 0),
    k,
    Exp_let (
      Value_lam (y, Type_exn, Exp_exit 1),
      kexn,
      result
    )
  )
  val () = TLang.TypeCheck.typeExpCheck (TLang.Context.new ()) program

  val () = TextIO.print "cps:\n"
  val () = TLang.Print.printExp program
  val () = TextIO.print "\n\n\n"
  end

  local
    open ClosureConversion
    open T
  in
  val program = translateExp
    (DebugTranslation.emptyCtx ())
    program
  val () = TLang.TypeCheck.typeExpCheck (TLang.Context.new ()) program

  val () = TextIO.print "closure:\n"
  val () = TLang.Print.printExp program
  val () = TextIO.print "\n\n\n"
  end
end
