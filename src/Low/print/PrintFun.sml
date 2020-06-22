
functor PrintFun(
  structure Abt : ABT
) : PRINT = struct
  open Abt
  val vp = Variable.print

  open HeadPrinter

  val name = Symbols.name

  local
  fun sv value =
    case value of
      Value_var i => raw (vp i)
    | Value_pick (v, i) =>
        head "pick" [sv v, raw (name i)]
    | Value_tuple vals =>
        head "*" (ParList.map sv vals)
    | Value_inj (cases, i, v) =>
        head "inj" [int cases, int i, sv v]

  and se exp =
    case exp of
      Exp_app (v, v') => head "app" [sv v, list (ParList.map sv v')]
    | Exp_proj (v, i, x, e) =>
        head "proj" [sv v, int i, raw (vp x), se e]
    | Exp_case (v, exps) =>
        head "case" ((sv v) ::
          (ParList.map (fn (x, e) => list [raw (vp x), se e]) exps))
    | Exp_let (v, x, e) =>
        head "let" [sv v, raw (vp x), se e]
    | Exp_exit i => head "exit" [int i]

  and sb block =
    case block of
      Block_fixlam lams =>
        head "fix" (ParList.map (fn (s, f, xs, e) =>
          head "fn" [raw (name s), raw (vp f),
            list (List.concat
              (ParList.map (fn x => [raw (vp x)]) xs)),
            se e])
          lams)
    | Block_lam (xs, e) =>
        head "fn" [ list (List.concat
          (ParList.map (fn x => [raw (vp x)]) xs)),
          se e]

  and sp program =
    case program of
      Program (bnds, e) =>
        list ((ParList.map
          (fn (x, b) => head "bnd" [raw (vp x), sb b]) bnds) @ [se e])
  in

  val serializeValue = sv
  val serializeExp = se
  val serializeBlock = sb
  val serializeProgram = sp

  val pp = fn f => fn a => print TextIO.stdOut (f a)

  val printValue = pp sv
  val printExp = pp se
  val printBlock = pp sb
  val printProgram = pp sp

  end

end
