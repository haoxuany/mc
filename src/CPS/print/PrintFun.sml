
functor PrintFun(
  structure Abt : ABT
  structure ExternalPrinter : EXTERNALPRINTER
) = struct
  open Abt
  val vp = Variable.print

  structure Printer = HeadPrinterFun(
    structure ExternalPrinter = ExternalPrinter
  )
  open Printer

  local
  fun sk kind =
    case kind of
      Kind_type => head "T" nil
    | Kind_pi (a, b) => head "Pi" [sk a, sk b]
    | Kind_sigma (a, b) => head "Sigma" [sk a, sk b]
    | Kind_singleton c => head "S" [sc c]
    | Kind_unit => head "1" nil

  and sc con =
   case con of
     Type_not c => head "not" [sc c]
   | Type_exists (k, c) => head "exists" [sk k, sc c]
   | Type_product cons => head "*" (ParList.map sc cons)
   | Type_sum cons => head "+" (ParList.map sc cons)
   | Type_rec con => head "rec" [sc con]
   | Type_exn => head "exn" nil
   | Con_var i => int i
   | Con_lam (k, c) => head "Lam" [sk k, sc c]
   | Con_app (c, c') => head "App" [sc c, sc c']
   | Con_pair (c, c') => head "Pair" [sc c, sc c']
   | Con_proj1 c => head "Pi1" [sc c]
   | Con_proj2 c => head "Pi2" [sc c]
   | Con_unit => head "<>" nil

  and sv value =
    case value of
      Value_var i => raw (vp i)
    | Value_lam (i, c, e) =>
        head "fn" [raw (vp i), sc c, se e]
    | Value_tuple vals =>
        head "*" (ParList.map sv vals)
    | Value_inj (c, i, v) =>
        head "inj" [sc c, int i, sv v]

  and se exp =
    case exp of
      Exp_app (v, v') => head "app" [sv v, sv v']
    | Exp_proj (v, i, x, e) =>
        head "proj" [sv v, int i, raw (vp x), se e]
    | Exp_case (v, exps) =>
        head "case" ((sv v) ::
          (ParList.map (fn (x, e) => list [raw (vp x), se e]) exps))
  in

  val serializeKind = sk
  val serializeCon = sc
  val serializeValue = sv
  val serializeExp = se

  val pp = fn f => fn a => print TextIO.stdOut (f a)

  val printKind = pp sk
  val printCon = pp sc
  val printValue = pp sv
  val printExp = pp se

  end

end
