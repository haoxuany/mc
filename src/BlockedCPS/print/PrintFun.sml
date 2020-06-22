
functor PrintFun(
  structure Abt : ABT
) : PRINT = struct
  open Abt
  val vp = Variable.print

  open HeadPrinter

  val name = Symbols.name

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
     Type_not c => head "not" (ParList.map sc c)
   | Type_productfix tys => head "*" (List.concat
       (ParList.map (fn (s, c) => [raw (name s), sc c]) tys))
   | Type_forall (k, c) => head "forall" [sk k, sc c]
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
    | Value_pick (v, i) =>
        head "pick" [sv v, raw (name i)]
    | Value_pack (c, v, c') =>
        head "pack" [sc c, sv v, sc c']
    | Value_polyapp (v, c) =>
        head "polyapp" [sv v, sc c]
    | Value_tuple vals =>
        head "*" (ParList.map sv vals)
    | Value_inj (c, i, v) =>
        head "inj" [sc c, int i, sv v]
    | Value_fold (c, v) =>
        head "fold" [sc c, sv v]

  and se exp =
    case exp of
      Exp_app (v, v') => head "app" [sv v, list (ParList.map sv v')]
    | Exp_unpack (v, x, e) =>
        head "unpack" [sv v, raw (vp x), se e]
    | Exp_proj (v, i, x, e) =>
        head "proj" [sv v, int i, raw (vp x), se e]
    | Exp_case (v, exps) =>
        head "case" ((sv v) ::
          (ParList.map (fn (x, e) => list [raw (vp x), se e]) exps))
    | Exp_unfold (v, x, e) =>
        head "unfold" [sv v, raw (vp x), se e]
    | Exp_let (v, x, e) =>
        head "let" [sv v, raw (vp x), se e]
    | Exp_exit i => head "exit" [int i]

  and sb block =
    case block of
      Block_fixlam lams =>
        head "fix" (ParList.map (fn (s, f, bnds, e) =>
          head "fn" [raw (name s), raw (vp f),
            list (List.concat
              (ParList.map (fn (x, c) => [raw (vp x), sc c]) bnds)),
            se e])
          lams)
    | Block_lam (bnds, e) =>
        head "fn" [ list (List.concat
          (ParList.map (fn (x, c) => [raw (vp x), sc c]) bnds)),
          se e]
    | Block_polylam (k, b) => head "plam" [sk k, sb b]

  and sp program =
    case program of
      Program (bnds, e) =>
        list ((ParList.map
          (fn (x, b) => head "bnd" [raw (vp x), sb b]) bnds) @ [se e])
  in

  val serializeKind = sk
  val serializeCon = sc
  val serializeValue = sv
  val serializeExp = se
  val serializeBlock = sb
  val serializeProgram = sp

  val pp = fn f => fn a => print TextIO.stdOut (f a)

  val printKind = pp sk
  val printCon = pp sc
  val printValue = pp sv
  val printExp = pp se
  val printBlock = pp sb
  val printProgram = pp sp

  end

end
