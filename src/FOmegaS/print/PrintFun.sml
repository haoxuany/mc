
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

  and sc con =
   case con of
     Type_arrow (a, b) => head "->" [sc a, sc b]
   | Type_forall (k, c) => head "forall" [sk k, sc c]
   | Type_exists (k, c) => head "exists" [sk k, sc c]
   | Type_product cons => head "*" (ParList.map sc cons)
   | Type_sum cons => head "+" (ParList.map sc cons)
   | Type_exn => head "exn" nil
   | Con_var i => int i
   | Con_lam (k, c) => head "Lam" [sk k, sc c]
   | Con_app (c, c') => head "App" [sc c, sc c']
   | Con_pair (c, c') => head "Pair" [sc c, sc c']
   | Con_proj1 c => head "Pi1" [sc c]
   | Con_proj2 c => head "Pi2" [sc c]

  and st term =
    case term of
      Term_var i => raw (vp i)
    | Term_let (i, t, t') =>
        head "let" [raw (vp i), st t, st t']
    | Term_lam (i, c, t) =>
        head "fn" [raw (vp i), sc c, st t]
    | Term_app (t, t') => head "app" [st t, st t]
    | Term_polylam (k, t) => head "tfn" [sk k, st t]
    | Term_polyapp (t, c) => head "tapp" [st t, sc c]
    | Term_pack (c, t, c') => head "pack" [sc c, st t, sc c']
    | Term_unpack (x, t, t') => head "unpack" [raw (vp x), st t, st t']
    | Term_tuple terms => head "tuple" (ParList.map st terms)
    | Term_proj (t, i) => head "proj" [st t, int i]
    | Term_inj (c, i, t) => head "inj" [sc c, int i, st t]
    | Term_case (t, cases) => let
        val cases = ParList.map (fn (x, t) => list [raw (vp x), st t]) cases
      in head "case" ((st t) :: cases) end
  in

  val serializeKind = sk
  val serializeCon = sc
  val serializeTerm = st

  fun printKind k = print TextIO.stdOut (serializeKind k)
  fun printCon c = print TextIO.stdOut (serializeCon c)
  fun printTerm t = print TextIO.stdOut (serializeTerm t)

  end

end
