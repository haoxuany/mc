
functor PrintFun(
  structure Abt : ABT
) : PRINT = struct
  open Abt
  val vp = Variable.print

  open HeadPrinter

  val name = Symbols.name

  fun u l = Byte.bytesToString (Word8Vector.fromList l)
  val downarrow = u [0wxe2, 0wx86, 0wx93]
  val bigpi = u [0wxce, 0wxa0]
  val bigsigma = u [0wxce, 0wxa3]
  val arrow = u [0wxe2, 0wx86, 0wx92]
  val times = u [0wxc2, 0wx97]
  val star = u [0wxe2, 0wx8b, 0wx86]
  val circle = u [0wxe2, 0wx83, 0wx9d]

  local
  fun sk kind =
    case kind of
      Kind_type => head "T" nil
    | Kind_pi (a, b) => head bigpi [sk a, sk b]
    | Kind_sigma (a, b) => head bigsigma [sk a, sk b]
    | Kind_singleton c => head "S" [sc c]
    | Kind_unit => head "1" nil

  and sc con =
   case con of
     Type_arrow (a, b) => head arrow [sc a, sc b]
   | Type_productfix tys => head times (List.concat
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

  and st term =
    case term of
      Term_var i => raw (vp i)
    | Term_let (t, x, t') =>
        head "let" [st t, raw (vp x), st t']
    | Term_fixlam lams =>
        head "fixlam"
          (ParList.map (fn (s, f, x, c, t, c') =>
          head "fn" [raw (name s), raw (vp f), raw (vp x), sc c, st t, sc c'])
          lams)
    | Term_pick (t, i) => head "pick" [st t, raw (name i)]
    | Term_app (t, t') => head "app" [st t, st t']
    | Term_polylam (k, t) => head "tfn" [sk k, st t]
    | Term_polyapp (t, c) => head "tapp" [st t, sc c]
    | Term_pack (c, t, c') => head "pack" [sc c, st t, sc c']
    | Term_unpack (t, x, t') => head "unpack" [st t, raw (vp x), st t']
    | Term_tuple terms => head "tuple" (ParList.map st terms)
    | Term_proj (t, i) => head "proj" [st t, int i]
    | Term_inj (c, i, t) => head "inj" [sc c, int i, st t]
    | Term_case (t, cases) => let
        val cases = ParList.map (fn (x, t) => list [raw (vp x), st t]) cases
      in head "case" ((st t) :: cases) end
    | Term_fold (c, t) => head "fold" [sc c, st t]
    | Term_unfold t => head "unfold" [st t]
    | Term_ext m => head "ext" [sm m]

  and ss sg =
    case sg of
      Sg_unit => head "1" nil
    | Sg_kind k => head "sgk" [sk k]
    | Sg_type t => head "sgt" [sc t]
    | Sg_lam (s, s') => head bigpi [ss s, ss s']
    | Sg_pair (s, s') => head bigsigma [ss s, ss s']
    | Sg_circ p => head circle [sp p]

  and sp psg =
    case psg of
      Psg_shift s => head "psgdown" [ss s]
    | Psg_exists (k, p) =>
        head "psgexists" [sk k, sp p]

  and sm module =
    case module of
      Module_var v => raw (vp v)
    | Module_unit => head star nil
    | Module_con c => head "mdc" [sc c]
    | Module_term t => head "mdt" [st t]
    | Module_lam (x, s, m) =>
        head "md->" [raw (vp x), ss s, sm m]
    | Module_app (m, m') =>
        head "mdapp" [sm m, sm m']
    | Module_pair (m, x, m') =>
        head "mdpair" [sm m, raw (vp x), sm m']
    | Module_tuple (m, m') =>
        head "md<>" [sm m, sm m']
    | Module_proj1 m =>
        head "mdpi1" [sm m]
    | Module_proj2 m =>
        head "mdpi1" [sm m]
    | Module_let (t, x, m) =>
        head "mdlet" [st t, raw (vp x), sm m]
    | Module_circ l => head circle [sl l]

  and sl lmodule =
    case lmodule of
      Lmodule_ret m => head "ret" [sm m]
    | Lmodule_seal (m, s) => head ":>" [sm m, ss s]
    | Lmodule_bind (m, x, l) =>
        head "bnd" [sm m, raw (vp x), sl l]
  in

  val serializeKind = sk
  val serializeCon = sc
  val serializeTerm = st
  val serializeSg = ss
  val serializePsg = sp
  val serializeModule = sm
  val serializeLmodule = sl

  val pp = fn f => fn x => print TextIO.stdOut (f x)

  val printKind = pp sk
  val printCon = pp sc
  val printTerm = pp st
  val printSg = pp ss
  val printPsg = pp sp
  val printModule = pp sm
  val printLmodule = pp sl

  end

end
