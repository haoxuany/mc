
functor TypeCheckFun(
  structure Abt : ABT

  structure Context : CONTEXT
  (* nooo SML/NJ doesn't allow a type sharing spec here for whatever magical reason *)
  where type var = Abt.var
  where type kind = Abt.kind
  where type con = Abt.con
  where type value = Abt.value
  where type exp = Abt.exp

  structure Subst : SUBST
  where type var = Abt.var
  where type kind = Abt.kind
  where type con = Abt.con
  where type value = Abt.value
  where type exp = Abt.exp

  structure Equiv : EQUIV
  where type context = Context.t
  where type var = Abt.var
  where type kind = Abt.kind
  where type con = Abt.con
  where type value = Abt.value
  where type exp = Abt.exp
) : TYPECHECK = struct
  open Abt
  open Context
  open Subst
  open Equiv

  (* Bidirectional type checking *)
  (* I can't find a really good paper to link here because most of
   * them are working with different type systems, but
   * Frank Pfenning's notes on verifications is probably good enough.
   * The only difference here is that when I need to verify some
   * subexpression is of a specific type, I need to weak head normalize it,
   * otherwise we might get redexes.
   *)
  (* ctx |> v --> c *)
  fun typeValueSynth ctx value =
    case value of
      Value_var v => lookupType ctx v
    | Value_lam (v, t, e) =>
        (kindCheck ctx t Kind_type;
        typeExpCheck (extendType ctx v t) e;
        Type_not t)

  (* ctx |> v <-- c *)
  and typeValueCheck ctx value con =
    (conEquiv ctx con (typeValueSynth ctx value) Kind_type) : unit

  and typeExpCheck ctx exp =
    case exp of
      Exp_app (v, v') =>
        (case weakHeadNormalize ctx (typeValueSynth ctx v) of
           Type_not c => typeValueCheck ctx v c
         | _ => raise TypeError)
end
