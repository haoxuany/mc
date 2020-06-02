
structure CpsConstructorTranslation = struct
  structure S = FOmegaS.Abt
  structure T = Cps.Abt

  (* Note, this translation is fundamentally different from
  * [Morrisett, Walker, Crary, Glew 1999], by doing a direct
  * translation of everything, except for:
  * - Lambda functions gets translated to continuation passing style
  * (t -> t' removed, (not t) aka t -> 0 remains)
  * - Polymorphic lambdas gets translated to continuation passing style
  * (forall a : k. t removed)
  *)
  fun translateKind (kind : S.kind) : T.kind =
    case kind of
      S.Kind_type => T.Kind_type
    | S.Kind_pi (k, k') => T.Kind_pi (translateKind k, translateKind k')
    | S.Kind_sigma (k, k') => T.Kind_sigma (translateKind k, translateKind k')
    | S.Kind_singleton c => T.Kind_singleton (translateCon c)
    | S.Kind_unit => T.Kind_unit

  and translateCon (con : S.con) : T.con =
    case con of
      S.Con_var i => T.Con_var i
    | S.Con_lam (k, c) => T.Con_lam (translateKind k, translateCon c)
    | S.Con_app (c, c') => T.Con_app (translateCon c, translateCon c')
    | S.Con_pair (c, c') => T.Con_pair (translateCon c, translateCon c')
    | S.Con_proj1 c => T.Con_proj1 (translateCon c)
    | S.Con_proj2 c => T.Con_proj2 (translateCon c)
    | S.Con_unit => T.Con_unit

    | S.Type_arrow (c, c') =>
        T.Type_not (T.Type_product [
          translateCon c, (* input *)
          T.Type_not (translateCon c'), (* continuation for the result *)
          T.Type_not T.Type_exn (* continuation if function throws exception *)
        ])
    | S.Type_forall (k, c) =>
        T.Type_not (T.Type_exists (
          translateKind k, (* input *)
          T.Type_product [
            T.Type_not (translateCon c), (* continuation for the result *)
            T.Type_not T.Type_exn (* continuation if expression throw exception *)
          ]
        ))
    | S.Type_exists (k, c) => T.Type_exists (translateKind k, translateCon c)
    | S.Type_product tys => T.Type_product (ParList.map translateCon tys)
    | S.Type_sum tys => T.Type_sum (ParList.map translateCon tys)
    | S.Type_rec c => T.Type_rec (translateCon c)
    | S.Type_exn => T.Type_exn
end
