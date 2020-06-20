
structure HoistingConstructorTranslation = struct
  structure SLang = Cps
  structure TLang = BlockedCps
  structure S = SLang.Abt
  structure T = TLang.Abt
  open TLang.Subst

  exception TypeError

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

    | S.Type_not c => T.Type_not [translateCon c]
    | S.Type_productfix cs => T.Type_productfix (ParList.map translateCon cs)
    | S.Type_exists (k, c) => T.Type_exists (translateKind k, translateCon c)
    | S.Type_product tys => T.Type_product (ParList.map translateCon tys)
    | S.Type_sum tys => T.Type_sum (ParList.map translateCon tys)
    | S.Type_rec c => T.Type_rec (translateCon c)
    | S.Type_exn => T.Type_exn
end
