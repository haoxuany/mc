
structure ClosureConversionConstructorTranslation = struct
  open Cps
  open Abt

  exception TypeError
  open Subst

  (* Note, we use the same source and destination language for this
  * translation, using the type translation briefly mentioned in
  * [Morrisett, Walker, Crary, Glew 1999]. Unlike the paper, we do
  * not go into type erasure just yet.
  * The only notable change here is the translation of functions, where
  * we inject an environment type into the function so that functions no longer
  * refers to free variables
  *)
  fun translateKind (kind : kind) : kind =
    case kind of
      Kind_type => Kind_type
    | Kind_pi (k, k') => Kind_pi (translateKind k, translateKind k')
    | Kind_sigma (k, k') => Kind_sigma (translateKind k, translateKind k')
    | Kind_singleton c => Kind_singleton (translateCon c)
    | Kind_unit => Kind_unit

  and translateCon (con : con) : con =
    case con of
      Con_var i => Con_var i
    | Con_lam (k, c) => Con_lam (translateKind k, translateCon c)
    | Con_app (c, c') => Con_app (translateCon c, translateCon c')
    | Con_pair (c, c') => Con_pair (translateCon c, translateCon c')
    | Con_proj1 c => Con_proj1 (translateCon c)
    | Con_proj2 c => Con_proj2 (translateCon c)
    | Con_unit => Con_unit
    | Type_not cs => Type_exists (
        Kind_type,
        Type_product [
          Con_var 0, (* environment *)
          Type_not (
            (ParList.map (fn c => substInCon 0 nil 1 (translateCon c)) cs)
            (* this need to be appended to make sure that previous projections work correctly *)
            @ [Con_var 0]
          )
        ]
      )
    (* Note that translation for fixed point product of lambdas and
    * standard lambdas are intentionally different. We know by construction
    * that all productfix by construction consists of tau -> 0s only. *)
    | Type_productfix cons => Type_exists (
        Kind_type,
        Type_product [
          Con_var 0, (* environment *)
          Type_productfix (ParList.map
            (fn (s, c) =>
              case c of
                Type_not cs => (s, Type_not (
                  (ParList.map (fn c => substInCon 0 nil 1 (translateCon c)) cs)
                  (* this need to be appended to make sure that previous projections work correctly *)
                  @ [Con_var 0]
                ))
              | _ => raise TypeError)
            cons)
        ]
      )
    | Type_exists (k, c) => Type_exists (translateKind k, translateCon c)
    | Type_product tys => Type_product (ParList.map translateCon tys)
    | Type_sum tys => Type_sum (ParList.map translateCon tys)
    | Type_rec c => Type_rec (translateCon c)
    | Type_exn => Type_exn
end
