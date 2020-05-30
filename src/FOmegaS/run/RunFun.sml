
functor RunFun(
  structure Abt : ABT
  structure Subst : SUBST
  where type var = Abt.var
  where type kind = Abt.kind
  where type con = Abt.con
  where type term = Abt.term
) : RUN = struct
  open Abt
  open Subst

  (* Metatheory violation: doesn't typecheck, hence got stuck *)
  exception Stuck of term

  (* continuation to pass back to the caller if the term is a value *)
  exception TermVal
  fun step (fullterm as term) = let
    val term = case term of
      Term_var _ => raise TermVal
    | Term_let (term, var, term') =>
        ((Term_let (step term, var, term'))
        handle TermVal => substTerm term var term')
    | Term_fix (var, _, term) => substTerm fullterm var term
    | Term_lam _ => raise TermVal
    | Term_app (term, term') =>
        ((Term_app ((step term, term')))
        handle TermVal =>
          ((Term_app (term, (step term')))
          handle TermVal =>
          (* inversion tells us that term should be a lamda *)
            (case term of
               Term_lam (x, _, rest) =>
                 substTerm term' x rest
             | _ => raise Stuck fullterm)
          ))
    | Term_polylam _ => raise TermVal
    | Term_polyapp (term, con) =>
        ((Term_polyapp (step term, con))
          handle TermVal =>
          (* inversion tells us this should be a lambda *)
          (case term of
             Term_polylam (_, t) =>
               substConInTerm 0 [con] 0 t
           | _ => raise Stuck fullterm))
    | Term_pack (c, t, c') => Term_pack (c, step t, c')
    | Term_unpack (t, v, t') =>
        ((Term_unpack (step t, v, t'))
          handle TermVal =>
          (* inversion tell us this should be a pack *)
          (case t of
             Term_pack (c, t, c') =>
               substConInTerm 0 [c] 0 (substTerm t v t')
           | _ => raise Stuck fullterm))
    | Term_tuple terms => let
        fun eval_tuple l =
          case l of
            nil => raise TermVal
          | h :: t => ( ((step h) :: t) handle TermVal => h :: (eval_tuple t))
      in Term_tuple (eval_tuple terms) end
    | Term_proj (term, i) =>
        (Term_proj (step term, i)
          handle TermVal => (
            (* inversion tells us this term should be a tuple *)
            case term of
              Term_tuple terms => List.nth (terms, i)
            | _ => raise Stuck fullterm
          ))
    | Term_inj _ => raise TermVal
    | Term_case (term, terms) =>
        ((Term_case (step term, terms))
        handle TermVal =>
          (* inversion tells us this term should be a injection *)
          case term of
            Term_inj (_, i, term) => let
              val (x, rest) = List.nth (terms, i)
            in substTerm term x rest end
          | _ => raise Stuck fullterm
        )
    | Term_fold (con, term) => Term_fold (con, step term)
    | Term_unfold term =>
        ((Term_unfold (step term))
        handle TermVal =>
          (* inversion tells us this term should be a fold *)
          case term of
            Term_fold (_, term) => term
          | _ => raise Stuck fullterm
        )
  in term end

  fun run term = (run (step term)) handle TermVal => term
end
