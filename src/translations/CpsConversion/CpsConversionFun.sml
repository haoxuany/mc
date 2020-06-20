
functor CpsConversionFun(
  structure DebugTranslation : DEBUGTRANSLATION
) = struct
  structure SLang = FOmegaS
  structure TLang = Cps
  structure S = SLang.Abt
  structure T = TLang.Abt

  open CpsConstructorTranslation
  structure DebugTranslation = DebugTranslation
  open DebugTranslation

  val new = Variable.new
  exception TypeError


  (* Type directed (and preserving) translation *)
  (* ctx |- term : tau -->
  * (translateCtx ctx), k : not (translateCon tau), kexn : not exn |- exp *)
  (* translateTerm +ctx +term +k +kexn -exp -tau -(translateCon tau) *)
  fun translateTerm (ctx : ctx) ((fullterm as term) : S.term)
    (k : Variable.t) (kexn : Variable.t) = let

    val debug = fn ((result, tau, tau'), name) => let
      val () = debug ctx fullterm tau k kexn result tau' name
    in (result, tau, tau') end
    infix 9 debug

    val return =
    case term of

      S.Term_var x => let
        val tau = lookupType ctx x
        val result = T.Exp_app (T.Value_var k, [T.Value_var x])
        val tau' = translateCon tau
      in (result, tau, tau') end
      debug "var"

    | S.Term_let (e, x, e') => let
        (* assume e : t -> u *)
        (* e' : t' -> u' *)
        val k' = new () (* : not u *)
        val (f, t, u) = translateTerm ctx e k' kexn

        val (f', t', u') = translateTerm (extendType ctx x t) e' k kexn
        (* at this point k : not u', since we want to pass the result of
        * e' to the next computation, we can just use k for this. *)

        val result =
          T.Exp_let (
            T.Value_lam (
              [(x, u)],
              f'
            ),
            k', (* not u *)
            f
          )
        val tau = t'
        val tau' = u'
      in (result, tau, tau') end
      debug "let"

    | S.Term_fixlam lams => let
        val ctx = List.foldl
          (fn ((f, x, c, e, c'), ctx) => extendType ctx f (S.Type_arrow (c, c')))
          ctx
          lams

        val fixlam = ParList.map
          (fn (f, x, t, e, t') => let
            val u = translateCon t
            val k' = new () (* : not u' *)
            val kexn' = new ()
            val (e', _, u') = translateTerm (extendType ctx x t) e k' kexn'

            val y = [x, k', kexn']
            val ytau = [u, T.Type_not [u'], T.Type_not [T.Type_exn]]

            val tau = S.Type_arrow (t, t')
            val tau' = T.Type_not ytau
          in (f, (ListPair.zip (y, ytau)), e', tau, tau') end)
          lams

        val (result, tau, tau') = List.foldr
          (fn ((f, bnds, e, tau, tau'), (lam, taus, tau's)) =>
            ((f, bnds, e) :: lam, tau :: taus, tau' :: tau's))
          (nil, nil, nil)
          fixlam

        val result = T.Exp_app (T.Value_var k, [T.Value_fixlam result])
        val tau = S.Type_productfix tau
        val tau' = T.Type_productfix tau'
      in (result, tau, tau') end
      debug "fixlam"

    | S.Term_pick (e, i) => let
        val k' = new () (* : not productfix [...] *)
        val (e', t, u) = translateTerm ctx e k' kexn

        val x = new () (* : productfix [...] *)
        val result = T.Exp_let (
          T.Value_lam (
            [(x, u)],
            T.Exp_app (T.Value_var k, [T.Value_pick (T.Value_var x, i)])
          ),
          k',
          e'
        )
        val tau = case weakHeadNormalize ctx t of
          S.Type_productfix c => List.nth (c, i)
        | _ => raise TypeError
        val tau' = translateCon tau
      in (result, tau, tau') end
      debug "pick"

    | S.Term_app (e1, e2) => let
        (* assume e1 : t1 -> t2, e2 : t1 *)
        (* t1 -> t2 ==> not (u1 * not u2 * not exn) *)
        (* t1 ==> u1 *)
        (* thus e1 e2 : t2 ==> u2 *)
        val k1 = new () (* : not (not (u1 * not u2 * not exn)) *)
        val (e1', t1arrowt2, u1') = translateTerm ctx e1 k1 kexn
        (* we use kexn directly, since by dynamics, if
        * e1 throws to some kexn1, then the whole expression throws to
        * kexn *)

        val k2 = new () (* : not u1 *)
        val (e2', t1, u1) = translateTerm ctx e2 k2 kexn
        (* we use kexn directly, since by dynamics, if
        * e1 throws to some kexn1, then the whole expression throws to
        * kexn, otherwise it passes to k1, which if e2 throws to some kexn2,
        * then it should throw to kexn. *)

        val x = new () (* : u1' *)
        val y = new () (* : u1 *)
        val result = T.Exp_let (
          T.Value_lam (
            [(x, u1')], (* not (u1 * not u2 * not exn) *)
            T.Exp_let (
              T.Value_lam (
                [(y, u1)], (* u1 *)
                T.Exp_app (
                  T.Value_var x,
                  [
                    T.Value_var y,
                    T.Value_var k,
                    T.Value_var kexn
                  ]
                )
              ),
              k2, (* not u1 *)
              e2'
            )
          ),
          k1, (* : not (not (u1 * not u2 * not exn)) *)
          e1'
        )
        val tau = case weakHeadNormalize ctx t1arrowt2 of
          S.Type_arrow (_, t2) => t2
        | _ => raise TypeError
        val tau' = translateCon tau
      in (result, tau, tau') end
      debug "app"

    | S.Term_polylam (kind, e) => let
        (* assume (Lam a : kind. e) :
        *  (forall a : kind. t) => not (exists a : kind. not u * not exn)
        *)
        val k' = new () (* : not u (given a : kind) *)
        val kexn' = new ()
        val (e', t, u) = translateTerm (extendKind ctx kind) e k' kexn'

        val tau = S.Type_forall (kind, t)

        val x = new () (* : exists a : kind. not u * not exn *)
        val xty = T.Type_exists
          (translateKind kind,
          T.Type_product [T.Type_not [u], T.Type_not [T.Type_exn]])
        val y = new () (* : not u * not exn given a : kind *)

        val result = T.Exp_app (
          T.Value_var k, (* : not not (exists a : kind. not u * not exn) *)
          [T.Value_lam (
            [(x, xty)],
            T.Exp_unpack (
              T.Value_var x,
              y, (* : not u * not exn given a : kind *)
              T.Exp_proj (
                T.Value_var y,
                0,
                k', (* : not u given a : kind *)
                T.Exp_proj (
                  T.Value_var y,
                  1,
                  kexn',
                  e'
                )
              )
            )
          )]
        )
        val tau' = T.Type_not [xty]
      in (result, tau, tau') end
      debug "polylam"

    | S.Term_polyapp (e, c) => let
        (* assume e :
        * forall a : kind. t => not (exists a : kind. not u * not exn) *)
        val k' = new () (* : not not (exists a : kind. not u * not exn) *)
        val (e', t, u') = translateTerm ctx e k' kexn

        val (kind, t) = case weakHeadNormalize ctx t of
          S.Type_forall (kind, t) => (kind, t)
        | _ => raise TypeError

        val x = new () (* : not (exists a : kind. not u * not exn) *)
        val result = T.Exp_let (
          T.Value_lam (
            [(x, u')],
            T.Exp_app (
              T.Value_var x,
              [T.Value_pack (translateCon c,
                T.Value_tuple [T.Value_var k, T.Value_var kexn],
                T.Type_exists (translateKind kind,
                  T.Type_product [
                    T.Type_not [translateCon t],
                    T.Type_not [T.Type_exn]
                  ]
                )
              )]
            )
          ),
          k', (* : not not (exists a : kind. not u * not exn) *)
          e'
        )
        val tau = SLang.Subst.substInCon 0 [c] 0 t
        val tau' = translateCon tau
      in (result, tau, tau') end
      debug "polyapp"

    | S.Term_pack (c, e, cexists) => let
        (* assume e : t => u *)
        val tau = cexists
        val tau' = translateCon tau

        val k' = new () (* : not u *)
        val (e', t, u) = translateTerm ctx e k' kexn

        val x = new () (* : u *)
        val result = T.Exp_let (
          T.Value_lam (
            [(x, u)],
            T.Exp_app (
              T.Value_var k, (* : not tau' *)
              [T.Value_pack (
                translateCon c,
                T.Value_var x,
                tau'
              )]
            )
          ),
          k', (* : not u *)
          e'
        )
      in (result, tau, tau') end
      debug "pack"

    | S.Term_unpack (e, x, e') => let
        (* assume e : t => u *)
        (* e' : t' => u' given a : kind, x : t|u *)
        val k' = new () (* : not u *)
        val (f, t, u) = translateTerm ctx e k' kexn

        val (kind, t) = case weakHeadNormalize ctx t of
          S.Type_exists (kind, t) => (kind, t)
        | _ => raise TypeError

        val (f', t', u') = translateTerm
          (extendType (extendKind ctx kind) x t) e' k kexn

        val y = new () (* : u *)
        val result = T.Exp_let (
          T.Value_lam (
            [(y, u)],
            T.Exp_unpack (
              T.Value_var y,
              x,
              f'
            )
          ),
          k',
          f
        )
        val tau = SLang.Subst.substInCon 0 nil (~1) t'
        val tau' = TLang.Subst.substInCon 0 nil (~1) u'
      in (result, tau, tau') end
      debug "unpack"

    | S.Term_tuple es => let

        val translations = ParList.map (fn e =>
          let
            val ki = new () (* : not ui *)
            val (ei', ti, ui) = translateTerm ctx e ki kexn
          in (ei', ti, ui, ki) end
        ) es

        fun process translations bindings =
          case translations of
            nil => T.Exp_app (T.Value_var k, [T.Value_tuple (List.rev bindings)])
          | (ei', ti, ui, ki) :: rest =>
              let
                val xi = new () (* : ui *)
              in T.Exp_let (
                T.Value_lam (
                  [(xi, ui)],
                  process rest ((T.Value_var xi) :: bindings)
                ),
                ki, (* not ui *)
                ei'
              ) end

        val result = process translations nil
        val tau = S.Type_product
          (ParList.map (fn (_, ti, _, _) => ti)
          translations)
        val tau' = T.Type_product
          (ParList.map (fn (_, _, ui, _) => ui)
          translations)
      in (result, tau, tau') end
      debug "tuple"

    | S.Term_proj (e, i) => let

        val k' = new () (* : not u *)
        val (e', t, u) = translateTerm ctx e k' kexn

        val tau = case weakHeadNormalize ctx t of
          S.Type_product tys => List.nth (tys, i)
        | _ => raise TypeError
        val tau' = translateCon tau

        val x = new () (* : u *)
        val y = new () (* : tau' *)
        val result = T.Exp_let (
          T.Value_lam (
            [(x, u)],
            T.Exp_proj (
              T.Value_var x,
              i,
              y,
              T.Exp_app (T.Value_var k, [T.Value_var y])
            )
          ),
          k', (* not u *)
          e'
        )
      in (result, tau, tau') end
      debug "proj"

    | S.Term_inj (c, i, e) => let

        val k' = new () (* : not u *)
        val (e', t, u) = translateTerm ctx e k' kexn

        val tau = c
        val tau' = translateCon c

        val x = new () (* u *)
        val result = T.Exp_let (
          T.Value_lam (
            [(x, u)],
            T.Exp_app (
              T.Value_var k, (* not tau' *)
              [T.Value_inj (tau', i, T.Value_var x)]
            )
          ),
          k', (* : not u *)
          e'
        )
      in (result, tau, tau') end
      debug "inj"

    | S.Term_case (e, cases) => let

        val k' = new () (* : not u *)
        val (e', t, u) = translateTerm ctx e k' kexn

        val tys = case weakHeadNormalize ctx t of
          S.Type_sum tys => tys
        | _ => raise TypeError

        val translations = ParList.map
          (fn (ty, (x, e)) => let
            val (e', ti, ui) = translateTerm (extendType ctx x t) e k kexn
          in (x, ti, ui, e') end)
          (ListPair.zip (tys, cases))

        val x = new () (* : u *)
        val result = T.Exp_let (
          T.Value_lam (
            [(x, u)],
            T.Exp_case (
              T.Value_var x,
              ParList.map
              (fn (x, _, _, e) => (x, e))
              translations
            )
          ),
          k', (* not u *)
          e'
        )

        val (_, tau, tau', _) = List.nth (translations, 0)
      in (result, tau, tau') end
      debug "case"

    | S.Term_fold (t, e) => let
        (* assume fold e : t => u *)
        (* e : tunfolded => uunfolded *)
        val k' = new () (* : not uunfolded *)
        val (e', tunfolded, uunfolded) =
          translateTerm ctx e k' kexn

        val u = translateCon t
        val x = new () (* : uunfolded *)
        val result = T.Exp_let (
          T.Value_lam (
            [(x, uunfolded)],
            T.Exp_app (T.Value_var k, [T.Value_fold (u, T.Value_var x)])
          ),
          k', (* : not uunfolded *)
          e'
        )
        val tau = t
        val tau' = u
      in (result, tau, tau') end
      debug "fold"

    | S.Term_unfold e => let
        (* assume e : t => u *)
        (* unfold e : tunfolded => uunfolded *)
        val k' = new () (* : not u *)
        val (e', t, u) =
          translateTerm ctx e k' kexn

        val tunfolded = case weakHeadNormalize ctx t of
          (crec as (S.Type_rec c)) =>
            SLang.Subst.substInCon 0 [crec] 0 c
        | _ => raise TypeError
        val uunfolded = translateCon tunfolded

        val x = new () (* : u *)
        val y = new () (* : uunfolded *)
        val result = T.Exp_let (
          T.Value_lam (
            [(x, u)],
            T.Exp_unfold (
              T.Value_var x,
              y, (* : uunfolded *)
              T.Exp_app (T.Value_var k, [T.Value_var y])
            )
          ),
          k', (* : not u *)
          e'
        )
        val tau = tunfolded
        val tau' = uunfolded
      in (result, tau, tau') end
      debug "unfold"

    in return end
end
