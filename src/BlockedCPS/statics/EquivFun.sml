
functor EquivFun(
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
) : EQUIV = struct
  open Abt
  open Context
  open Subst

  type context = Context.t

  exception TypeError
  exception NotAPath

  (* see Stone's PhD Thesis on what any of this is *)

  (* Singleton kind encodings for higher kinds: Figure 2.4 *)
  (* S(c :: k) := k' *)
  fun singleton c k =
    case k of
      Kind_type => Kind_singleton c
    | Kind_singleton _ => k
    | Kind_pi (k1, k2) =>
        Kind_pi (k1,
        singleton
        (* c gets put under a binder from k1, so lift it by 1 *)
        (Con_app (substInCon 0 [] 1 c, Con_var 0))
        k2)
    | Kind_sigma (k1, k2) =>
        Kind_sigma (
          singleton (Con_proj1 c) k1,
          (* everything gets lifted here, as a consequence of (pi2 c)
          * getting put under a new binder from k1, and [pi1 c/alpha]k2
          * no longer refers to variable in k1; aka this a nondependent product.
          *)
          substInKind 0 [] 1
          (singleton (Con_proj2 c) (substInKind 0 [Con_proj1 c] 0 k2))
        )
    | Kind_unit => k

  (* Kind validity: Figure 4.2 *)
  (* ctx |> k *)
  fun kindValid ctx k =
    case k of
      Kind_type => ()
    | Kind_singleton c => kindCheck ctx c Kind_type
    | Kind_pi (k', k'') =>
        (kindValid ctx k'; kindValid (extendKind ctx k') k'')
    | Kind_sigma (k', k'') =>
        (kindValid ctx k'; kindValid (extendKind ctx k') k'')
    | Kind_unit => ()

  (* Subkinding: Figure 4.2 *)
  (* ctx |> k <= k' *)
  and subkind ctx k1 k2 =
    case (k1, k2) of
      (Kind_type, Kind_type) => ()
    | (Kind_singleton _, Kind_type) => ()
    | (Kind_singleton c1, Kind_singleton c2) => conEquiv ctx c1 c2 Kind_type
    | (Kind_pi (k1, l1), Kind_pi (k2, l2)) => (
        subkind ctx k2 k1;
        subkind (extendKind ctx k2) l1 l2
      )
    | (Kind_sigma (k1, l1), Kind_sigma (k2, l2)) => (
        subkind ctx k1 k2;
        subkind (extendKind ctx k1) l1 l2
      )
    | (Kind_unit, Kind_unit) => ()
    | _ => raise TypeError

  (* Kind equivalence: Figure 4.2 *)
  (* ctx |> k <=> k' *)
  and kindEquiv ctx k k' =
    case (k, k') of
      (Kind_type, Kind_type) => ()
    | (Kind_singleton c, Kind_singleton c') =>
        conEquiv ctx c c' Kind_type
    | (Kind_pi (k1, l1), Kind_pi (k2, l2)) =>
        ( kindEquiv ctx k1 k2; kindEquiv (extendKind ctx k1) l1 l2 )
    | (Kind_sigma (k1, l1), Kind_sigma (k2, l2)) =>
        ( kindEquiv ctx k1 k2; kindEquiv (extendKind ctx k1) l1 l2 )
    | (Kind_unit, Kind_unit) => ()
    | _ => raise TypeError

  (* Kind synthesis: Figure 4.3 *)
  (* ctx |> c --> k *)
  and kindSynth ctx (allc as c) =
    case c of
      Con_var i => singleton c (lookupKind ctx i)
    | Con_lam (k', c) =>
        (kindValid ctx k'; Kind_pi (k', kindSynth (extendKind ctx k') c))
    | Con_app (c, c') =>
        (case kindSynth ctx c of
           Kind_pi (k', k'') => (
             kindCheck ctx c' k';
             substInKind 0 [c'] 0 k''
           )
        | _ => raise TypeError)
    | Con_pair (c, c') =>
        Kind_sigma (
          kindSynth ctx c,
          (* this is a nondependent product, so needs to be lifted *)
          substInKind 0 [] 1 (kindSynth ctx c'))
    | Con_proj1 c =>
        (case kindSynth ctx c of
          Kind_sigma (k', _) => k'
        | _ => raise TypeError)
    | Con_proj2 c =>
        (case kindSynth ctx c of
           Kind_sigma (_, k'') => substInKind 0 [Con_proj1 c] 0 k''
         | _ => raise TypeError)
    | Con_unit => Kind_unit
   (* all the annoying base cases that just return the singleton of itself *)
   (* we also make an extra check for constructor validity, because the type
   * can be technically malformed *)
   | Type_not cs =>
       (ParList.map (fn c => kindCheck ctx c Kind_type) cs;
       Kind_singleton allc)
   | Type_productfix tys =>
       (ParList.map (fn (_, c) => kindCheck ctx c Kind_type) tys;
       Kind_singleton allc)
   | Type_forall (k, c) =>
       (kindValid ctx k; kindCheck (extendKind ctx k) c Kind_type;
       Kind_singleton allc)
   | Type_exists (k, c) =>
       (kindValid ctx k; kindCheck (extendKind ctx k) c Kind_type;
       Kind_singleton allc)
   | Type_product tys =>
       (ParList.map (fn c => kindCheck ctx c Kind_type) tys;
       Kind_singleton allc)
   | Type_sum tys =>
       (ParList.map (fn c => kindCheck ctx c Kind_type) tys;
       Kind_singleton allc)
   | Type_rec ty =>
       (kindCheck (extendKind ctx Kind_type) ty;
       Kind_singleton allc)
   | Type_exn => Kind_singleton allc

  (* Kind checking: Figure 4.3 *)
  (* ctx |> c <-- k *)
  and kindCheck ctx c k = subkind ctx (kindSynth ctx c) k

  (* Kind extraction (of a path): Figure 4.4 *)
  (* ctx |> c ^ k *)
  and kindExtract ctx c =
    case c of
      Con_var i => lookupKind ctx i
    | Con_proj1 p =>
        (case kindExtract ctx p of
          Kind_sigma (k', _) => k'
        | _ => raise TypeError)
    | Con_proj2 p =>
        (case kindExtract ctx p of
          Kind_sigma (_, k'') =>
            substInKind 0 [Con_proj1 p] 0 k''
        | _ => raise TypeError)
    | Con_app (p, c) =>
        (case kindExtract ctx p of
           Kind_pi (_, k'') =>
             substInKind 0 [c] 0 k''
        | _ => raise TypeError)
    | Con_unit => Kind_unit
    (* constant constructor cases (aka types) *)
    | Type_not _ => Kind_type
    | Type_productfix _ => Kind_type
    | Type_forall _ => Kind_type
    | Type_exists _ => Kind_type
    | Type_product _ => Kind_type
    | Type_sum _ => Kind_type
    | Type_rec _ => Kind_type
    | Type_exn => Kind_type

    | Con_lam _ => raise NotAPath
    | Con_pair _ => raise NotAPath

  (* Weak head reduction + normalization: Figure 4.4 *)
  (* ctx |> c -> c' *)
  and weakHeadNormalize ctx c = let

    (* cases 1 ~ 3 in paper, case 4 is handled later *)
    (* split out because this is easier to do recursively
    * within the evaluation context *)
    fun reduce c =
      case c of
        Con_app (c1, c2) => let
          val c1 = reduce c1
        in
          case c1 of
            Con_lam (_, cbody) => reduce (substInCon 0 [c2] 0 cbody)
          | _ => Con_app (c1, c2)
        end
      | Con_proj1 c => let
          val c = reduce c
        in
          case c of
            Con_pair (c1, _) => reduce c1
          | _ => Con_proj1 c
        end
      | Con_proj2 c => let
          val c = reduce c
        in
          case c of
            Con_pair (_, c2) => reduce c2
          | _ => Con_proj2 c
        end
      (* assume everything else are just types rather than constructors *)
      (* which are the constant cases of a path *)
      (* also happens to align with empty con of empty kind, since it
      * can't be normalized further *)
      | _ => c

    val c = reduce c
  in
    case kindExtract ctx c of
      (* case 4 *)
      Kind_singleton c => weakHeadNormalize ctx c
    | _ => c
  end

  (* Algorithmic constructor equivalence: Figure 4.4 *)
  (* ctx |> c <=> c' :: k *)
  and conEquiv ctx c c' k =
    case k of
      Kind_type =>
        (case pathEquiv ctx
          (weakHeadNormalize ctx c)
          (weakHeadNormalize ctx c') of
           Kind_type => ()
        | _ => raise TypeError)
    | Kind_singleton _ => ()
    | Kind_pi (k', k'') =>
        conEquiv (extendKind ctx k')
        (* extension of kind context requires lifting this by one *)
        (Con_app (substInCon 0 [] 1 c, Con_var 0))
        (Con_app (substInCon 0 [] 1 c', Con_var 0))
        k''
    | Kind_sigma (k', k'') =>
        (
          conEquiv ctx (Con_proj1 c) (Con_proj2 c') k';
          conEquiv ctx (Con_proj2 c) (Con_proj2 c')
          (substInKind 0 [Con_proj1 c] 0 k'')
        )
    | Kind_unit => ()

  (* Algorithmic path equivalence: Figure 4.4 *)
  (* ctx |> p <-> p ^ k *)
  and pathEquiv ctx p p' =
    case (p, p') of
      (Con_var i, Con_var j) =>
        if i = j then lookupKind ctx i
        else raise TypeError
    | (Con_app (p1, c1), Con_app (p2, c2)) =>
        (case pathEquiv ctx p1 p2 of
           Kind_pi (k', k'') => (
             conEquiv ctx c1 c2 k';
             substInKind 0 [c1] 0 k''
           )
        | _ => raise TypeError)
    | (Con_proj1 p1, Con_proj1 p2) =>
        (case pathEquiv ctx p1 p2 of
           Kind_sigma (k', _) => k'
        | _ => raise TypeError)
    | (Con_proj2 p1, Con_proj2 p2) =>
        (case pathEquiv ctx p1 p2 of
           Kind_sigma (_, k'') => substInKind 0 [Con_proj1 p1] 0 k''
        | _ => raise TypeError)
    (* this should never get called: constructor equivalence will just work,
    * nevertheless adding this anyway *)
    | (Con_unit, Con_unit) => Kind_unit
    (* the rest are the annoying and unilluminating constant cases *)
    | (Type_not cs, Type_not cs') =>
        (ParList.map (fn (c, c') => conEquiv ctx c c' Kind_type)
          (ListPair.zip (cs, cs'));
        Kind_type)
    | (Type_productfix tys, Type_productfix tys') =>
        (ParList.map (fn ((sym, ty), (sym', ty')) =>
          if Symbols.eq (sym, sym') then conEquiv ctx ty ty' Kind_type
          else raise TypeError)
          (ListPair.zip (tys, tys'));
        Kind_type)
    | (Type_forall (k1, c1), Type_forall (k2, c2)) =>
        (kindEquiv ctx k1 k2; conEquiv (extendKind ctx k1) c1 c2 Kind_type;
        Kind_type)
    | (Type_exists (k1, c1), Type_exists (k2, c2)) =>
        (kindEquiv ctx k1 k2; conEquiv (extendKind ctx k1) c1 c2 Kind_type;
        Kind_type)
    | (Type_product tys, Type_product tys') =>
        (* TODO: a parallel version of listpair would be nice *)
        (ListPair.appEq
        (fn (ty1, ty2) => conEquiv ctx ty1 ty2 Kind_type)
        (tys, tys');
        Kind_type)
    | (Type_sum tys, Type_sum tys') =>
        (* TODO: a parallel version of listpair would be nice *)
        (ListPair.appEq
        (fn (ty1, ty2) => conEquiv ctx ty1 ty2 Kind_type)
        (tys, tys');
        Kind_type)
    | (Type_rec ty, Type_rec ty') =>
        (conEquiv (extendKind ctx Kind_type) ty ty' Kind_type;
        Kind_type)
    | (Type_exn, Type_exn) => Kind_type
    | _ => raise TypeError
end
