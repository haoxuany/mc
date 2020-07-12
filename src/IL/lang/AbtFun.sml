
functor AbtFun(
  structure Variable : VARIABLE
) = struct
  structure Variable = Variable

  type var = Variable.t

  datatype kind =
    Kind_type
  | Kind_pi of kind * kind
  | Kind_sigma of kind * kind
  | Kind_singleton of con
  | Kind_unit

  and con =
    (* types *)
    Type_arrow of con * con
    (* this acts like Type_product, but reserved for fixed points,
    * all symbols need to be globally unique for each fixed point *)
  | Type_productfix of (Symbols.t * con) list
  | Type_forall of kind * con
  | Type_exists of kind * con
  | Type_product of con list
  | Type_sum of con list
  | Type_rec of con
  | Type_exn

    (* type constructors *)
  | Con_var of int
  | Con_lam of kind * con
  | Con_app of con * con
  | Con_pair of con * con
  | Con_proj1 of con
  | Con_proj2 of con
  | Con_unit

  and term =
    Term_var of var
  | Term_let of term * var * term
  | Term_fixlam of (Symbols.t * var * var * con * term * con) list
  | Term_pick of term * Symbols.t
  | Term_app of term * term
  | Term_polylam of kind * term
  | Term_polyapp of term * con
  | Term_pack of con * term * con
  | Term_unpack of term * var * term
  | Term_tuple of term list
  | Term_proj of term * int
  | Term_inj of con * int * term
  | Term_case of term * (var * term) list
  | Term_fold of con * term
  | Term_unfold of term
  | Term_ext of module

  and sg =
    Sg_unit
  | Sg_kind of kind
  | Sg_type of con
  | Sg_lam of sg * sg
  | Sg_pair of sg * sg

  and module =
    Module_var of var
  | Module_unit
  | Module_con of con
  | Module_term of term
  | Module_lam of var * sg * module (* applicative *)
  | Module_app of module * module (* applicative *)
  | Module_pair of module * var * module
  | Module_tuple of module * module
  | Module_proj1 of module
  | Module_proj2 of module
  | Module_let of term * var * module

  (* [Crary 2020, Figure 2] *)
  fun fstSg sg =
    case sg of
      Sg_unit => Kind_unit
    | Sg_kind k => k
    | Sg_type _ => Kind_unit
    | Sg_lam (s, s') => Kind_pi (fstSg s, fstSg s')
    | Sg_pair (s, s') => Kind_sigma (fstSg s, fstSg s')
end
