
signature ABT = sig
  structure Variable : VARIABLE

  type var = Variable.t

  datatype kind =
    Kind_type
  | Kind_pi of kind * kind
  | Kind_sigma of kind * kind
  | Kind_singleton of con

  and con =
    (* types *)
    Type_arrow of con * con
  | Type_forall of kind * con
  | Type_exists of kind * con
  | Type_product of con list
  | Type_sum of con list
  | Type_exn

    (* type constructors *)
  | Con_var of int
  | Con_lam of kind * con
  | Con_app of con * con
  | Con_pair of con * con
  | Con_proj1 of con
  | Con_proj2 of con

  and term =
    Term_var of var
  | Term_let of term * var * term
  | Term_fix of var * con * term
  | Term_lam of var * con * term
  | Term_app of term * term
  | Term_polylam of kind * term
  | Term_polyapp of term * con
  | Term_pack of con * term * con
  | Term_unpack of term * var * term
  | Term_tuple of term list
  | Term_proj of term * int
  | Term_inj of con * int * term
  | Term_case of term * (var * term) list
end
