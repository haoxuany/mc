
signature ABT = sig
  structure Variable : VARIABLE

  type var = Variable.t

  datatype kind =
    Kind_type
  | Kind_pi of kind * kind
  | Kind_sigma of kind * kind
  | Kind_singleton of con
  | Kind_unit

  and con =
    (* types *)
    Type_not of con list
  | Type_productfix of (Symbols.t * con) list
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

  and value =
    Value_var of var
  | Value_fixlam of (Symbols.t * var * (var * con) list * exp) list
  | Value_pick of value * Symbols.t
  | Value_lam of (var * con) list * exp
  | Value_pack of con * value * con
  | Value_tuple of value list
  | Value_inj of con * int * value
  | Value_fold of con * value

  and exp =
    Exp_app of value * value list
  | Exp_unpack of value * var * exp
  | Exp_proj of value * int * var * exp
  | Exp_case of value * (var * exp) list
  | Exp_unfold of value * var * exp
  | Exp_let of value * var * exp
  | Exp_exit of int
end
