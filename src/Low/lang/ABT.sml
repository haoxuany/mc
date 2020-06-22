
signature ABT = sig
  structure Variable : VARIABLE

  type var = Variable.t

  datatype value =
    Value_var of var
  | Value_pick of value * Symbols.t
  | Value_tuple of value list
  | Value_inj of int (* type cases *) * int * value

  and exp =
    Exp_app of value * value list
  | Exp_proj of value * int * var * exp
  | Exp_case of value * (var * exp) list
  | Exp_let of value * var * exp
  | Exp_exit of int

  and block =
    Block_fixlam of (Symbols.t * var * var list * exp) list
  | Block_lam of var list * exp

  and program =
    Program of (var * block) list * exp
end
