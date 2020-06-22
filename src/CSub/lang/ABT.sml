
signature ABT = sig
  type sym = Symbols.t

  datatype macro =
    Macro_include of string

  datatype ctype =
    CType_sym of sym
  | CType_void
  | CType_ptr of ctype

  datatype state =
    State_exp of exp
  | State_decl of ctype * sym
  | State_return of exp option

  and exp =
    Exp_sym of sym
  | Exp_int of int
  | Exp_string of string
  | Exp_assign of exp * exp
  | Exp_cast of exp * ctype
  | Exp_deref of exp
  | Exp_addr of exp
  | Exp_index of exp * exp
  | Exp_call of exp * exp list

  and decl =
    Decl_fn of ctype * sym * (ctype * sym) list * state list

  datatype cfile =
    CFile of {
      macros: macro list,
      decls: decl list
    }
end
