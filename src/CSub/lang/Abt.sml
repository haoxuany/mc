
structure Abt : ABT = struct
  type sym = Symbols.t

  datatype macro =
    Macro_include of string

  datatype ctype =
    CType_sym of sym
  | CType_void
  | CType_ptr of ctype
  | CType_fn of ctype * ctype list

  datatype state =
    State_exp of exp
  | State_decl of ctype * sym
  | State_return of exp option
  | State_switch of exp * (exp * state list) list * (state list option)

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
  | Decl_fnty of ctype * sym * ctype list

  datatype cfile =
    CFile of {
      macros: macro list,
      decls: decl list
    }
end
