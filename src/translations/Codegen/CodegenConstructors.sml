
structure CodegenConstructors = struct
  local
  open CSub
  open Abt
  in

  val cvoid = CType_void
  val ptr = CType_ptr
  val cvoidstar = ptr cvoid
  val nullsym = Symbols.raw "NULL"
  val null = Exp_sym nullsym
  val cword = CType_sym (Symbols.raw "size_t")

  val mallocsym = Symbols.raw "malloc"
  fun declmalloc sym ty i =
    State_decl (ty, sym,
      if i = 0 then NONE
      else SOME (
        Exp_cast (
          Exp_call (
            Exp_sym mallocsym,
            [Exp_int (i * 8)]
          ),
          ptr cvoid
        )
      )
    )
  end
end
