
structure Examples = struct
  open CSub

  open Abt
  open Symbols
  open Print

  val program = let
    val macros = [
      Macro_include "stdio.h",
      Macro_include "stdlib.h"
    ]
    val cint = CType_sym (raw "int")
    val cchar = CType_sym (raw "char")
    val cstring = CType_ptr cchar
    val i = fresh "i";
    val s = fresh "s";
    val decls = [
      Decl_fn (
        cint, raw "main",
        [ (cint, raw "argc"),
          (cstring , raw "argv")
        ],
        [ State_decl (cint, i,
            SOME (Exp_int 10)),
          State_decl (cstring, s, NONE),
          State_exp (
            Exp_assign (
              Exp_sym s,
              Exp_string "Hello, World!\\n"
            )
          ),
          State_exp (
            Exp_call (
              Exp_sym (raw "printf"),
              [Exp_sym s]
            )
          ),
          State_return (SOME (Exp_int 0))
        ]
      )
    ]
  in CFile {
    macros = macros,
    decls = decls
  } end

  val () = printCFile (TextIO.openOut "example.c") program
end
