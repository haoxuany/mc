
functor CodegenFun(
  structure DebugTranslation : DEBUGTRANSLATION
) = struct
  structure SLang = Low
  structure TLang = CSub
  structure S = SLang.Abt
  structure T = TLang.Abt

  structure DebugTranslation = DebugTranslation
  open DebugTranslation

  structure VarMap = SplayRDict(
    structure Key = S.Variable
  )
  open VarMap

  exception TypeError

  val cword = T.CType_sym (Symbols.raw "size_t")
  fun cmalloc i =
    T.Exp_cast (
      T.Exp_call (
        T.Exp_sym (Symbols.raw "malloc"),
        [T.Exp_int (i * 8)]
      ),
      T.CType_ptr cword
    )

  fun translateValue vmap value = let
    val result = case value of
      S.Value_var x => (nil,
        case VarMap.find vmap x of
           NONE => raise TypeError
         | SOME s => s)

    | S.Value_pick (_, s) => (nil, s)

    | S.Value_tuple vs => let
        val sym = Symbols.fresh "tuple"
        val temp = T.Exp_sym sym
        val decl = T.State_decl (T.CType_ptr cword, sym)
        val alloc = T.State_exp (
          T.Exp_assign (
            temp,
            cmalloc (List.length vs)
          )
        )

        fun writeEach vs i =
          case vs of
            nil => nil
          | v :: rest => let
              val (s, e) = translateValue vmap v
              val rest = writeEach rest (i + 1)
              val write = T.State_exp (
                T.Exp_assign (
                  T.Exp_index (
                    temp,
                    T.Exp_int i
                  ),
                  T.Exp_sym e
                )
              )
            in s @ (write :: rest) end
      in (decl :: alloc :: (writeEach vs 0), sym) end

    | S.Value_inj (cases, i, v) => let
        val sym = Symbols.fresh "inj"
        val temp = T.Exp_sym sym

        val decl = T.State_decl (T.CType_ptr cword, sym)
        val alloc = T.State_exp (
          T.Exp_assign (
            temp,
            cmalloc 2
          )
        )

        val writeHead = T.State_exp (
          T.Exp_assign (
            T.Exp_index (
              temp,
              T.Exp_int 0
            ),
            T.Exp_int i
          )
        )

        val (s, e) = translateValue vmap v
        val writeBody = T.State_exp (
          T.Exp_assign (
            T.Exp_index (
              temp,
              T.Exp_int 1
            ),
            T.Exp_sym e
          )
        )
      in (s @ (decl :: alloc :: writeHead :: writeBody :: nil), sym) end

  in result end

  and translateExp vmap exp = let
    val result = case exp of
      S.Exp_app (v, vs) => let
        val (sv, vsym) = translateValue vmap v
        val (svs, vssym) = ListPair.unzip (
          ParList.map (translateValue vmap) vs
        )
        val call = T.State_exp (
          T.Exp_call (
            T.Exp_cast (
              T.Exp_sym vsym,
              T.CType_fn (
                T.CType_void,
                ParList.map (fn _ => T.CType_ptr cword) vs
              )
            ),
            ParList.map T.Exp_sym vssym
          )
        )
      in List.concat (sv :: (svs @ [[call]])) end

    | S.Exp_proj (v, i, x, e) => let
        val sym = Symbols.fresh "projected"
        val temp = T.Exp_sym sym

        val decl = T.State_decl (T.CType_ptr cword, sym)

        val (sv, vsym) = translateValue vmap v
        val write = T.State_exp (
          T.Exp_assign (
            temp,
            T.Exp_index (
              T.Exp_sym vsym,
              T.Exp_int i
            )
          )
        )

        val rest = translateExp (insert vmap x sym) e
      in (sv @ (decl :: write :: rest)) end

    | S.Exp_case (v, cases) => let
        val sym = Symbols.fresh "cased"
        val temp = T.Exp_sym sym

        val decl = T.State_decl (T.CType_ptr cword, sym)

        val (sv, vsym) = translateValue vmap v

        fun constructCases cases i =
          case cases of
            nil => nil
          | (x, e) :: rest => let
              val write = T.State_exp (
                T.Exp_assign (
                  temp,
                  T.Exp_index (
                    T.Exp_sym vsym,
                    T.Exp_int 1
                  )
                )
              )
              val s = translateExp (insert vmap x sym) e
              val terminate = T.State_return NONE
            in (T.Exp_int i, write :: (s @ [terminate]))
               :: (constructCases rest (i + 1)) end

        val switch =
          T.State_switch (
            T.Exp_index (
              T.Exp_sym vsym,
              T.Exp_int 0
            ),
            constructCases cases 0,
            NONE
          )
      in sv @ [switch] end

    | S.Exp_let (v, x, e) => let
        val sym = Symbols.fresh "letbound"
        val temp = T.Exp_sym sym

        val decl = T.State_decl (T.CType_ptr cword, sym)

        val (sv, vsym) = translateValue vmap v
        val write = T.State_exp (
          T.Exp_assign (
            temp,
            T.Exp_sym vsym
          )
        )

        val rest = translateExp (insert vmap x sym) e
      in (sv @ (decl :: write :: rest)) end

    | S.Exp_exit i => [T.State_exp (
        T.Exp_call (T.Exp_sym (Symbols.raw "exit"), [T.Exp_int i])
      )]

  in result end

  fun translateBlock vmap sym block = let
    val result = case block of
      S.Block_fixlam lams => let
        val decls = ParList.map
          (fn (s, _, args, _) =>
            T.Decl_fnty (T.CType_void, s,
              ParList.map (fn _ => T.CType_ptr cword) args))
          lams

        val vmap = List.foldl
          (fn ((s, f, _, _), vmap) => insert vmap f s)
          vmap lams

        val lams = ParList.map
          (fn (s, _, bnds, e) => let
            val bnds = ParList.map (fn x => (x, Symbols.fresh "arg")) bnds
            val vmap = List.foldr
              (fn ((x, sym), vmap) => insert vmap x sym)
              vmap bnds

            val e = translateExp vmap e
          in T.Decl_fn (
              T.CType_void,
              s,
              ParList.map (fn (_, sym) => (T.CType_ptr cword, sym)) bnds,
              e
          ) end)
          lams
      in decls @ lams end

    | S.Block_lam (bnds, e) => let
        val bnds = ParList.map (fn x => (x, Symbols.fresh "arg")) bnds
        val vmap = List.foldr
          (fn ((x, sym), vmap) => insert vmap x sym)
          vmap bnds

        val e = translateExp vmap e
      in [T.Decl_fn (
            T.CType_void,
            sym,
            ParList.map (fn (_, sym) => (T.CType_ptr cword, sym)) bnds,
            e
        )] end
  in result end

  and translateProgram program = let
    val result = case program of
      S.Program (bnds, e) => let
        val macros = [
          T.Macro_include "stdio.h",
          T.Macro_include "stdlib.h"
        ]

        fun trans vmap bnds =
          case bnds of
            nil => [[
              T.Decl_fn (
                T.CType_void, Symbols.raw "main",
                nil,
                translateExp vmap e
              )
            ]]
         | (x, b) :: rest => let
             val f = Symbols.fresh "fn"
             val decls = translateBlock vmap f b
             val vmap = insert vmap x f
           in decls :: (trans vmap rest) end

        val decls = List.concat (trans VarMap.empty bnds)

      in T.CFile {
            macros = macros,
            decls = decls
         }
      end

  in result end
end
