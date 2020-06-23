
functor CodegenFun(
  structure DebugTranslation : DEBUGTRANSLATION
) = struct
  open CodegenConstructors

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
        val decl = declmalloc sym (ptr cword) (List.length vs)

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
      in (decl :: (writeEach vs 0), sym) end

    | S.Value_inj (cases, i, v) => let
        val sym = Symbols.fresh "inj"
        val temp = T.Exp_sym sym
        val decl = declmalloc sym (ptr cword) 2

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
      in (s @ (decl :: writeHead :: writeBody :: nil), sym) end

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

        val (sv, vsym) = translateValue vmap v

        val decl = T.State_decl (ptr cword, sym,
          SOME (T.Exp_index (
            T.Exp_sym vsym,
            T.Exp_int i
          ))
        )

        val rest = translateExp (insert vmap x sym) e
      in (sv @ (decl :: rest)) end

    | S.Exp_case (v, cases) => let
        val sym = Symbols.fresh "cased"
        val temp = T.Exp_sym sym

        val decl = T.State_decl (cword, sym, NONE)

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
      in sv @ [decl, switch] end

    | S.Exp_let (v, x, e) => let
        val sym = Symbols.fresh "letbound"
        val temp = T.Exp_sym sym

        val (sv, vsym) = translateValue vmap v

        val decl = T.State_decl (ptr cword, sym, SOME (T.Exp_sym vsym))

        val rest = translateExp (insert vmap x sym) e
      in (sv @ (decl :: rest)) end

    | S.Exp_exit i => [T.State_exp (
        T.Exp_call (T.Exp_sym (Symbols.raw "exit"), [T.Exp_int i])
      )]

  in result end

  fun translateBlock vmap block = let
    val result = case block of
      S.Block_fixlam lams => let
        val decls = ParList.map
          (fn (s, _, args, _) =>
            T.Decl_fnty (cvoid, s, ParList.map (fn _ => ptr cword) args))
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
              cvoid,
              s,
              ParList.map (fn (_, sym) => (ptr cword, sym)) bnds,
              e
          ) end)
          lams
      in (nullsym, decls, lams) end

    | S.Block_lam (bnds, e) => let
        val sym = Symbols.fresh "fn"
        val bnds = ParList.map (fn x => (x, Symbols.fresh "arg")) bnds
        val vmap = List.foldr
          (fn ((x, sym), vmap) => insert vmap x sym)
          vmap bnds

        val e = translateExp vmap e
      in (sym, nil, [T.Decl_fn (
            cvoid,
            sym,
            ParList.map (fn (_, sym) => (ptr cword, sym)) bnds,
            e
        )]) end
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
            nil => (nil, [[
              T.Decl_fn (
                cvoid, Symbols.raw "main",
                nil,
                translateExp vmap e
              )
            ]])
         | (x, b) :: rest => let
             val (s, decls, defns) = translateBlock vmap b
             val vmap = insert vmap x s
             val (decls', defns') = trans vmap rest
           in (decls :: decls', defns :: defns') end

        val (decls, defns) = trans VarMap.empty bnds
        val decls = List.concat (decls @ defns)

      in T.CFile {
            macros = macros,
            decls = decls
         }
      end

  in result end
end
