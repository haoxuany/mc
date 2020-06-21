
(* A name caching table the ensures global uniqueness of symbols *)
functor SymbolFun(
) :> SYMBOL = struct
  structure Cache = HashTable (
    structure Key = StringHashable
  )

  val cache = (Cache.table 90) : int Cache.table

  type t = string * word

  fun permissible c = Char.isAlpha c orelse c = #"_"

  fun fresh suggest = let
    val suggest = String.implode
      (List.filter permissible (String.explode suggest))

    val suggest = if String.size suggest = 0 then "fn" else suggest

    fun append s i = String.concat [s, "_", Int.toString i]

    val name = case Cache.find cache suggest of
      NONE => (Cache.insert cache suggest 0; append suggest 0)
    | SOME x => let val x = x + 1 in
        Cache.insert cache suggest x; append suggest x end
  in (name, StringHashable.hash name) end

  fun compare ((_, a), (_, b)) = Word.compare (a, b)
  fun eq (a, b) = compare (a, b) = EQUAL

  fun name (s, _) = s
end
