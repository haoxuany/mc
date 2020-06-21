(* Single Threaded Reference as variable. Can only be used in one thread. *)
structure RefVariable :> VARIABLE = struct
  type t = int

  val count = ref 0

  fun new () = let
    val result = !count
    val () = count := !count + 1
  in result end

  fun eq (a, b) = (a : int) = b
  val compare = Int.compare

  fun print i = "#" ^ (Int.toString i)
end
